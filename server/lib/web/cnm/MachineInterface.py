#!/usr/bin/env python
"MachineInterface: pexpect machine interface module"

import pxssh, pexpect
import time, os, re
from bombardier_core.static_data import OK, FAIL
from Exceptions import EnableRequiredException
from Exceptions import IncompleteConfigurationException
from Exceptions import MachineUnavailableException
from Exceptions import SecureCopyException
from AbstractMachineInterface import AbstractMachineInterface
from AbstractMachineInterface import DISCONNECTED, CONNECTED, BROKEN

#Statuses:
DOT_LENGTH = 20
BAD_BREAK_CHARS = ['[', '\033', 'm', ';', '0', '3', '1']
GOOD_BREAK_CHARS = ['.', '-', ' ', '\\', '/', '=', ')', ']', '_']
CONNECTION_TIMEOUT = 90 * 3600 #90 min
SSH_NEW_KEY = 'Are you sure you want to continue connecting'

class MachineInterface(AbstractMachineInterface):
    "Interface to a remote machine via pxssh"
    def __init__(self, machine_config, server_log):
        AbstractMachineInterface.__init__(self, machine_config, server_log)
        self.ssh_pass    = ''
        self.spkg_dir    = None # FIXME
        self.username    = None
        self.ip_address  = None
        self.platform    = None
        self.report_info = {}
        self.ssh_conn    = None
        self.set_data(machine_config)

    def set_data(self, machine_config):
        "Set up MachineInterface data"
        self.machine_name  = machine_config.machine_name
        self.server_home   = machine_config.server_home
        self.data          = machine_config.data
        self.username      = self.data.get("default_user", None)
        if self.username == None:
            if self.data.get("enc_username"):
                raise EnableRequiredException()
            msg = "'default_user' is not defined"
            raise IncompleteConfigurationException(self.machine_name, msg)
        self.ip_address    = self.data.get("ip_address", None)
        if self.ip_address == None:
            if self.data.get("enc_ip_address"):
                raise EnableRequiredException()
            msg = "'ip_address' is not defined"
            raise IncompleteConfigurationException(self.machine_name, msg)
        self.platform     = self.data.get("platform", None)
        if self.platform == None:
            if self.data.get("enc_platform"):
                raise EnableRequiredException()
            msg = "'platform' is not defined"
            raise IncompleteConfigurationException(self.machine_name, msg)

    def terminate(self):
        "Harshly disconnect from the remote system"
        status = OK
        if self.ssh_conn:
            result = self.ssh_conn.terminate(force=True)
            if result:
                self.status = DISCONNECTED
            else:
                status = FAIL
        self.unset_job()
        return status

    def connect(self):
        "Make a new connection to the remote system"
        self.ssh_conn = pxssh.pxssh()
        self.ssh_conn.timeout = 6000
        msg = "Connecting to %s..." % self.machine_name
        self.polling_log.debug(msg)
        #self.server_log.debug(msg, self.machine_name)
        try:
            self.server_log.debug(msg, self.machine_name)
            login_okay = self.ssh_conn.login(self.ip_address, self.username,
                                             self.ssh_pass, login_timeout=6000)
            if not login_okay:
                msg = "Could not connect."
                raise MachineUnavailableException(self.machine_name, msg)
            self.ssh_conn.sendline('stty -echo')
            self.ssh_conn.prompt()
        except (MachineUnavailableException, pexpect.TIMEOUT):
            msg = "SSH session failed on login."
            self.polling_log.error(msg)
            self.server_log.error(msg, self.machine_name)
            self.status = BROKEN
            return FAIL
        self.status = CONNECTED
        self.connect_time = time.time()
        return OK

    def control_c(self):
        #self.ssh_conn.sendcontrol('c')
        pass

    def freshen(self):
        '''Since we like to keep connections around for a long time,
        verify that our connection to the remote system is still working'''
        connection_age = time.time() - self.connect_time
        if self.status == DISCONNECTED or \
            connection_age > CONNECTION_TIMEOUT or \
            self.status == BROKEN:
            if self.status == CONNECTED:
                msg = "Assuming our connection to %s is stale after "\
                      "%4.2f minutes. Reconnecting..."
                msg = msg % (self.machine_name, connection_age / 60.0)
                self.polling_log.warning(msg)
                self.terminate()
            if self.connect() != OK:
                msg = "Unable to connect to %s." % self.machine_name
                raise MachineUnavailableException(self.machine_name, msg)

        dead = False
        try:
            self.ssh_conn.sendline('echo hello')
            if not self.ssh_conn.prompt(timeout = 5):
                dead = True
        except:
            dead = True

        if dead:
            msg = "Our connection handle is dead. Reconnecting..."
            self.polling_log.warning(msg)
            self.server_log.warning(msg, self.machine_name)
            try:
                self.terminate()
            except:
                pass
            if self.connect() != OK:
                msg = "Unable to connect to %s." % self.machine_name
                raise MachineUnavailableException(self.machine_name, msg)

    def _process_scp(self, scp_conn):
        "After we've started an scp, drive it until finished"
        expect_list = [pexpect.TIMEOUT, SSH_NEW_KEY,
                       '[pP]assword: ', 'Exit status']
        select_index = scp_conn.expect(expect_list, timeout=50)
        if select_index == 0:
            msg = scp_conn.before+'|'+scp_conn.after
            raise MachineUnavailableException(self.machine_name, msg)
        if select_index == 1:
            scp_conn.sendline('yes')
            scp_conn.expect('[pP]assword: ', timeout=30)
            select_index = scp_conn.expect([pexpect.TIMEOUT,
                                           '[pP]assword: '], timeout=50)
            if select_index == 0:
                msg = scp_conn.before+'|'+scp_conn.after
                raise MachineUnavailableException(self.machine_name, msg)
            scp_conn.sendline(self.ssh_pass)
        if select_index == 2:
            scp_conn.sendline(self.ssh_pass)
        if select_index == 3:
            pass
        scp_conn.expect(pexpect.EOF)
        scp_conn.close()
        return OK

    def get(self, dest_file):
        "secure copy from a remote host"
        self.polling_log.info( "Getting %s" % dest_file)
        cmd = 'scp -v %s@%s:%s .'
        cmd = cmd % (self.username, self.ip_address, dest_file)
        self.polling_log.debug("EXECUTING: %s" % cmd, cmd)
        scp_conn = pexpect.spawn(cmd, timeout=30)
        return self._process_scp(scp_conn)

    def scp(self, source, dest, verbose=True):
        "secure copy to a remote host"
        if not os.path.isfile(source):
            msg = "Attempting to send nonexistant file: %s" % source
            self.server_log.error(msg)
            self.polling_log.error(msg, self.machine_name)
            raise SecureCopyException(source, dest, msg)
        if verbose:
            msg = "Sending %s to %s:%s" % (source, self.machine_name, dest)
            self.polling_log.info(msg)
            self.server_log.info(msg, self.machine_name)
        else:
            msg = "Sending %s..." % (source)
            self.polling_log.debug(msg)
            self.server_log.debug(msg, self.machine_name)
        cmd = 'scp -v %s %s@%s:%s'
        cmd = cmd % (source, self.username, self.ip_address, dest)
        try:
            scp_conn = pexpect.spawn(cmd, timeout=600)
            select_index = scp_conn.expect([pexpect.TIMEOUT, SSH_NEW_KEY,
                                           '[pP]assword: ', 'Exit status'],
                                           timeout=600)
        except pexpect.EOF:
            msg = "Connection refused."
            raise MachineUnavailableException(dest, msg)
        if select_index == 0:
            msg = scp_conn.before+'|'+scp_conn.after
            raise MachineUnavailableException(dest, msg)
        if select_index == 1:
            scp_conn.sendline('yes')
            scp_conn.expect('[pP]assword: ', timeout=30)
            select_index = scp_conn.expect([pexpect.TIMEOUT,
                                           '[pP]assword: '], timeout=50)
            if select_index == 0:
                if type(scp_conn.before) == type("string") and \
                   type(scp_conn.after) == type("string"):
                    msg = scp_conn.before+'|'+scp_conn.after
                else:
                    msg = "before: (%s) after: (%s)"
                    msg = msg % (scp_conn.before, scp_conn.after)
                raise MachineUnavailableException(dest, msg)
            scp_conn.sendline(self.ssh_pass)
        if select_index == 2:
            self.polling_log.warning('Using password authentication')
            scp_conn.sendline(self.ssh_pass)
        if select_index == 3:
            pass
        scp_conn.expect(pexpect.EOF)
        scp_conn.close()
        return OK

    def scp_dict(self, copy_dict):
        "Use scp with a dictionary to copy files grouped by file type"
        self.connect()
        for file_desc in copy_dict:
            src, dst = copy_dict[file_desc]
            if not src.startswith('/'):
                src = os.path.join(self.server_home, src)
            msg = "SOURCE_PATH: %s // DEST_DIR: %s"
            msg =  msg % (src, dst)
            self.polling_log.debug(msg)
            status = self.scp(src, dst)

    def chdir(self, path):
        "Change the current directory on a remote session"
        if not path:
            path = self.spkg_dir
        self.polling_log.debug("Changing directory to %s" % path)
        self.ssh_conn.sendline ('cd %s' % path)
        self.ssh_conn.prompt()

    def gso(self, cmd, raise_on_error=True, cmd_debug=False):
        "Run a remote shell command"
        if cmd_debug:
            msg = "* RUNNING: %s" % cmd
            self.polling_log.debug(msg)
            self.server_log.debug(msg, self.machine_name)
        try:
            self.ssh_conn.sendline( cmd )
            self.ssh_conn.prompt()
        except pexpect.EOF:
            if raise_on_error:
                msg = "Error running %s" % (cmd)
                raise MachineUnavailableException(self.machine_name, msg)
            else:
                return ""
        output = self.ssh_conn.before.strip()
        if cmd_debug:
            self.polling_log.info("* OUTPUT: %s" % output)
        return output

    def run_cmd(self, command_string):
        "Run a remote shell command"
        self.report_info = ''
        #self.freshen(False) # Not sure why this was needed, but removing.
        return_code = OK
        self.ssh_conn.sendline(command_string)
        command_complete = False
        total_output = ''
        output_queue = ''
        output_checker = re.compile('(.*)'+self.ssh_conn.PROMPT)
        while True:
            output = self.ssh_conn.read_nonblocking()
            output_queue += output
            total_output += output
            output_queue = self.log_raw_data(output_queue)
            if output_checker.findall(total_output):
                #self.polling_log.info("EXITING")
                break
        #self.polling_log.info(output_queue)
        self.ssh_conn.setecho(False)
        return [OK, []]

    def check_result(self):
        "Examine the output of a command"
        self.ssh_conn.setecho(False)
        self.ssh_conn.sendline("echo $?")
        self.ssh_conn.prompt()
        return_code = 0
        try:
            return_code = int(str(self.ssh_conn.before.split()[0].strip()))
        except Exception:
            return FAIL
        return return_code

    def disconnect(self):
        self.connect_time = 0
        try:
            #self.ssh_conn.logout()
            self.ssh_conn.terminate(force=True)
        finally:
            self.status = DISCONNECTED


