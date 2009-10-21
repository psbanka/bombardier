"pexpect machine interface module"
#!/usr/bin/env python

import pxssh, pexpect
import sys, time, os, re
import StringIO
from bombardier_core.static_data import OK, FAIL, SERVER, TRACEBACK, INFO
from ServerLogger import ServerLogger
from Exceptions import JobAlreadySet, EnableRequiredException
from Exceptions import IncompleteConfigurationException
from Exceptions import MachineUnavailableException
from Exceptions import SecureCopyException

#Statuses:
DISCONNECTED = 0
CONNECTED    = 1
BROKEN       = 2
DOT_LENGTH = 20
BAD_BREAK_CHARS = ['[', '\033', 'm', ';', '0', '3', '1']
GOOD_BREAK_CHARS = ['.', '-', ' ', '\\', '/', '=', ')', ']', '_']
CONNECTION_TIMEOUT = 90 * 3600 #90 min
SSH_NEW_KEY = 'Are you sure you want to continue connecting'

class MachineInterface:
    "Interface to a remote machine via pxssh"
    def __init__(self, machine_config, server_log):
        self.server_log    = server_log
        self.polling_log   = None
        self.job_name      = None
        self.output_handle = sys.stdout
        self.status        = DISCONNECTED
        self.connect_time  = 0
        self.ssh_pass      = ''
        self.ssh_conn      = None
        self.machine_name     = None
        self.server_home   = None
        self.data          = {}
        self.user_name     = None
        self.set_data(machine_config)

    def set_data(self, machine_config):
        "Set up MachineInterface data"
        self.machine_name     = machine_config.machine_name
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

    def set_job(self, job_name):
        if self.job_name:
            raise JobAlreadySet(self.job_name)
        self.job_name = job_name
        self.polling_log = ServerLogger(job_name)
        self.polling_log.add_stringio_handle()
        msg = "Binding interface %s to job %s" % (self.machine_name, job_name)
        self.server_log.info(msg, self.machine_name)

    def unset_job(self):
        self.job_name = None
        self.polling_log = None
        msg = "UN-Binding interface %s to job %s"
        msg = msg % (self.machine_name, self.job_name)
        self.server_log.info(msg, self.machine_name)

    def get_new_logs(self):
        if self.polling_log:
            return self.polling_log.get_new_logs()
        return []

    def traceback_output(self, source, msg):
        self.log(source, TRACEBACK, msg)

    def from_output(self, msg, severity=INFO):
        self.log(self.machine_name, severity, msg)

    def log(self, source, severity, msg):
        formatted_msg = "<<%s|%d|%s>>\n" % (source, severity, msg)
        self.output_handle.write(formatted_msg)
        self.output_handle.flush()

    def terminate(self):
        result = self.ssh_conn.terminate(force=True)
        if result:
            self.status = DISCONNECTED
            return OK
        return FAIL

    def connect(self):
        self.ssh_conn = pxssh.pxssh()
        self.ssh_conn.timeout = 6000
        msg = "Connecting to %s..." % self.machine_name
        self.polling_log.debug(msg)
        self.server_log.debug(msg, self.machine_name)
        try:
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

    def freshen(self):
        connection_age = time.time() - self.connect_time
        if self.status == DISCONNECTED or \
           connection_age > CONNECTION_TIMEOUT or \
           self.status == BROKEN:
            if self.status == CONNECTED:
                msg = "Assuming our connection to %s is stale after "\
                      "%4.2f minutes. Reconnecting..."
                msg = msg % (self.machine_name, connection_age / 60.0)
                self.polling_log.warning(msg)
                self.disconnect()
            if self.connect() != OK:
                self.polling_log.warning("A")
                return FAIL
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
                self.disconnect()
            except:
                pass
            if self.connect() != OK:
                self.polling_log.warning("B")
                return FAIL
        return OK

    def process_scp(self, scp_conn):
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
        self.polling_log.info( "Getting %s" % dest_file)
        cmd = 'scp -v %s@%s:%s .'
        cmd = cmd % (self.username, self.ip_address, dest_file)
        self.polling_log.debug("EXECUTING: %s" % cmd, cmd)
        scp_conn = pexpect.spawn(cmd, timeout=30)
        return self.process_scp(scp_conn)

    def scp(self, source, dest, verbose=True):
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

    # BEING USED
    def scp_dict(self, copy_dict):
        "Use scp with a dictionary to copy files grouped by file type"
        self.connect()
        for file_type in copy_dict:
            if file_type == "dist" or file_type == 'admin':
                dest_dir = '.'
            else:
                dest_dir = os.path.join(self.spkg_dir, 
                                        self.machine_name, file_type)
            for source_file in copy_dict[file_type]:
                source_path = os.path.join(self.server_home, file_type,
                                           source_file)
                msg = "SOURCE_PATH: %s // DEST_DIR: %s"
                msg =  msg % (source_path, dest_dir)
                self.polling_log.info(msg)
                status = self.scp(source_path, dest_dir)
        return
    
    def chdir(self, path):
        "Change the current directory on a remote session"
        if not path:
            path = self.spkg_dir
        self.polling_log.debug("Changing directory to %s" % path)
        self.ssh_conn.sendline ('cd %s' % path)
        self.ssh_conn.prompt()

    def log_raw_data(self, output_queue):
        "Transfer full lines from queue into the polling log"
        while '\n' in output_queue:
            position = len(output_queue.split('\n')[0])
            self.polling_log.info(output_queue[:position-1])
            output_queue = output_queue[position+2:]
        return output_queue

    def gso(self, cmd, raise_on_error=True, cmd_debug=False):
        "Run a remote shell command"
        if cmd_debug:
            msg = "* RUNNING: %s" % cmd
            self.polling_log.debug(msg)
            self.server_log.debug(msg, self.machine_name)
        try:
            self.ssh_conn.sendline( cmd )
            self.ssh_conn.prompt()
        except EOF:
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
        if self.freshen() != OK:
            msg = "Unable to connect to %s." % self.machine_name
            raise MachineUnavailableException(self.machine_name, msg)
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

    def dump_trace(self):
        "Pretty print a stack trace into the logs"
        stack_trace = []
        found_index = 1
        while found_index == 1:
            stack_trace.append(self.ssh_conn.match.groups()[0])
            expect_list = [self.ssh_conn.PROMPT, self.trace_matcher,
                           self.log_matcher]
            found_index = self.ssh_conn.expect(expect_list, timeout=6000)
        t_string = ''.join(stack_trace)

        noop_re = "NoOptionError\: No option \'(\w+)\' in section\: \'(\w+)\'"
        re_obj = re.compile(noop_re)
        data = re_obj.findall(t_string)
        invalid_data_msg = "Invalid client configuration data"
        if data:
            self.polling_log.error(invalid_data_msg)
            self.server_log.error(invalid_data_msg, self.machine_name)
            if len(data) == 2:
                need_msg = "Need option '%s' in section '%s'." % \
                            (data[0], data[1])
            else:
                need_msg = "Need options: %s" % data
            self.polling_log.info(need_msg)
            self.server_log.error(need_msg, self.machine_name)
        no_section_re = re.compile("NoSectionError\: No section\: \'(\w+)\'")
        data = no_section_re.findall(t_string)
        if data:
            self.polling_log.error(invalid_data_msg)
            self.server_log.error(invalid_data_msg, self.machine_name)
            need_msg = "Need section '%s'." % (data[0])
            self.polling_log.info(need_msg)
            self.server_log.error(need_msg, self.machine_name)
        else:
            for line in stack_trace:
                self.polling_log.error(line)
                self.server_log.error(line, self.machine_name)

    def check_result(self):
        self.ssh_conn.setecho(False)
        self.ssh_conn.sendline("echo $?")
        self.ssh_conn.prompt()
        return_code = 0
        try:
            return_code = int(str(self.ssh_conn.before.split()[0].strip()))
        except Exception:
            return FAIL
        return return_code

    def check_possible_paths(self, test_path):
        while len(test_path):
            self.ssh_conn.sendline("ls --color=never -Fd1 %s*" %test_path)
            self.ssh_conn.prompt()
            result_list = self.ssh_conn.before.split()[:-1]
            if self.check_result() != OK:
                test_path = test_path[:test_path.rfind('/')]
                continue
            output = [ result.replace('*', '') for result in result_list ]
            if test_path.endswith('/'):
                output.append(test_path)
            return output
        return [test_path]

    def disconnect(self):
        self.connect_time = 0
        try:
            self.ssh_conn.logout()
        finally:
            self.status = DISCONNECTED


