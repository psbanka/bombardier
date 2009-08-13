#!/usr/bin/env python

import pxssh, pexpect
import sys, time
from bombardier_core.static_data import OK, FAIL, SERVER, TRACEBACK
from bombardier_core.static_data import DEBUG, INFO, WARNING, ERROR, CRITICAL



#Statuses:
DISCONNECTED = 0
CONNECTED    = 1
BROKEN       = 2
DOT_LENGTH = 20
BAD_BREAK_CHARS = ['[', '\033', 'm', ';', '0', '3', '1']
GOOD_BREAK_CHARS = ['.', '-', ' ', '\\', '/', '=', ')', ']', '_']
CONNECTION_TIMEOUT = 90 * 3600 #90 min
SSH_NEW_KEY = 'Are you sure you want to continue connecting'

class EnableRequiredException(Exception):
    def __init__(self):
        Exception.__init__(self)
    def __repr__(self):
        return "Must be in enable mode to connect to this server"
    def __str__(self):
        return self.__repr__()

class IncompleteConfigurationException(Exception):
    def __init__(self, server, errmsg):
        Exception.__init__(self)
        self.server = server
        self.errmsg = errmsg
    def __repr__(self):
        msg = "Server configuration for %s is incomplete (%s)"
        return msg % (self.server, self.errmsg)
    def __str__(self):
        return self.__repr__()

class MachineUnavailableException(Exception):
    def __init__(self, server, errmsg):
        Exception.__init__(self)
        self.server = server
        self.errmsg = errmsg
    def __repr__(self):
        return "Unable to connect to %s (%s)" % (self.server, self.errmsg)
    def __str__(self):
        return self.__repr__()

class MachineInterface:

    def __init__(self, machine_config, server_log):
        self.server_log    = server_log
        self.host_name     = machine_config.host_name
        self.server_home   = machine_config.server_home
        self.data          = machine_config.data
        self.output_handle = sys.stdout
        self.status        = DISCONNECTED
        self.ssh_pass      = ''
        self.ssh_conn      = None
        self.username      = self.data.get("default_user", None)
        if self.username == None:
            if self.data.get("enc_username"):
                raise EnableRequiredException()
            msg = "'default_user' is not defined"
            raise IncompleteConfigurationException(self.host_name, msg)
        self.ip_address    = self.data.get("ip_address", None)
        if self.ip_address == None:
            if self.data.get("enc_ip_address"):
                raise EnableRequiredException()
            msg = "'ip_address' is not defined"
            raise IncompleteConfigurationException(self.host_name, msg)
        self.platform     = self.data.get("platform", None)
        if self.platform == None:
            if self.data.get("enc_platform"):
                raise EnableRequiredException()
            msg = "'platform' is not defined"
            raise IncompleteConfigurationException(self.host_name, msg)
        self.connect_time = 0
        self.save_directory = ''

    def set_output_handle(self, output_handle):
        self.output_handle = output_handle

    def debug(self, msg, source=SERVER):
        self.log(source, DEBUG, msg)

    def info(self, msg, source=SERVER):
        self.log(source, INFO, msg)

    def warning(self, msg, source=SERVER):
        self.log(source, WARNING, msg)

    def error(self, msg, source=SERVER):
        self.log(source, ERROR, msg)

    def critical(self, msg, source=SERVER):
        self.log(source, CRITICAL, msg)

    def traceback_output(self, source, msg):
        self.log(source, TRACEBACK, msg)

    def from_output(self, msg, severity=INFO):
        self.log(self.host_name, severity, msg)

    def log(self, source, severity, msg):
        formatted_msg = "<<%s|%d|%s>>" % (source, severity, msg)
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
        msg = "Connecting to %s..." % self.host_name
        self.debug(msg)
        try:
            login_okay = self.ssh_conn.login(self.ip_address, self.username,
                                       self.ssh_pass, login_timeout=6000)
            if not login_okay:
                msg = "Could not connect."
                raise MachineUnavailableException(self.host_name, msg)
            self.ssh_conn.sendline('stty -echo')
            self.ssh_conn.prompt()
        except (MachineUnavailableException, pexpect.TIMEOUT):
            msg = "SSH session failed on login."
            self.debug(msg)
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
                msg = msg % (self.host_name, connection_age / 60.0)
                self.debug(msg)
                self.disconnect()
            if self.connect() != OK:
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
            self.debug(msg)
            try:
                self.disconnect()
            except:
                pass
            if self.connect() != OK:
                return FAIL
        return OK

    def process_scp(self, scp_conn):
        expect_list = [pexpect.TIMEOUT, SSH_NEW_KEY,
                       '[pP]assword: ', 'Exit status']
        select_index = scp_conn.expect(expect_list, timeout=50)
        if select_index == 0:
            msg = scp_conn.before+'|'+scp_conn.after
            raise MachineUnavailableException(self.host_name, msg)
        if select_index == 1:
            scp_conn.sendline('yes')
            scp_conn.expect('[pP]assword: ', timeout=30)
            select_index = scp_conn.expect([pexpect.TIMEOUT, '[pP]assword: '], timeout=50)
            if select_index == 0:
                msg = scp_conn.before+'|'+scp_conn.after
                raise MachineUnavailableException(self.host_name, msg)
            scp_conn.sendline(self.ssh_pass)
        if select_index == 2:
            scp_conn.sendline(self.ssh_pass)
        if select_index == 3:
            pass
        scp_conn.expect(pexpect.EOF)
        scp_conn.close()
        return OK

    def get(self, dest_file):
        self.debug( "Getting %s" % dest_file)
        cmd = 'scp -v %s@%s:%s .'
        cmd = cmd % (self.username, self.ip_address, dest_file)
        self.debug("EXECUTING: %s" % cmd, cmd)
        scp_conn = pexpect.spawn(cmd, timeout=30)
        return self.process_scp(scp_conn)

    def scp(self, source, dest, verbose=True):
        if verbose:
            msg = "Sending %s to %s:%s" % (source, self.host_name, dest)
            self.debug(msg)
        else:
            self.debug("Sending %s..." % (source))
        cmd = 'scp -v %s %s@%s:%s'
        cmd = cmd % (source, self.username, self.ip_address, dest)
        try:
            scp_conn = pexpect.spawn(cmd, timeout=600)
            select_index = scp_conn.expect([pexpect.TIMEOUT, SSH_NEW_KEY,
                         '[pP]assword: ', 'Exit status'], timeout=600)
        except pexpect.EOF:
            errMsg = "Connection refused."
            raise MachineUnavailableException(dest, errMsg)
        if select_index == 0:
            raise MachineUnavailableException(dest, scp_conn.before+'|'+scp_conn.after)
        if select_index == 1:
            scp_conn.sendline('yes')
            scp_conn.expect('[pP]assword: ', timeout=30)
            select_index = scp_conn.expect([pexpect.TIMEOUT, '[pP]assword: '], timeout=50)
            if select_index == 0:
                if type(scp_conn.before) == type("string") and \
                   type(scp_conn.after) == type("string"):
                    errMsg = scp_conn.before+'|'+scp_conn.after
                else:
                    errMsg = "before: (%s) after: (%s)" % (scp_conn.before, scp_conn.after)
                raise MachineUnavailableException(dest, errMsg)
            scp_conn.sendline(self.ssh_pass)
        if select_index == 2:
            self.debug('Using password authentication')
            scp_conn.sendline(self.ssh_pass)
        if select_index == 3:
            pass
        scp_conn.expect(pexpect.EOF)
        scp_conn.close()
        return OK

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

    def return_to_start(self):
        if self.save_directory:
            self.ssh_conn.setecho(False)
            self.ssh_conn.sendline("cd %s" % self.save_directory)
            self.ssh_conn.prompt()
            self.ssh_conn.sendline("pwd")
            self.ssh_conn.prompt()
            cwd = self.ssh_conn.before.split()[0]
            if cwd != self.save_directory:
                sys.exit(1)

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

    # FIXME: Duplicate code

    def parse_section(self, section_string, default, optional):
        sections = section_string.split('.')
        data = self.data
        for section in sections:
            try:
                data = data[section]
            except:
                data = None
                break
        if data == None:
            if not optional:
                msg = "Option %s not found" % section_string
                raise IncompleteConfigurationException(msg, None, None)
            data = default
        return data

    def getobj(self, section_string, default, expType, optional):
        value = self.parse_section(section_string, default, optional)
        if type(expType) == type("string"):
            if type(value) == type(1234) or type(value) == type(123.32):
                value = str(value)
        if type(value) == type(expType):
            return value
        raise IncompleteConfigurationException(section_string, type(value), type(expType))

    def listobj(self, section_string, default=[], optional=True):
        return self.getobj(section_string, default, [], optional)

    def string(self, section_string, default='', optional=True):
        return self.getobj(section_string, default, "string", optional)

    def integer(self, section_string, default=1, optional=True):
        return self.getobj(section_string, default, 1, optional)

    def dictionary(self, section_string, default={}, optional=True):
        return self.getobj(section_string, default, {}, optional)

