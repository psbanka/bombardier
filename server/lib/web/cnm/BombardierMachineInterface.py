#!/usr/bin/env python

import re, os, base64
import yaml
import StringIO
import copy
import traceback
from MachineInterface import MachineInterface, MachineUnavailableException
from Exceptions import MachineConfigurationException
from bombardier_core.mini_utility import strip_version
from bombardier_core.static_data import OK, FAIL, PURGE, DEBUG, EXECUTE
from bombardier_core.static_data import INIT, ACTION_DICT, RETURN_DICT

from pexpect import EOF
import syck


TMP_FILE = "tmp.yml"
DOT_LENGTH = 20
BLK_SIZE = 77
PROGRESS = "install-progress"
LOCAL_PACKAGES = "local-packages"

class BombardierMachineInterface(MachineInterface):

    def __init__(self, machine_config, server_log):
        MachineInterface.__init__(self, machine_config, server_log)
        self.status_data    = {}
        self.local_filename = ''
        self.report_info    = ''
        self.state_machine  = []
        self.default_group  = "root"

        self.state_machine.append([re.compile("\=\=REPORT\=\=:(.*)"), self.get_report])
        self.state_machine.append([re.compile("\=\=REQUEST-CONFIG\=\="), self.send_client])
        self.state_machine.append([re.compile("Unable to shred before deleting"), self.no_report])
        self.state_machine.append([re.compile("Uninstalling package \((\S+)\)"), self.uninstall])
        self.state_machine.append([re.compile("Beginning installation of \((\S+)\)"), self.install])
        self.state_machine.append([re.compile("(\S+) result for (\S+) : (\d)"), self.action_result])

        self.log_matcher = re.compile( "\d+\-\d+\-\d+\s\d+\:\d+\:\d+\,\d+\|([A-Z]+)\|(.+)\|" )
        self.trace_matcher = re.compile( "\|\|\>\>\>(.+)" )
        if self.platform == 'win32':
            self.python  = '/cygdrive/c/Python25/python.exe'
            self.spkg_dir = '/cygdrive/c/spkg'
        else:
            self.python  = '/usr/bin/python'
            self.spkg_dir = '/opt/spkg'
        if self.data.get("python_path"):
            self.python = self.data.get("python_path")
        if self.data.get("spkg_path"):
            self.spkg_dir = self.data.get("spkg_path")
        self.cmd_debug = None
        self.pull_report = True

    # BEING USED
    def scp_dict(self, copy_dict):
        self.connect()
        for file_type in copy_dict:
            if file_type == "dist":
                dest_dir = '.'
            else:
                dest_dir = os.path.join(self.spkg_dir, self.host_name, file_type)
            for source_file in copy_dict[file_type]:
                source_path = os.path.join(self.server_home, file_type,
                                           source_file)
                self.polling_log.info("SOURCE_PATH: %s // DEST_DIR: %s" % (source_path, dest_dir))
                status = self.scp(source_path, dest_dir)
        return
    
    # BEING USED
    def freshen(self):
        status_file = os.path.join(self.server_home, "status",
                                   "%s.yml" % self.host_name)
        self.status_data = ''
        if os.path.isfile(status_file):
            try:
                self.status_data = syck.load(open(status_file).read())
            except Exception, exc:
                if exc[0] == "syntax error":
                    msg = "Syntax error in status information for %s"
                    self.polling_log.error(msg % self.host_name)
                    self.status_data = ''
            if type(self.status_data) != type({}) \
                or PROGRESS not in self.status_data \
                or LOCAL_PACKAGES not in self.status_data:
                    msg = "Invalid status data. Ignoring."
                    self.polling_log.warning(msg)
        return(MachineInterface.freshen(self))

    def action_result(self, data):
        action, package_name, result = data
        message = "%s %s: %s" % (action.lower(), RETURN_DICT[int(result)], package_name)
        self.polling_log.info(message)
        self.server_log.info(message, self.host_name)

    def action_start(self, action, package_name):
        message = "%s installing %s" % (self.host_name, package_name)
        self.polling_log.info(message)
        self.server_log.info(message, self.host_name)

    def install(self, package_name):
        self.action_start("installing", package_name)

    def uninstall(self, package_name):
        self.action_start("uninstalling", package_name)

    def no_report(self, data):
        self.pull_report = False

    def send_package(self, package_name, dest_path):
        file_name = os.path.join(self.server_home, "packages", package_name)
        if not os.path.isfile(file_name):
            message = "Machine requested a file that is not on this server: %s"
            self.polling_log.error(message % file_name)
            return OK
        self.scp(file_name, dest_path, False)

    def stream_file(self, file_name):
        plain_text = open(file_name, 'rb').read()
        return self.stream_data(plain_text)

    def stream_data(self, plain_text):
        import zlib
        compressed = zlib.compress(plain_text)
        encoded    = base64.encodestring(compressed)
        self.ssh_conn.setecho(False)
        handle = StringIO.StringIO(encoded)
        msg = "==> Sending configuration information:"
        self.polling_log.info(msg)
        while True:
            chunk = handle.read(BLK_SIZE)
            if chunk == '':
                chunk = ' '*(BLK_SIZE-1)+'\n'
                self.ssh_conn.send(chunk)
                break
            if len(chunk) < BLK_SIZE:
                pad = ' '*(BLK_SIZE-len(chunk))
                chunk = chunk[:-1] + pad + '\n'
            self.ssh_conn.send(chunk)

    def send_client(self, data):
        if data:
            pass
        tmp_file_path = self.server_home+"/"+TMP_FILE
        open(tmp_file_path, 'w').write(yaml.dump( self.data ))
        self.stream_file(tmp_file_path)
        if os.path.isfile(tmp_file_path):
            os.unlink(tmp_file_path)

    def get_report(self, yaml_line):
        self.report_info += yaml_line + "\n"

    def process_message(self, message):
        for state in self.state_machine:
            match, function = state
            grep_info = match.findall(message)
            if grep_info:
                if function:
                    function(grep_info[0])
                    return True
        return False

    def gso(self, cmd, raise_on_error=True):
        if self.cmd_debug:
            msg = "* RUNNING: %s" % cmd
            self.polling_log.debug(msg)
            self.server_log.debug(msg, self.host_name)
        try:
            self.ssh_conn.sendline( cmd )
            self.ssh_conn.prompt()
        except EOF:
            if raise_on_error:
                msg = "Error running %s" % (cmd)
                raise MachineUnavailableException(self.host_name, msg)
            else:
                return ""
        output = self.ssh_conn.before.strip()
        if self.cmd_debug:
            self.polling_log.info("* OUTPUT: %s" % output)
        return output

    def chdir(self, path):
        if not path:
            path = self.spkg_dir
        self.polling_log.debug("Changing directory to %s" % path)
        self.ssh_conn.sendline ('cd %s' % path)
        self.ssh_conn.prompt()

    def run_cmd(self, command_string):
        self.report_info = ''
        if self.freshen() != OK:
            msg = "Unable to connect to %s." % self.host_name
            raise MachineUnavailableException(self.host_name, msg)
        return_code = OK
        self.ssh_conn.sendline(command_string)
        command_complete = False
        total_output = ''
        output_checker = re.compile('(.*)'+self.ssh_conn.PROMPT)
        while True:
            output = self.ssh_conn.read_nonblocking()
            total_output += output
            self.polling_log.info("CMD OUTPUT: %s" % output)
            if output_checker.findall(total_output):
                self.polling_log.info("EXITING")
                break
        self.ssh_conn.setecho(False)
        return [OK, '\n'.join(total_output.split('\n')[:-1])] # FIXME, get status

    def dump_trace(self):
        stack_trace = []
        found_index = 1
        while found_index == 1:
            stack_trace.append(self.ssh_conn.match.groups()[0])
            expect_list = [self.ssh_conn.PROMPT, self.trace_matcher,
                           self.log_matcher]
            found_index = self.ssh_conn.expect(expect_list, timeout=6000)
        tString = ''.join(stack_trace)

        noop_search = "NoOptionError\: No option \'(\w+)\' in section\: \'(\w+)\'"
        re_obj = re.compile(noop_search)
        data = re_obj.findall(tString)
        if data:
            message1 = "Invalid client configuration data"
            self.polling_log.error(message1)
            self.server_log.error(message1, self.host_name)
            if len(data) == 2:
                message2 = "Need option '%s' in section '%s'." % (data[0], data[1])
            else:
                message2 = "Need options: %s" % data
            self.polling_log.info(message2)
            self.server_log.error(message2, self.host_name)
        data = re.compile("NoSectionError\: No section\: \'(\w+)\'").findall(tString)
        if data:
            message1 = "Invalid client configuration data"
            self.polling_log.error(message1)
            self.server_log.error(message1, self.host_name)
            message2 = "Need section '%s'." % (data[0])
            self.polling_log.info(message2)
            self.server_log.error(message2, self.host_name)
        else:
            for line in stack_trace:
                self.polling_log.error(line)
                self.server_log.error(line, self.host_name)

    def get_package_names_from_progress(self):
        #CANNIBALIZED FROM PackageField.py
        status_yml = os.path.join(self.server_home, "status", "%s.yml" % self.host_name)
        if not os.path.isfile(status_yml):
            self.polling_log.info("Cannot retrieve status (NO FILE: %s)" % status_yml)
            return {}
        yml = syck.load( open(status_yml).read() )
        if yml == None:
            self.polling_log.info("Cannot retrieve status (EMPTY FILE: %s)" % status_yml)
            return {}
        return yml

    def get_all_package_names(self):
        package_names = set([strip_version(x) for x in self.get_package_names_from_progress().get(PROGRESS, {})])
        package_names = package_names.union(set(self.data.get("packages")))
        return list(package_names)

    def get_package_data(self, package_name):
        yml_path = os.path.join(self.server_home, "package", "%s.yml" % package_name)
        package_data = {}
        if not os.path.isfile(yml_path):
            self.server_log.warning("Requested invalid package: %s" % package_name)
            raise MachineConfigurationException(self.host_name)
        else:
            package_data = syck.load(open(yml_path).read())
        return package_data

    def send_all_client_data(self, action):
        send_data = {"configData": self.data, 
                     "package_data": {},
                     "packageData": {}, # FIXME
                    }
        if action != PURGE:
            package_names = self.get_all_package_names()
            for package_name in package_names:
                this_package_data = self.get_package_data(package_name)
                if not this_package_data:
                    message = "Could not find package data for %s." % package_name
                    self.polling_log.error(message)
                    raise MachineConfigurationException(self.host_name)
                send_data["package_data"][package_name] = this_package_data
                send_data["packageData"][package_name] = this_package_data # FIXME
        self.stream_data(yaml.dump(send_data))

    def run_bc(self, action, package_name, script_name, debug):
        self.chdir(self.spkg_dir)
        if self.platform == "win32":
            cmd = "cat /proc/registry/HKEY_LOCAL_MACHINE/SOFTWARE/Python/PythonCore/2.5/InstallPath/@"
            python_home_win = self.gso(cmd)
            python_home_cyg = self.gso("cygpath $(%s)" %cmd)
            self.get_status_yml()
            cmd = "%spython.exe '%sScripts\\bc.py' %s %s %s %s" % (python_home_cyg, python_home_win,
                  ACTION_DICT[action], self.host_name, package_name, script_name)
        else:
            cmd = "export PYTHON_HOME=$(%s -c 'import sys; print sys.prefix')" % self.python
            gso_out = self.gso(cmd)
            cmd = '%s $PYTHON_HOME/bin/bc.py %s %s %s %s' % (self.python, ACTION_DICT[action],
                                         self.host_name, package_name, script_name)
        self.ssh_conn.sendline(cmd)
        self.send_all_client_data(action)
        found_index = 0
        return_code = OK
        while True:
            expect_list = [self.ssh_conn.PROMPT, self.trace_matcher,
                           self.log_matcher]
            found_index = self.ssh_conn.expect(expect_list, timeout=6000)
            if found_index == 1: # Stack trace
                self.server_log.error("FOUND A STACK TRACE")
                self.dump_trace()
                self.ssh_conn.prompt()
                self.get_status_yml()
                return FAIL, ["Machine raised an exception."]
            elif found_index == 0: # BC exited
                if self.ssh_conn.before.strip():
                    msg = "Remaining output: %s" % self.ssh_conn.before.strip()
                    self.polling_log.info(msg)
                    self.server_log.info(msg, self.host_name)
                self.ssh_conn.setecho(False)
                break
            elif found_index == 2: # Log message
                level, message = self.ssh_conn.match.groups()
                if level in ["ERROR", "CRITICAL"]:
                    return_code = FAIL
                if not self.process_message(message):
                    message = message.strip()
                    self.polling_log.log_message(level, message)
                    self.server_log.log_message(level, message, self.host_name)
        return return_code

    def send_package(self, package_name, dest_path):
        filename = os.path.join(self.server_home, "packages", package_name)
        if not os.path.isfile(filename):
            message = "Client requested a file that is not on this server: %s" % filename
            self.server_log.error(message, self.host_name)
            return OK
        self.scp(filename, dest_path, False)

    def upload_new_packages(self):
        dest_path = os.path.join(self.spkg_dir, self.host_name, "packages")
        delivered_packages = self.get_package_names_from_progress().get(LOCAL_PACKAGES, [])
        required_base_names = copy.deepcopy(self.data.get("packages"))
        newest_names = []
        for base_name in required_base_names:
            newest_data = self.get_package_data(base_name)
            newest_name = newest_data.get("install", {}).get("fullName")
            if newest_name not in delivered_packages:
                msg = "Need to send package: %s" % newest_name
                self.server_log.info(msg, self.host_name)
                self.send_package(newest_name+".spkg", dest_path)

    def process(self, action, package_name, script_name, debug):
        try:
            self.report_info = ''
            self.pull_report = True
            if action == EXECUTE:
                self.clear_script_output(script_name)
            if self.freshen() != OK:
                msg = "UNABLE TO CONNECT TO %s. No actions are available."
                self.server_log.error(msg, self.host_name)
                return FAIL, [msg % self.host_name]
            return_code = OK
            if action != INIT:
                pass
                self.upload_new_packages()
            return_code = self.run_bc(action, package_name, script_name, debug)
        except MachineUnavailableException:
            return FAIL, ["Remote system refused connection."]
        except MachineConfigurationException:
            return FAIL, []
        except EOF:
            return FAIL, ["Machine unexpectedly disconnected."]
        except Exception, exc:
            print "------------------- PROCESS EXEPTION"
            exc = StringIO.StringIO()
            traceback.print_exc(file=exc)
            exc.seek(0)
            data = exc.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg = "%% %s" % line
                self.polling_log.error(ermsg)
                self.server_log.error(ermsg, self.host_name)
            return FAIL, ["Exception in client-handling code."]

        self.get_status_yml()
        if action == EXECUTE:
            if self.report_info:
                file_name = "%s-%s.yml" % (self.host_name, script_name)
                file_path = os.path.join(self.server_home, "output", file_name)
                open(file_path, 'w').write(self.report_info)
                cmd = "chgrp %s %s 2> /dev/null"
                os.system(cmd % (self.default_group, file_path))
                cmd = "chmod 660 %s 2> /dev/null"
                os.system(cmd % (file_path))
            else:
                if not self.pull_report:
                    return return_code, []
                self.get_script_output(script_name)

            try:
                report_data = yaml.load(self.report_info)
            except:
                return FAIL, ["Bad output from script:", self.report_info]
            return return_code, report_data
        return return_code, []

    def clear_script_output(self, script_name):
        file_name = "%s-%s.yml" % (self.host_name, script_name)
        file_path = os.path.join(self.server_home, "output", file_name)
        if os.path.isfile(file_path):
            os.unlink(file_path)
        return

    def get_script_output(self, script_name):
        remote_file_name = "%s-output.yml" % (script_name)
        self.local_filename  = "%s-%s.yml" % (self.host_name, script_name)
        self.get("%s/output/%s" % (self.spkg_dir, remote_file_name))
        if os.path.isfile(remote_file_name):
            report_path = os.path.join(self.server_home, "output",
                                       self.local_filename)
            os.system("mv -f %s %s" % (remote_file_name, report_path ))
            self.report_info = open(report_path).read()
        return

    def get_status_yml(self):
        status_dir = os.path.join(self.server_home, 'status')
        if not os.path.isdir( status_dir ):
            os.makedirs( status_dir )

        new_line = 'cat %s/%s/status.yml;echo "======="'
        self.ssh_conn.sendline(new_line % (self.spkg_dir, self.host_name))
        self.ssh_conn.prompt()
        status_yml = str(self.ssh_conn.before).split("======")[0]
        status_yml = status_yml.replace('\r','')
        try:
            syck.load(status_yml)
        except:
            msg = "status.yml could not be parsed (writing to error.yml)"
            self.polling_log.error(msg)
            open( os.path.join(status_dir, "error.yml"), 'w' ).write(status_yml)
            return
        status_file = os.path.join(status_dir, "%s.yml" % self.host_name)
        try:
            open( status_file, 'w' ).write(status_yml)
            cmd = "chgrp %s %s 2> /dev/null"
            os.system(cmd % (self.default_group, status_file))
            cmd = "chmod 660 %s 2> /dev/null"
            os.system(cmd % (status_file))
        except IOError, ioe:
            self.polling_log.error("Unable to write '%s' (%s)" % (status_file, ioe))

