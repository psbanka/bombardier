"Bombardier Machine Interface"
#!/usr/bin/env python

import re, os, base64
import yaml
import StringIO
import copy
import traceback
from MachineInterface import MachineInterface, MachineUnavailableException
from Exceptions import MachineConfigurationException
from bombardier_core.mini_utility import strip_version
from bombardier_core.static_data import OK, FAIL, PURGE, EXECUTE
from bombardier_core.static_data import INIT, ACTION_DICT, RETURN_DICT

from pexpect import EOF
import syck


TMP_FILE = "tmp.yml"
DOT_LENGTH = 20
BLK_SIZE = 77
PROGRESS = "install-progress"
LOCAL_PACKAGES = "local-packages"

class BombardierMachineInterface(MachineInterface):
    "Bombardier subclass of MachineInterface"
    def __init__(self, machine_config, server_log):
        "Set up machine interface and set up based on the environment."        
        MachineInterface.__init__(self, machine_config, server_log)
        self.status_data    = {}
        self.local_filename = ''
        self.report_info    = ''
        self.default_group  = "root"
        self.exit_code      = FAIL
        self.action_result  = []
        self.state_machine  = self._get_state_machine()

        log_re_str = "\d+\-\d+\-\d+\s\d+\:\d+\:\d+\,\d+\|([A-Z]+)\|(.+)\|" 
        self.log_matcher = re.compile(log_re_str) 

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

    def _get_state_machine(self):
        "Return a list of compiled regular expression objects for log handling"
        exit_re        = re.compile("\=\=EXIT-CODE\=\=:(\d+)")
        report_re      = re.compile("\=\=REPORT\=\=:(.*)")
        output_re      = re.compile("\=\=OUTPUT\=\=:(.*)")
        no_report_re   = re.compile("Unable to shred before deleting")
        uninstall_re   = re.compile("Uninstalling package \((\S+)\)")
        install_re     = re.compile("Beginning installation of \((\S+)\)")
        result_re      = re.compile("(\S+) result for (\S+) : (\d)")

        state_machine = [ [exit_re,        self.get_exit_code],
                          [report_re,      self.get_report],
                          [output_re,      self.get_output],
                          [no_report_re,   self.no_report],
                          [uninstall_re,   self.uninstall],
                          [install_re,     self.install],
                          [result_re,      self.get_action_result],
                        ]
        return state_machine

    # BEING USED
    def scp_dict(self, copy_dict):
        "Use scp with a dictionary to copy files grouped by file type"
        self.connect()
        for file_type in copy_dict:
            if file_type == "dist":
                dest_dir = '.'
            else:
                dest_dir = os.path.join(self.spkg_dir, 
                                        self.host_name, file_type)
            for source_file in copy_dict[file_type]:
                source_path = os.path.join(self.server_home, file_type,
                                           source_file)
                msg = "SOURCE_PATH: %s // DEST_DIR: %s"
                msg =  msg % (source_path, dest_dir)
                self.polling_log.info(msg)
                status = self.scp(source_path, dest_dir)
        return
    
    # BEING USED
    def freshen(self):
        "Refresh config data"
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
            self.check_status_data()
        return(MachineInterface.freshen(self))

    def check_status_data(self):
        "Check for proper elements being present in status data"
        valid = type(self.status_data) == type({}) and \
                PROGRESS in self.status_data and \
                LOCAL_PACKAGES in self.status_data
        if not valid:
            msg = "Invalid status data. Ignoring."
            self.polling_log.warning(msg)

    def get_output(self, output):
        "Add output to logging and action result"
        self.server_log.info(output)
        self.polling_log.info(output, self.host_name)
        self.action_result.append(output)

    def get_exit_code(self, exit_code):
        "Parse action code from string data, this needs to be more defensive."
        message = "Output received: %s" % exit_code
        self.polling_log.info(message)
        self.server_log.info(message, self.host_name)
        self.exit_code = int(exit_code)

    def get_action_result(self, data):
        "Add action result to logs"
        action, package_name, result = data
        message = "%s %s: %s" % (action.lower(),
                    RETURN_DICT[int(result)], package_name)
        self.polling_log.info(message)
        self.server_log.info(message, self.host_name)
        self.action_result.append(message)

    def action_start(self, action, package_name):
        "Logging for beginning of an action"
        message = "%s installing %s" % (self.host_name, package_name)
        self.polling_log.info(message)
        self.server_log.info(message, self.host_name)

    def install(self, package_name):
        "install"
        self.action_start("installing", package_name)

    def uninstall(self, package_name):
        "uninstall"
        self.action_start("uninstalling", package_name)

    def no_report(self, data):
        "Don't pull report"
        self.pull_report = False

    def send_package(self, package_name, dest_path):
        "Scp a package to a remote machine"
        filename = os.path.join(self.server_home, "packages", package_name)
        if not os.path.isfile(filename):
            message = "Client requested a file that is not on this server: %s"
            message = message  % filename
            self.server_log.error(message, self.host_name)
            self.polling_log.error(message)
            return OK
        self.scp(filename, dest_path, False)

    def stream_file(self, file_name):
        "Stream a file"
        plain_text = open(file_name, 'rb').read()
        return self.stream_data(plain_text)

    def stream_data(self, plain_text):
        "Send file contents over stdin via pxssh"
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
        "Stream yaml data to a client"
        tmp_file_path = self.server_home+"/"+TMP_FILE
        open(tmp_file_path, 'w').write(yaml.dump( self.data ))
        self.stream_file(tmp_file_path)
        if os.path.isfile(tmp_file_path):
            os.unlink(tmp_file_path)

    def get_report(self, yaml_line):
        "Add a yaml line to the report"
        self.report_info += yaml_line + "\n"

    def process_message(self, message):
        "Parse log message and possibly take action"
        for state in self.state_machine:
            match, function = state
            grep_info = match.findall(message)
            if grep_info:
                if function:
                    function(grep_info[0])
                    return True
        return False

    def gso(self, cmd, raise_on_error=True):
        "Run a remote shell command"
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

    def run_cmd(self, command_string):
        "Run a remote shell command"
        self.report_info = ''
        if self.freshen() != OK:
            msg = "Unable to connect to %s." % self.host_name
            raise MachineUnavailableException(self.host_name, msg)
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
            self.server_log.error(invalid_data_msg, self.host_name)
            if len(data) == 2:
                need_msg = "Need option '%s' in section '%s'." % \
                            (data[0], data[1])
            else:
                need_msg = "Need options: %s" % data
            self.polling_log.info(need_msg)
            self.server_log.error(need_msg, self.host_name)
        no_section_re = re.compile("NoSectionError\: No section\: \'(\w+)\'")
        data = no_section_re.findall(t_string)
        if data:
            self.polling_log.error(invalid_data_msg)
            self.server_log.error(invalid_data_msg, self.host_name)
            need_msg = "Need section '%s'." % (data[0])
            self.polling_log.info(need_msg)
            self.server_log.error(need_msg, self.host_name)
        else:
            for line in stack_trace:
                self.polling_log.error(line)
                self.server_log.error(line, self.host_name)

    def get_package_names_from_progress(self):
        "Pull status yaml data from local file"
        #CANNIBALIZED FROM PackageField.py
        status_yml = os.path.join(self.server_home, "status", \
                                  "%s.yml" % self.host_name)
        if not os.path.isfile(status_yml):
            msg = "Cannot retrieve status (NO FILE: %s)" % status_yml
            self.polling_log.info(msg)
            return {}
        yml = syck.load( open(status_yml).read() )
        if yml == None:
            msg = "Cannot retrieve status (EMPTY FILE: %s)" % status_yml
            self.polling_log.info(msg)
            return {}
        return yml

    def get_all_package_names(self):
        "Get package names from status and this object and union them together."
        package_list = self.get_package_names_from_progress().get(PROGRESS, {})
        package_names = set([strip_version(x) for x in package_list])
        package_names = package_names.union(set(self.data.get("packages")))
        return list(package_names)

    def get_package_data(self, package_name):
        "Get package metadata from a local yaml file."
        yml_path = os.path.join(self.server_home, "package",
                                "%s.yml" % package_name)
        package_data = {}
        if not os.path.isfile(yml_path):
            msg = "Requested invalid package: %s" % package_name
            self.server_log.warning()
            raise MachineConfigurationException(self.host_name)
        else:
            package_data = syck.load(open(yml_path).read())
        return package_data

    def send_all_client_data(self, action):
        "Stream package metadata to a remote client"
        send_data = {"configData": self.data, 
                     "package_data": {},
                     "packageData": {}, # FIXME
                    }
        if action != PURGE:
            package_names = self.get_all_package_names()
            for package_name in package_names:
                this_package_data = self.get_package_data(package_name)
                if not this_package_data:
                    message = "Could not find package data for %s."
                    self.polling_log.error(message % package_name)
                    raise MachineConfigurationException(self.host_name)
                send_data["package_data"][package_name] = this_package_data
                # FIXME
                send_data["packageData"][package_name] = this_package_data 
        self.stream_data(yaml.dump(send_data))

    def get_bc_command(self):
        if self.platform == "win32":
            cmd = "cat /proc/registry/HKEY_LOCAL_MACHINE/SOFTWARE/"
            cmd += "Python/PythonCore/2.5/InstallPath/@"
            python_home_win = self.gso(cmd)
            python_home_cyg = self.gso("cygpath $(%s)" %cmd)
            self.get_status_yml()
            cmd = "%spython.exe '%sScripts\\bc.py' " % \
                  (python_home_cyg, python_home_win)
        else:
            cmd = "export PYTHON_HOME=$(%s -c 'import sys; print sys.prefix')"
            cmd = cmd % self.python
            gso_out = self.gso(cmd)
            cmd = '%s $PYTHON_HOME/bin/bc.py ' % self.python
        

    def run_bc(self, action,
               package_name, script_name, package_revision, debug):
        "Run bc.py on a remote machine, and watch the logs"
        self.report_info = ''
        self.chdir(self.spkg_dir)
        self.server_log.info("PACKAGE_REVISION: %s" % package_revision)
        if package_revision:
            package_name += '-%s' % package_revision
            self.server_log.info("PACKAGE_NAME: %s" % package_name)

        cmd = self.get_bc_command()
        cmd += " %s %s %s %s" % (ACTION_DICT[action],
                                self.host_name, package_name, script_name)
        self.ssh_conn.sendline(cmd)
        self.send_all_client_data(action)
        found_index = 0
        while True:
            bc_result = self.watch_bc()
            if bc_result:
                return bc_result
        try:
            if self.report_info:
                return yaml.load(self.report_info)
            return self.action_result
        except Exception:
            msg = "Cannot read report data: (%s)" % self.report_info
            self.polling_log.error(msg)
            self.exit_code = FAIL
            return [msg]

    def watch_bc(self):
        expect_list = [self.ssh_conn.PROMPT, self.trace_matcher,
                       self.log_matcher]
        found_index = self.ssh_conn.expect(expect_list, timeout=6000)
        if found_index == 0: # BC exited
            if self.ssh_conn.before.strip():
                msg = "Remaining output: %s" % self.ssh_conn.before.strip()
                self.polling_log.info(msg)
                self.server_log.info(msg, self.host_name)
            self.ssh_conn.setecho(False)
        elif found_index == 1: # Stack trace
            self.server_log.error("FOUND A STACK TRACE")
            self.dump_trace()
            self.ssh_conn.prompt()
            self.get_status_yml()
            self.exit_code = FAIL
            return ["Machine raised an exception."]
        elif found_index == 2: # Log message
            level, message = self.ssh_conn.match.groups()
            if not self.process_message(message):
                message = message.strip()
                self.polling_log.log_message(level, message)
                self.server_log.log_message(level, message, self.host_name)

    def upload_new_packages(self):
        "Send needed packages to a machine"
        dest_path = os.path.join(self.spkg_dir, self.host_name, "packages")
        package_names = self.get_package_names_from_progress()
        delivered_packages = package_names.get(LOCAL_PACKAGES, [])
        required_base_names = copy.deepcopy(self.data.get("packages"))
        newest_names = []
        for base_name in required_base_names:
            newest_data = self.get_package_data(base_name)
            newest_name = newest_data.get("install", {}).get("fullName")
            if newest_name not in delivered_packages:
                msg = "Need to send package: %s" % newest_name
                self.server_log.info(msg, self.host_name)
                self.send_package(newest_name+".spkg", dest_path)

    def take_action(self, action, package_name, script_name, 
                    package_revision, debug):
        "Run a maintenance script on a machine"
        message = []
        try:
            self.action_result = []
            self.pull_report = True
            if action == EXECUTE:
                self.clear_script_output(script_name)
            if self.freshen() != OK:
                msg = "UNABLE TO CONNECT TO %s. No actions are available."
                self.server_log.error(msg, self.host_name)
                return FAIL, [msg % self.host_name]
            if action != INIT:
                self.upload_new_packages()
            message = self.run_bc(action, package_name, script_name,
                                  package_revision, debug)
        except MachineUnavailableException:
            message = ["Remote system refused connection."]
            self.exit_code = FAIL
        except MachineConfigurationException:
            self.exit_code = FAIL
        except EOF:
            self.exit_code = FAIL
            message = ["Machine unexpectedly disconnected."]
        except Exception, exc:
            print "------------------- PROCESS EXCEPTION"
            exc = StringIO.StringIO()
            traceback.print_exc(file=exc)
            exc.seek(0)
            data = exc.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg = "%% %s" % line
                self.polling_log.error(ermsg)
                self.server_log.error(ermsg, self.host_name)
            self.exit_code = FAIL
            message = ["Exception in client-handling code."]
        self.get_status_yml()
        msg = "take_action is returning: %s / %s" % (self.exit_code, message)
        self.server_log.info(msg)
        return self.exit_code, message

    def clear_script_output(self, script_name):
        "Remove old output yml file if it exists on remote machine"
        file_name = "%s-%s.yml" % (self.host_name, script_name)
        file_path = os.path.join(self.server_home, "output", file_name)
        if os.path.isfile(file_path):
            os.unlink(file_path)
        return

    def get_script_output(self, script_name):
        "Read output yaml file and add it to the report"
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
        "Pull status.yml from remote machine and cache it locally"
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
            msg = "Unable to write '%s' (%s)" % (status_file, ioe)
            self.polling_log.error(msg)

