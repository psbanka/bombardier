"Bombardier Machine Interface"
#!/usr/bin/env python

import re, os, base64
from commands import getstatusoutput as gso
import glob
import yaml
import StringIO
import copy
import traceback
from MachineInterface import MachineInterface, MachineUnavailableException
from Exceptions import MachineConfigurationException, BombardierMachineException
from Exceptions import MachineStatusException, PackageNotFound
from bombardier_core.static_data import OK, FAIL, EXECUTE
from bombardier_core.static_data import INIT, FIX, PURGE
from bombardier_core.static_data import ACTION_DICT, RETURN_DICT
from bombardier_core.mini_utility import update_dict
from MachineStatus import MachineStatus, LOCAL_PACKAGES
from bombardier_core.Cipher import Cipher

from pexpect import EOF
import tempfile

BLK_SIZE = 77
TMP_FILE = "tmp.yml"

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
        self.machine_status = MachineStatus(self.server_home, self.machine_name)

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
        self.pull_report = True

    ##############################################################
    ## PRIVATE METHODS
    ##############################################################

    def _get_state_machine(self):
        "Return a list of compiled regular expression objects for log handling"
        exit_re        = re.compile("\=\=EXIT-CODE\=\=:(\d+)")
        report_re      = re.compile("\=\=REPORT\=\=:(.*)")
        output_re      = re.compile("\=\=OUTPUT\=\=:(.*)")
        no_report_re   = re.compile("Unable to shred before deleting")
        uninstall_re   = re.compile("Uninstalling package \((\S+)\)")
        install_re     = re.compile("Beginning installation of \((\S+)\)")
        result_re      = re.compile("(\S+) result for (\S+) : (\d)")

        state_machine = [ [exit_re,        self._get_exit_code],
                          [report_re,      self._get_report],
                          [output_re,      self._get_output],
                          [no_report_re,   self._no_report],
                          [uninstall_re,   self._uninstall],
                          [install_re,     self._install],
                          [result_re,      self._get_action_result],
                        ]
        return state_machine

    # STATE-MACHINE ROUTINES

    def _action_start(self, action, package_name):
        "Logging for beginning of an action"
        message = "%s installing %s" % (self.machine_name, package_name)
        self.polling_log.info(message)
        self.server_log.info(message, self.machine_name)

    def _get_exit_code(self, exit_code):
        "Parse action code from string data, this needs to be more defensive."
        message = "Output received: %s" % exit_code
        self.polling_log.debug(message)
        self.server_log.debug(message, self.machine_name)
        self.exit_code = int(exit_code)

    def _get_report(self, yaml_line):
        "Add a yaml line to the report"
        self.report_info += yaml_line + "\n"

    def _get_output(self, output):
        "Add output to logging and action result"
        #self.server_log.info(output)
        #self.polling_log.info(output, self.machine_name)
        self.action_result.append(output)

    def _no_report(self, data):
        "Don't pull report"
        self.pull_report = False

    def _uninstall(self, package_name):
        "Flag the logger that an un-installation is taking place"
        self._action_start("uninstalling", package_name)

    def _install(self, package_name):
        "Flag the logger that an installation is taking place"
        self._action_start("installing", package_name)

    def _get_action_result(self, data):
        "Add action result to logs"
        action, package_name, result = data
        message = "%s %s: %s" % (action.lower(),
                    RETURN_DICT[int(result)], package_name)
        self.polling_log.info(message)
        self.server_log.info(message, self.machine_name)
        self.action_result.append(message)

    # END STATE MACHINE ROUTINES

    def _stream_file(self, file_name):
        "Stream a file"
        plain_text = open(file_name, 'rb').read()
        return self._stream_data(plain_text)

    def _stream_data(self, plain_text):
        "Send file contents over stdin via pxssh"
        import zlib
        compressed = zlib.compress(plain_text)
        encoded    = base64.encodestring(compressed)
        self.ssh_conn.setecho(False)
        handle = StringIO.StringIO(encoded)
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

    def _scp_all_client_data(self, raw_data):
        """If the remote machine has a configuration key, we will
        encrypt its configuration data and secure-copy it to him rather
        than stream the data to bc.py's stdin"""
        enc_key = self.data['config_key']
        yaml_data_str = yaml.dump(raw_data)
        cipher = Cipher(enc_key)
        enc_data = cipher.encrypt_string(yaml_data_str)
        dest_path = os.path.join(self.spkg_dir, self.machine_name, "client.yml.enc")
        temp_file = tempfile.mkstemp()[1]
        open(temp_file, 'w').write(enc_data)
        self.scp(temp_file, dest_path, False)
        #self.server_log.debug("Cleaning local temporary file %s" % temp_file)
        #os.system("rm -f %s" % temp_file)
        self._stream_data("config_key: '%s'\n" % enc_key)

    def _get_all_client_data(self):
        """For a remote machine that doesn't have a configuration key, this
        method gathers all the configuration data that it needs"""
        send_data = {"config_data": self.data, 
                     "package_data": {},
                    }
        package_names = self.machine_status.get_all_package_names(self.data.get("packages"))
        for package_name in package_names:
            this_package_data = self.machine_status.get_package_data(package_name)
            if not this_package_data:
                message = "Could not find package data for %s."
                self.polling_log.error(message % package_name)
                raise MachineConfigurationException(self.machine_name)
            send_data["package_data"][package_name] = this_package_data
        return send_data

    def _send_all_client_data(self, action):
        "Stream package metadata to a remote client"
        if action == PURGE:
            send_data = {"config_data": self.data}
        else:
            send_data = self._get_all_client_data()
        if 'config_key' in self.data and action != INIT:
            self._scp_all_client_data(send_data)
        else:
            self.polling_log.info("Streaming configuration data...")
            self._stream_data(yaml.dump(send_data))

    def _get_bc_command(self):
        "Get shell command to run bc.py on the target system."
        cmd = ""
        if self.platform == "win32":
            cmd = "cat /proc/registry/HKEY_LOCAL_MACHINE/SOFTWARE/"
            cmd += "Python/PythonCore/2.5/InstallPath/@"
            python_home_win = self.gso(cmd)
            python_home_cyg = self.gso("cygpath $(%s)" %cmd)
            self._get_status_yml()
            cmd = "%spython.exe '%sScripts\\bc.py' " % \
                  (python_home_cyg, python_home_win)
        else:
            cmd = "export PYTHON_HOME=$(%s -c 'import sys; print sys.prefix')"
            cmd = cmd % self.python
            gso_out = self.gso(cmd)
            cmd = '%s $PYTHON_HOME/bin/bc.py ' % self.python
        return cmd

    def _run_bc(self, action,
               package_name, script_name, debug):
        "Run bc.py on a remote machine, and watch the logs"
        self.report_info = ''
        self.chdir(self.spkg_dir)

        cmd = self._get_bc_command()
        cmd += " %s %s %s %s" % (ACTION_DICT[action],
                                self.machine_name, package_name, script_name)
        self.ssh_conn.sendline(cmd)
        self._send_all_client_data(action)
        self._watch_bc()

        try:
            if self.report_info:
                return yaml.load(self.report_info)
            return self.action_result
        except Exception:
            msg = "Cannot read report data: (%s)" % self.report_info
            self.polling_log.error(msg)
            self.exit_code = FAIL
            return [msg]

    def _process_message(self, message):
        "Parse log message and possibly take action"
        for state in self.state_machine:
            match, function = state
            grep_info = match.findall(message)
            if grep_info:
                if function:
                    function(grep_info[0])
                    return True
        return False

    def _watch_bc(self):
        "Watch log output from bc.py"
        while True:
            expect_list = [self.ssh_conn.PROMPT, self.trace_matcher,
                           self.log_matcher]
            found_index = self.ssh_conn.expect(expect_list, timeout=6000)
            if found_index == 0: # BC exited
                if self.ssh_conn.before.strip():
                    msg = "Remaining output: %s" % self.ssh_conn.before.strip()
                    self.polling_log.debug(msg)
                    self.server_log.debug(msg, self.machine_name)
                self.ssh_conn.setecho(False)
                break
            elif found_index == 1: # Stack trace
                raise BombardierMachineException()
            elif found_index == 2: # Log message
                level, message = self.ssh_conn.match.groups()
                if not self._process_message(message):
                    message = message.strip()
                    self.polling_log.log_message(level, message)
                    self.server_log.log_message(level, message, self.machine_name)

    def _check_file(self, package_name, file_path):
        "Determine if a file specified in the type-5 package actually exists"
        if file_path:
            if not os.path.isfile(file_path):
                msg = "%s not found." % file_path
                self.server_log.error(msg, self.machine_name)
                raise PackageNotFound(package_name, file_path)

    def _get_type_5_files(self, package_name, package_data):
        """Examine the files specified by a type-5 package and create
        a dictionary of files to be synchronized to the remote machine"""
        package_sync = {
                        "injectors": [],
                        "libs": [],
                       }

        for dir_name in [ "libs", "injectors" ]:
            sync_section = package_data.get(dir_name, [])
            self.server_log.info("SYNC SECTION: %s" % sync_section)
            for sync_item in sync_section:
                sync_data = sync_section[sync_item]
                sync_file = sync_data.get("path")
                if not sync_file:
                    raise PackageNotFound(package_name, "No PATH DEFINED")
                if not sync_file.startswith(os.path.sep):
                    sync_file = os.path.join(self.server_home, "repos",
                                             dir_name, sync_file)
                self._check_file( package_name, sync_file )
                package_sync[dir_name].append(sync_file)
        return package_sync

    def _create_sync_directory(self, files_to_send):
        "Creating a temporary directory where RSYNC can operate"
        tmp_path = tempfile.mkdtemp()
        for directory_name in files_to_send:
            subdir = os.path.join(tmp_path, directory_name)
            cmd = "mkdir -p %s" % subdir
            os.system(cmd)
            file_list = files_to_send[directory_name]
            for file_name in file_list:
                cmd = "ln -s %s %s" % (file_name, subdir)
                self.server_log.info("Running: %s" % cmd)
                os.system(cmd)
        return tmp_path

    def _rsync_repository(self, tmp_path, dest):
        """Performing an RSYNC command to get all repos files to
        the remote machine"""
        cmd = "rsync -La %s %s@%s:%s" 
        cmd = cmd % (tmp_path, self.username, self.ip_address, dest)
        self.server_log.info("Running: %s" % cmd)
        files = glob.glob("%s/*" % tmp_path)
        self.polling_log.debug(cmd)
        status, output = gso(cmd)
        if status != OK:
            msg = "Error rsyncing repository: %s" % output
            raise MachineUnavailableException(self.machine_name, msg)

    def _get_delivered_packages(self):
        "Find out what packages exist on the remote machine"
        try:
            package_names = self.machine_status.get_package_names_from_progress()
            delivered_packages = package_names.get(LOCAL_PACKAGES, [])
        except MachineStatusException:
            msg = "No/invalid status data. Assuming there are "\
                  "no packages on remote system"
            self.polling_log.warning(msg)
            delivered_packages = []
        return delivered_packages

    def _get_files_to_send(self, required_base_names, delivered_packages):
        "Determine what files need to be sync'd to the remote machine"
        files_to_send = {"type4": []}
        for base_name in required_base_names:
            newest_data = self.machine_status.get_package_data(base_name)
            version = newest_data.get("package-version")
            if version == 4:
                newest_name = newest_data.get("install", {}).get("fullName")
                path_to_file = os.path.join(self.server_home, "repos",
                                            "type4", "%s.spkg" % newest_name)
                self._check_file(base_name, path_to_file)
                if newest_name not in delivered_packages:
                    files_to_send["type4"].append(path_to_file)
            elif version == 5:
                package_sync = self._get_type_5_files(base_name, newest_data)
                files_to_send = update_dict(files_to_send, package_sync)
        return files_to_send

    def _upload_new_packages(self):
        "Send needed packages to a machine using symlinks and rsync"
        self.polling_log.info("Syncing packages...")
        delivered_packages = self._get_delivered_packages()
        required_base_names = copy.deepcopy(self.data.get("packages"))
        files_to_send = self._get_files_to_send(required_base_names,
                                                delivered_packages)

        tmp_path = self._create_sync_directory(files_to_send)
        dest = os.path.join(self.spkg_dir, "repos")
        self._rsync_repository(tmp_path + os.path.sep, dest)
        dest = os.path.join(self.spkg_dir, self.machine_name, "packages")

        type_4_dir = os.path.join(self.spkg_dir, "repos", "type4")
        cmd = "ln -fs %s/*.spkg %s/" % (type_4_dir, dest)
        self.ssh_conn.sendline(cmd)
        if not self.ssh_conn.prompt(timeout = 5):
            dead = True
            msg = "Unable to create links: %s" % self.ssh_conn.before
            raise MachineUnavailableException(self.machine_name, msg)
            
        #self.server_log.debug("Removing directory %s..." % tmp_path)
        #os.system("rm -rf %s" % tmp_path)
        self.polling_log.debug("...Finished syncing packages.")

    def _dump_trace(self):
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

    def _clear_script_output(self, script_name):
        "Remove old output yml file if it exists on remote machine"
        file_name = "%s-%s.yml" % (self.machine_name, script_name)
        file_path = os.path.join(self.server_home, "output", file_name)
        if os.path.isfile(file_path):
            os.unlink(file_path)
        return

    def _get_status_yml(self):
        "Pull status.yml from remote machine and cache it locally"
        status_dir = os.path.join(self.server_home, 'status')
        if not os.path.isdir( status_dir ):
            os.makedirs( status_dir )

        new_line = 'cat %s/%s/status.yml;echo "======="'
        self.ssh_conn.sendline(new_line % (self.spkg_dir, self.machine_name))
        self.ssh_conn.prompt()
        status_yml = str(self.ssh_conn.before).split("======")[0]
        status_yml = status_yml.replace('\r','')
        try:
            yaml.load(status_yml)
        except:
            error_file =  os.path.join(status_dir, "error.yml")
            msg = "status.yml could not be parsed (writing to %s)" % error_file
            self.polling_log.error(msg)
            open( error_file, "w" ).write(status_yml)
            return
        status_file = os.path.join(status_dir, "%s.yml" % self.machine_name)
        try:
            open( status_file, 'w' ).write(status_yml)
            cmd = "chgrp %s %s 2> /dev/null"
            os.system(cmd % (self.default_group, status_file))
            cmd = "chmod 660 %s 2> /dev/null"
            os.system(cmd % (status_file))
        except IOError, ioe:
            msg = "Unable to write '%s' (%s)" % (status_file, ioe)
            self.polling_log.error(msg)

    ##############################################################
    ## PUBLIC METHODS
    ##############################################################

    def freshen(self, job_name, require_status):
        "Refresh config data"
        self.set_job(job_name)
        try:
            MachineInterface.freshen(self)
        except MachineUnavailableException:
            self.unset_job()
            raise
        try:
            self.machine_status.freshen()
        except MachineStatusException:
            msg = "Can't find status. Fetching from machine."
            self.polling_log.warning(msg)
            self._get_status_yml()
            try:
                self.machine_status.freshen()
            except MachineStatusException:
                if require_status:
                    self.unset_job()
                    raise
        return OK

    def take_action(self, action, package_name, script_name, debug):
        '''
        Run a package-oriented action on a remote machine
        action -- one of the following: EXECUTE, INIT, INSTALL,
                  UNINSTALL, VERIFY, CONFIGURE, DRY_RUN
        package_name -- the name of a package to operate on
        script_name -- the name of a script to run (againsta a package)
        debug -- defunct
        '''
        message = []
        try:
            self.action_result = []
            self.pull_report = True
            if action == EXECUTE:
                self._clear_script_output(script_name)
            if not action in [ INIT, FIX, PURGE ]:
                self._upload_new_packages()
            message = self._run_bc(action, package_name, script_name, debug)
        except MachineUnavailableException:
            message = ["Remote system refused connection."]
            self.exit_code = FAIL
        except MachineConfigurationException, exc:
            self.exit_code = FAIL
            message = ["Machine configuration error: %s" % exc]
        except EOF:
            self.exit_code = FAIL
            message = ["Machine unexpectedly disconnected."]
        except BombardierMachineException, exc:
            self.server_log.error("FOUND A STACK TRACE")
            self._dump_trace()
            self.ssh_conn.prompt()
            self._get_status_yml()
            self.exit_code = FAIL
            message = ["Machine raised an exception."]
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
                self.server_log.error(ermsg, self.machine_name)
            self.exit_code = FAIL
            message = ["Exception in client-handling code."]
        self._get_status_yml()
        msg = "take_action is returning: %s / %s" % (self.exit_code, message)
        self.server_log.debug(msg)
        return self.exit_code, message

