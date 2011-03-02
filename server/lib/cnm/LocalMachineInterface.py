#!/usr/bin/env python
"LocalInterface: Interface with the local server"

from bombardier_core.static_data import OK
from AbstractMachineInterface import AbstractMachineInterface
from Exceptions import CnmServerException, BuildError
import pexpect
import tempfile
import os, re, sys
import yaml, random
import StringIO, traceback
from commands import getstatusoutput as gso
from MockConfig import MockConfig

BUILD_BASE_DIR = "/tmp/bdr_build"

class LocalMachineInterface(AbstractMachineInterface):
    "Interface to a machine"
    def __init__(self, machine_config, server_log):
        AbstractMachineInterface.__init__(self, machine_config, server_log)

    def terminate(self):
        "Part of interface. No function for local system"
        pass

    def chdir(self, path):
        "Part of interface. No function for local system"
        pass

    def gso(self, cmd, raise_on_error=True, cmd_debug=False):
        "Part of interface. No function for local system"
        pass

    def run_cmd(self, command_string):
        "Run a remote shell command"
        return [OK, []]

    def _dump_trace(self):
        "Pretty print a stack trace into the logs"
        exc = StringIO.StringIO()
        traceback.print_exc(file=exc)
        exc.seek(0)
        data = exc.read()
        ermsg = ''
        for line in data.split('\n'):
            ermsg = "%% %s" % line
            self.polling_log.error(ermsg)
            self.server_log.error(ermsg, self.machine_name)

    def _svn_checkout(self, version, svn_user, svn_password,
                      svn_url, checkout_dir, debug):
        "Check out some SVN code for building a package"
        command = "export"
        if debug:
            command = "co"
        username_section = ''
        if svn_user:
            username_section = ' --username %s ' % svn_user
        cmd = "svn %s --no-auth-cache -r %s %s %s %s"
        cmd = cmd % (command, version, username_section, svn_url, checkout_dir)
        #self.polling_log.info("CMD: --- %s" % cmd)
        self.server_log.info("cmd: (%s)" % cmd, self.machine_name)
        svn_conn = pexpect.spawn(cmd, timeout=30)
        output = []
        while True:
            expect_list = [re.compile("Exported revision (\d+)"),
                           re.compile("Checked out revision (\d+)"),
                           str("Password for '%s':" % svn_user),
                           re.compile("A\s+(\S+)"),
                           "svn: REPORT request failed", 
                           "is already a working copy",
                           "doesn't exist", pexpect.TIMEOUT, pexpect.EOF]
            select_index = svn_conn.expect(expect_list, timeout=50)
            #self.server_log.info("SELECT INDEX: (%s)" % select_index, self.machine_name)
            if select_index == 0 or select_index == 1:
                version = svn_conn.match.groups()[0]
                self.server_log.info("Exported version %s" % version)
                break
            elif select_index == 2:
                svn_conn.sendline(svn_password)
            elif select_index == 3:
                self.polling_log.info("--- %s" % svn_conn.match.groups()[0])
            elif select_index > 3:
                for line in svn_conn.before.split('\n'):
                    self.polling_log.info("--- %s" % line)
                    self.server_log.info("--- %s" % line, self.machine_name)
                msg = "Could not execute SVN export (%s)" % select_index
                raise CnmServerException(msg)
        svn_conn.close()
        return version

    def _build_component(self, component_dict, section_name,
                         svn_user, svn_password, tmp_path, debug, prepare):
        '''Export one item from SVN and create an appropriate TAR file
        component_dict -- dictionary of item data
        section_name -- either 'libs' or 'injectors'
        svn_user -- account to use for logging in to svn
        svn_password -- password for said account
        '''
        tar_file_path = component_dict.get("path", '')
        svn_url = component_dict["svn"]
        if svn_url.endswith('/'):
            svn_url = svn_url[:-1]
        base = svn_url.split('/')[-1]
        if prepare:
            version = "HEAD"
        else:
            version = component_dict.get("version", "HEAD")
        expected_file_path = os.path.join(self.server_home, "repos",
                                          section_name,
                                          "%s-%s.tar.gz" % (base, version))
        
        if tar_file_path == expected_file_path:
            if os.path.isfile(tar_file_path):
                msg = "No need to build %s: already exists" % base
                self.polling_log.info(msg)
                return None
        start_path = os.getcwd()
        checkout_dir = os.path.join(tmp_path, section_name, base)
        if not os.path.isdir(checkout_dir):
            os.system("mkdir -p %s" % checkout_dir)
        
        msg = "Checking out and building %s..." % base
        self.polling_log.info(msg)
        version = self._svn_checkout(version, svn_user, svn_password,
                           svn_url, checkout_dir, debug)
        os.chdir( checkout_dir )
        section_dir =  os.path.join(self.server_home, "repos", section_name)
        if not os.path.isdir(section_dir):
            status = os.system("mkdir -p %s" % section_dir)
            if status != OK:
                msg = "Could not create required directory: %s" % section_dir
                raise CnmServerException(msg)
            else:
                msg = "Created required directory: %s" % section_dir
                self.server_log.warning(msg)
        tar_file_path = os.path.join(section_dir, 
                                     "%s-%s.tar.gz" % (base, version))
        cmd = "tar -czf %s *" % ( tar_file_path )
        status, output = gso(cmd)
        if status != OK:
            self.server_log.error("Command: %s" % cmd)
            self.server_log.error("Current Directory: %s" % os.getcwd())
            self.server_log.error("Output from command: %s" % output)
            msg = "Could not create tarball"
            os.chdir( start_path )
            raise CnmServerException(msg)
        os.chdir( start_path )
        return tar_file_path, version

    def get_non_builtin_modules(self):
        modules = []
        module_list = sys.modules.values()
        for item in module_list:
            work_str = str(item)[8:-1]
            if not " from " in work_str:
                continue
            module_name = item.__name__
            item_str, source = work_str.split(" from ")
            source_path = source[1:-1]
            modules.append((item_str, source_path, module_name))
        return modules

    def remove_test_modules(self, prefix=BUILD_BASE_DIR):
        "removes any trace of modules we've imported"
        modules = self.get_non_builtin_modules()
        for item_str, source_path, module_name in modules:
            if source_path.startswith(prefix):
                self.server_log.info("UN-IMPORTING: %30s %20s / %s" % (item_str, source_path, module_name))
                del sys.modules[module_name]

    def verify_test_modules(self, prefix):
        "Assures that all modules we're importing come from what we want"
        modules = self.get_non_builtin_modules()
        for item_str, source_path, module_name in modules:
            if source_path.startswith(BUILD_BASE_DIR):
                if not source_path.startswith(prefix):
                    raise BuildError("We have a dirty build environment (%s)" % source_path)

    def pre_cleanup(self, tmp_path):
        "Make sure the build environment is clean."
        self.remove_test_modules()
        self.verify_test_modules(tmp_path)
        bad_path_entries = []
        for entry in sys.path:
            if entry.startswith(BUILD_BASE_DIR):
                bad_path_entries.append(entry)
        for bad_path_entry in bad_path_entries:
            self.polling_log.warning("Removing stale import path %s" % bad_path_entry)
            sys.path.remove(bad_path_entry) 
        self.polling_log.info("Build environment is clean.")

    def _inspect_libraries(self, tmp_path, pkg_data):
        """This will instantiate the Bombardier installer script and collect
        all of the configuration information that it requires"""
        start_path = os.getcwd()
        injectors_path = os.path.join(tmp_path, "injectors")
        if not os.path.isdir(injectors_path):
            os.system("mkdir -p %s" % injectors_path)
        os.chdir(injectors_path)
        base_name = ''
        class_name = ''
        description = ''
        self.pre_cleanup(tmp_path)
        try:
            tmp_libs = os.path.join(tmp_path, "libs")
            sys.path.append(tmp_libs)

            output = []
            metadata = {}

            class_name = pkg_data.get("class_name")
            base_name  = class_name.split('.')[0]

            letters = [ chr( x ) for x in range(65, 91) ]
            random.shuffle(letters)
            rand_name = ''.join(letters)
            self.polling_log.info("My current directory: %s" % os.getcwd())
            if base_name in sys.modules:
                sys.modules.pop(base_name)
            if class_name in sys.modules:
                sys.modules.pop(class_name)
            cmd = "import %s as %s" % ( class_name, rand_name )
            exec(cmd)
            exec("description = %s.__doc__" % (rand_name))
            if not description:
                exec("description = %s.%s.__doc__" % (rand_name, base_name))
            if description:
                self.polling_log.info("Description: %s" % description)
            else:
                self.polling_log.warning("Class %s has no description" % class_name)

            self.verify_test_modules(tmp_path)

            config = MockConfig()
            exec ('object = %s.%s(config)' % ( rand_name, base_name))

            if hasattr(object, "metadata"):
                metadata = object.metadata
            config_requests = config.get_requests()
            for item in config_requests:
                self.polling_log.info("Configuration dictionary: %s" % item)
            public_methods = {}
            for method in dir(object):
                if callable(getattr(object, method)) and not method.startswith('_'):
                    exec("docstring = object.%s.__doc__" % method)
                    if not docstring:
                        docstring = 'Undocumented Executable'
                    public_methods[method] = docstring
        except Exception, exp:
            os.chdir(start_path)
            raise

        # cleanup:
        self.remove_test_modules(tmp_path)
        sys.path.remove(tmp_libs)
        os.chdir(start_path)
        return description, config_requests, public_methods

        
    def build_components(self, package_name, svn_user, svn_password,
                         debug, prepare):
        '''We want to create files that match all of the svn entries for
        a given package
        package_name -- the name of the package we're modifying
        svn_user / svn_password -- svn credentials
        '''
        try:
            os.system("mkdir -p %s" % BUILD_BASE_DIR)
            tmp_path = tempfile.mkdtemp(dir=BUILD_BASE_DIR)
            package_file = os.path.join(self.server_home, "package",
                                        "%s.yml" % package_name)
            pkg_data = yaml.load(open(package_file).read())
            output = []
            for section_name in ["injectors", "libs"]:
                for component_name in pkg_data[section_name]:
                    component_dict = pkg_data[section_name][component_name]
                    if "svn" in component_dict:
                        tar_file_path, version = self._build_component(component_dict,
                                                              section_name,
                                                              svn_user,
                                                              svn_password,
                                                              tmp_path, debug,
                                                              prepare)
                        pkg_data[section_name][component_name]["version"] = version
                        if tar_file_path:
                            pkg_data[section_name][component_name]["path"] = tar_file_path
                            output.append("built %s" % tar_file_path)

            pkg_data["release"] = pkg_data.get("release", 0) + 1
            description, configuration, methods = self._inspect_libraries(tmp_path, pkg_data)
            pkg_data['description'] = description
            pkg_data['configuration'] = configuration
            pkg_data['executables'] = methods
            open(package_file, 'w').write(yaml.dump(pkg_data))
            self.polling_log.info("Note: not deleting %s" % tmp_path)
        except Exception:
            self._dump_trace()
            advice = "%s is still available for code inspection." % tmp_path
            self.polling_log.error(advice)
        
        return OK, output

