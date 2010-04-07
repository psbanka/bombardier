#!/usr/bin/env python
"LocalInterface: Interface with the local server"

from bombardier_core.static_data import OK
from AbstractMachineInterface import AbstractMachineInterface
from Exceptions import CnmServerException
import pexpect
import tempfile
import os, re, sys
import yaml, random
import StringIO, traceback
from commands import getstatusoutput as gso
from MockConfig import MockConfig

class LocalMachineInterface(AbstractMachineInterface):
    "Interface to a machine"
    def __init__(self, machine_config, server_log):
        AbstractMachineInterface.__init__(self, machine_config, server_log)

    def terminate(self):
        pass

    def chdir(self, path):
        pass

    def gso(self, cmd, raise_on_error=True, cmd_debug=False):
        "Run a remote shell command"
        pass

    def run_cmd(self, command_string):
        "Run a remote shell command"
        return [OK, []]

    def dump_trace(self):
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
        tar_file_path = os.path.join(self.server_home, "repos",
                                     section_name,
                                     "%s-%s.tar.gz" % (base, version))
        cmd = "tar -czf %s *" % ( tar_file_path )
        status, output = gso(cmd)
        os.chdir( start_path )
        if status != OK:        
            msg = "Could not create tarball"
            raise CnmServerException(msg)
        return tar_file_path, version

    def _inspect_libraries(self, tmp_path, pkg_data):
        """This will instantiate the Bombardier installer script and collect
        all of the configuration information that it requires"""
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
        config = MockConfig()
        exec ('object = %s.%s(config)' % ( rand_name, base_name))

        if hasattr(object, "metadata"):
            metadata = object.metadata
        config_requests = config.get_requests()
        for item in config_requests:
            self.polling_log.info("Configuration dictionary: %s" % item)
        public_methods = []
        for method in dir(object):
            if callable(getattr(object, method)) and not method.startswith('_'):
                public_methods.append(method)

        if base_name in sys.modules:
            sys.modules.pop(base_name)
        if class_name in sys.modules:
            sys.modules.pop(class_name)
        sys.path.remove(tmp_libs)
        exec('del %s' % rand_name)
        return config_requests, public_methods
        
    def build_components(self, package_name, svn_user, svn_password,
                         debug, prepare):
        '''We want to create files that match all of the svn entries for
        a given package
        package_name -- the name of the package we're modifying
        svn_user / svn_password -- svn credentials
        '''
        try:
            tmp_path = tempfile.mkdtemp()
            package_file = os.path.join(self.server_home, "package",
                                        "%s.yml" % package_name)
            pkg_data = yaml.load(open(package_file).read())
            release = pkg_data.get("release", 0)
            output = []
            for section_name in ["injectors", "libs"]:
                for component_name in pkg_data[section_name]:
                    component_dict = pkg_data[section_name][component_name]
                    path = component_dict.get("path", '')
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
            pkg_data["release"] = release + 1
            configuration, methods = self._inspect_libraries(tmp_path, pkg_data)
            pkg_data['configuration'] = configuration
            pkg_data['executables'] = methods
            open(package_file, 'w').write(yaml.dump(pkg_data))
            self.polling_log.info("Note: not deleting %s" % tmp_path)
        except Exception:
            self.dump_trace()
            advice = "%s is still available for code inspection." % tmp_path
            self.polling_log.error(advice)
        
        return OK, output

