#!/usr/bin/env python
"LocalInterface: Interface with the local server"

from bombardier_core.static_data import OK
from AbstractMachineInterface import AbstractMachineInterface
from Exceptions import CnmServerException
import pexpect
import tempfile
import os, re, sys
import yaml
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
        pass

    def _svn_checkout(self, version, svn_user, svn_password,
                      svn_url, checkout_dir, debug):
        if debug:
            cmd = "svn co --no-auth-cache -r %s --username %s %s %s"
        else:
            cmd = "svn export --no-auth-cache -r %s --username %s %s %s"
        cmd = cmd % (version, svn_user, svn_url, checkout_dir)
        
        svn_conn = pexpect.spawn(cmd, timeout=30)
        select_index = 100
        while True:
            expect_list = [re.compile("Exported revision (\d+)"),
                           re.compile("Checked out revision (\d+)"),
                           str("Password for '%s':" % svn_user),
                           re.compile("A\s+(\S+)"),
                           "svn: REPORT request failed", 
                           "doesn't exist", pexpect.TIMEOUT, pexpect.EOF]
            select_index = svn_conn.expect(expect_list, timeout=50)
            #self.polling_log.info("SELECT INDEX: (%s)" % select_index)
            if select_index == 0 or select_index == 1:
                version = svn_conn.match.groups()[0]
                break
            elif select_index == 2:
                svn_conn.sendline(svn_password)
            elif select_index == 3:
                self.polling_log.info("--- %s" % svn_conn.match.groups()[0])
            elif select_index > 2:
                msg = "Could not execute SVN export"
                raise CnmServerException(msg)
        svn_conn.close()
        return version

    def _build_component(self, component_dict, section_name, svn_user,
                         svn_password, tmp_path, debug, prepare):
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
            version = component_dict["version"]
        expected_file_path = os.path.join(self.server_home, "repos",
                                          section_name,
                                          "%s-%s.tar.gz" % (base, version))
        
        if tar_file_path == expected_file_path:
            if os.path.isfile(tar_file_path):
                msg = "No need to build %s: already exists" % base
                self.polling_log.info(msg)
                return None
        start_path = os.getcwd()
        checkout_dir = os.path.join(tmp_path, base)
        
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
        if status != OK:        
            msg = "Could not create tarball"
            raise CnmServerException(msg)
        return tar_file_path

    def _inspect_libraries(self, tmp_path, pkg_data):
        """This will instantiate the Bombardier installer script and collect
        all of the configuration information that it requires"""
        sys.path.append(tmp_path)

        output = []
        metadata = {}
        class_name = pkg_data.get("class_name")
        exec("import %s as Pkg" % class_name )
        if not hasattr(Pkg, "metadata"):
            msg = "Package does not contain any metadata"
            self.polling_log.info(msg)
        else:
            metadata = Pkg.metadata
        config = MockConfig()
        exec ('object = Pkg.%s(config)' % class_name.split('.')[0])
        pkg_data['configuration'] = config.get_requests()
        for item in pkg_data['configuration']:
            self.polling_log.info("Configuration item: %s" % item)
        if class_name in sys.modules:
            sys.modules.pop(class_name)
        sys.path.remove(tmp_path)
        return output
        
    def build_components(self, package_name, svn_user, svn_password,
                         debug, prepare):
        '''We want to create files that match all of the svn entries for
        a given package
        package_name -- the name of the package we're modifying
        svn_user / svn_password -- svn credentials
        '''
        tmp_path = tempfile.mkdtemp()
        package_file = os.path.join(self.server_home, "package",
                                    "%s.yml" % package_name)
        pkg_data = yaml.load(open(package_file).read())
        release = pkg_data.get("release", 0)
        modified = False
        output = []
        for section_name in ["injectors", "libs"]:
            for component_name in pkg_data[section_name]:
                component_dict = pkg_data[section_name][component_name]
                path = component_dict.get("path", '')
                if "svn" in component_dict and "version" in component_dict:
                    tar_file_path = self._build_component(component_dict,
                                                          section_name,
                                                          svn_user,
                                                          svn_password,
                                                          tmp_path, debug,
                                                          prepare)
                    if tar_file_path:
                        modified = True
                        pkg_data[section_name][component_name]["path"] = tar_file_path
                        output.append("built %s" % tar_file_path)
        if modified:
            pkg_data["release"] = release + 1
            open(package_file, 'w').write(yaml.dump(pkg_data))
            output += self._inspect_libraries(tmp_path, pkg_data)
        else:
            output = ["Nothing to do."]
        cmd = "rm -rf %s" % ( tmp_path )
        status, output = gso(cmd)
        
        return OK, output

