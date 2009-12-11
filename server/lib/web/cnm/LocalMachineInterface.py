#!/usr/bin/env python
"LocalInterface: Interface with the local server"

from bombardier_core.static_data import OK
from AbstractMachineInterface import AbstractMachineInterface
from Exceptions import CnmServerException
import pexpect
import tempfile
import os
import yaml
from commands import getstatusoutput as gso

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
                      svn_url, checkout_dir):
        cmd = "svn export --no-auth-cache -r %d --username %s %s %s"
        cmd = cmd % (version, svn_user, svn_url, checkout_dir)
        
        svn_conn = pexpect.spawn(cmd, timeout=30)
        select_index = 100
        while select_index != 0:
            expect_list = ["Exported revision",
                           str("Password for '%s':" % svn_user),
                           "svn: REPORT request failed", 
                           "doesn't exist", pexpect.TIMEOUT]
            select_index = svn_conn.expect(expect_list, timeout=50)
            if select_index == 1:
                svn_conn.sendline(svn_password)
            elif select_index > 1:
                msg = "Could not execute SVN export"
                raise CnmServerException(msg)
        svn_conn.close()

    def _build_component(self, component_dict,
                         section_name, svn_user, svn_password):
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
        version = component_dict["version"]
        expected_file_path = os.path.join(self.server_home, "repos",
                                          section_name,
                                          "%s-%d.tar.gz" % (base, version))

        if tar_file_path == expected_file_path:
            if os.path.isfile(tar_file_path):
                msg = "No need to build %s: already exists" % base
                self.polling_log.info(msg)
                return None
        tar_file_path = expected_file_path
        start_path = os.getcwd()
        tmp_path = tempfile.mkdtemp()
        checkout_dir = os.path.join(tmp_path, "%s-%d" % (base, version))
        
        msg = "Checking out and building %s..." % base
        self.polling_log.info(msg)
        self._svn_checkout(version, svn_user, svn_password,
                           svn_url, checkout_dir)
        os.chdir( checkout_dir )
        cmd = "tar -czf %s *" % ( tar_file_path )
        status, output = gso(cmd)
        if status != OK:        
            msg = "Could not create tarball"
            raise CnmServerException(msg)
        cmd = "rm -rf %s" % ( tmp_path )
        #status, output = gso(cmd)
        return tar_file_path
        
    def build_components(self, package_name, svn_user, svn_password):
        '''We want to create files that match all of the svn entries for
        a given package
        package_name -- the name of the package we're modifying
        svn_user / svn_password -- svn credentials
        '''
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
                                                          svn_password)
                    if tar_file_path:
                        modified = True
                        pkg_data[section_name][component_name]["path"] = tar_file_path
                        output.append("built %s" % tar_file_path)
        if modified:
            pkg_data["release"] = release + 1
            open(package_file, 'w').write(yaml.dump(pkg_data))
        else:
            output = ["Nothing to do."]
        return OK, output

