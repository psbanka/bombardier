#!/usr/bin/python
# BSD License
# Copyright (c) 2009, Peter Banka et al
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the name of the GE Security nor the names of its contributors may
#   be used to endorse or promote products derived from this software without
#   specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

"""This provides a top-level command for the bomsh shell. This is for
managing machines in the network, and subclasses the [PinshCmd] module
"""

__author__ =  'Peter Banka'
__version__ = '1.0'

import PinshCmd
from ConfigField import ConfigField, MACHINE, DIST, PACKAGE
from bombardier_core.static_data import OK, FAIL
from SystemStateSingleton import SystemState, ENABLE
from PackageField import PackageField, FIX, PURGE, NOT_INSTALLED, INSTALLED
import PackageActionField
from Exceptions import MachineTraceback, CommandError
import Integer
system_state = SystemState()
import libUi, time, yaml

URL_LOOKUP = {'test': "json/machine/start_test/%s",
              'dist': "json/machine/dist/%s",
              'init': "json/machine/init/%s",
              'reconcile': "json/machine/reconcile/%s",
              'status': "json/machine/status/%s",
              'enable': "json/machine/enable/%s",
              'disable': "json/machine/disable/%s",
             }

def setup_test():
    fresh_status_yaml = """ 
clientVersion: 0.70-596
install-progress: {}
local-packages: []
status: {newInstall: 'True'}
timestamp: 1251918530.6349609"""

    status_file = "/opt/spkg/localhost/status.yml"
    open(status_file, "w").write(fresh_status_yaml)

class Machine(PinshCmd.PinshCmd):
#    '''bomsh# package Testpackagetype4 install localhost 
##       [OK, ['install OK: TestPackageType4-7', 'verify OK: TestPackageType4-7']]
#    '''
#       bomsh# package Testpackagetype4 install localhost 
#       [OK, ['install OK: TestPackageType4-7', 'verify OK: TestPackageType4-7', 'Finished install for TestPackageType4.']]
#       bomsh# package TestPackagetype4 echo localhost 
#       [OK, {'test file exists': 'True', 'contents of test file': 'The Quick Brown Fox Jumped over the Lazy Dog.'}]
#    '''
#       bomsh# package Testpackagetype4 uninstall localhost 
#       [OK, ['uninstall OK: TestPackageType4-7']]
#       bomsh# package bogusPackage install localhost 
#       [FAIL, []]
#    '''
    '''
       bomsh# machine localhost enable
       [OK, []]
       bomsh# machine localhost test
       [OK, ['Machine localhost is ready to take commands.']]
       bomsh# machine localhost dist test
       [OK, ['localhost updated with test']]
       bomsh# machine localhost init
       [OK, []]
       bomsh# machine localhost reconcile
       [OK, ['install OK: TestPackageType4-7', 'verify OK: TestPackageType4-7', 'Finished installing']]
       bomsh# machine localhost status
       [OK, []]
       bomsh# machine localhost status purge TestPackageType4-7
       [OK, ['TestPackageType4-7 has been removed from localhost status']]
       bomsh# machine localhost status fix TestPackageType4
       [OK, ['TestPackageType4-7 has been set to INSTALLED.']]
       bomsh# machine localhost status purge TestPackageType4-7
       [OK, ['TestPackageType4-7 has been removed from localhost status']]
       bomsh# machine localhost disable
       [OK, []]
       bomsh# machine localhost enable
       [OK, []]
    '''
    def __init__(self):
        """Top-level object has a 'test' child: test the machine
        """
        PinshCmd.PinshCmd.__init__(self, "machine")
        self.help_text = "machine\tcommands that operate on a given machine"
        self.cmd_owner = 1
        self.machine_field = None
        self.children = []
        self.build_children()
        self.auth = ENABLE

    def build_children(self):
        # Top-level commands
        self.machine_field = ConfigField(data_type=MACHINE)
        self.children = [self.machine_field]

        # Second-level commands
        test = PinshCmd.PinshCmd("test", "test\ttest a machine connection")
        dist = PinshCmd.PinshCmd("dist", "dist\tdeploy a library to a machine")
        enable = PinshCmd.PinshCmd("enable", "enable\tshare ssh keys with a machine")
        disable = PinshCmd.PinshCmd("disable", "disable\tremove ssh key from a machine")
        init = PinshCmd.PinshCmd("init", "init\tintialize bombardier on a machine")
        reconcile = PinshCmd.PinshCmd("reconcile", "reconcile\treconcile machine state to bill of materials")
        status = PinshCmd.PinshCmd("status", "status\tstatus reporting or manipulation")
        install = PinshCmd.PinshCmd("install", "install\tinstall a package")
        uninstall = PinshCmd.PinshCmd("uninstall", "uninstall\tremove a package")
        configure = PinshCmd.PinshCmd("configure", "configure\tconfigure a package")
        verify = PinshCmd.PinshCmd("verify", "verify\tverify a package")
        execute = PinshCmd.PinshCmd("execute", "execute\trun an action on a package")
        maintenance = PinshCmd.PinshCmd("maintenance", "maintenance\tset a server to be in maintenance mode")
        self.machine_field.children = [test, dist, init, enable, maintenance,
                                       disable, status, reconcile, execute,
                                       install, uninstall, configure, verify]

        # Third-level commands
        dist_field = ConfigField(data_type=DIST)
        dist.children = [dist_field]
        fix = PinshCmd.PinshCmd("fix", "fix\tsets the state of a package to INSTALLED")
        purge = PinshCmd.PinshCmd("purge", "purge\tremoves a package from the status of a machine")
        status.children = [fix, purge]
        installed_package_field = PackageField(INSTALLED)
        not_installed_package_field = PackageField(NOT_INSTALLED)
        executable_package_field = PackageField(INSTALLED)
        install.children=[not_installed_package_field]
        uninstall.children=[installed_package_field]
        verify.children=[installed_package_field]
        configure.children=[installed_package_field]
        execute.children=[executable_package_field]

        # Fourth-level commands
        fix.children = [PackageField(action_type=FIX)]
        purge.children = [PackageField(action_type=PURGE)]
        package_actions = PackageActionField.PackageActionField()
        executable_package_field.children = [package_actions]

    def analyze_output(self, machine_name, tokens, output, post_data):
        command = tokens[2].lower()
        if command == "test":
            for i in range(1,5):
                if "Testing %d/4" % i not in output["complete_log"]:
                    return FAIL, "Invalid output from server"
                return OK, [ "Machine %s is ready to take commands." % machine_name ]
        elif command == "dist":
            if output["command_status"] == OK:
                return OK, ["%s updated with %s" % (machine_name, post_data["dist"])]
            else:
                return FAIL, ["%s FAILED to install %s" % (machine_name, post_data["dist"])]
        return output["command_status"], output["command_output"]

    def watch_job(self, job_name):
        libUi.info("Job name: %s" % job_name)
        output = {"alive": 1}
        while output.get("alive", 0):
            time.sleep(0.25)
            url = "json/job/poll/%s" % job_name
            output = system_state.cnm_connector.service_yaml_request(url)
            if "traceback" in output:
                raise MachineTraceback(url, output["traceback"])
            new_output = output["new_output"]
            if new_output:
                libUi.process_cnm(new_output)
        libUi.info("Joining...")
        output = system_state.cnm_connector.join_job(job_name)
        return output

    def check_machine_name(self, tokens, no_flag):
        possible_machine_names = self.machine_field.preferred_names(tokens, 1)
        machine_name = possible_machine_names[0]
        if no_flag:
            raise CommandError("NO cannot be used here")
        if len(possible_machine_names) == 0:
            raise CommandError("Unknown machine name: %s" % tokens[2])
        if len(possible_machine_names) > 1:
            raise CommandError("Ambiguous machine name: %s" % tokens[2])
        return machine_name

    def get_url_and_post(self, machine_name, tokens):
        command = tokens[2].lower()
        if command not in URL_LOOKUP:
            raise CommandError("%s is not a valid command" % command)

        post_data = {}
        url = URL_LOOKUP[command] % machine_name
        if command == "dist":
            post_data = {"dist": tokens[-1]}
        elif command == "enable":
            prompt = "%s administrative ssh password: " % machine_name
            password = libUi.pwd_input(prompt)
            yml_dict = yaml.dump( {"password" : password } )
            post_data = {"yaml" : yml_dict}
        elif command == "status":
            if len(tokens) > 3:
                if len(tokens) < 5:
                    raise CommandError("Incomplete command")
                sub_command = tokens[3].lower()
                if sub_command not in ["fix", "purge"]:
                    raise CommandError("%s is not a valid command" % sub_command)
                post_data["action"] = sub_command
                post_data["machine"] = machine_name
                url = "/json/package_action/%s" % tokens[4]
        return url, post_data

    def cmd(self, tokens, no_flag):
        """
        tokens -- all of the keywords passed in the command string, parsed
        no_flag -- whether the 'no' keyword was used in the command string
        """
        if len(tokens) < 2:
            raise CommandError("Incomplete command.")
        machine_name = self.check_machine_name(tokens, no_flag)
        if len(tokens) == 2:
            system_state.push_prompt(["machine", machine_name])
            return OK,[]

        command = tokens[2].lower()

        if command in ["install", "uninstall", "verify",
                       "configure", "execute"]:
            if command == "execute":
                if len(tokens) != 5:
                    raise CommandError("Incomplete Command")
                command = tokens[4]
            package_name = tokens[3]
            machine_name = tokens[1]

            url = "/json/package_action/%s" % package_name
            post_data = {"machine": machine_name,
                         "action": command}
        else:
            url, post_data = self.get_url_and_post(machine_name, tokens)

        try:
            job_name = system_state.cnm_connector.get_job(url, post_data)
            output = self.watch_job(job_name)
            return self.analyze_output(machine_name, tokens, output, post_data)
        except MachineTraceback, m_err:
            libUi.process_traceback(m_err)
            return FAIL,[]

