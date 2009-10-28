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

__author__ =  'Peter Banka, Shawn Sherwood'
__version__ = '1.0'

import PinshCmd
import PackageActionField
from ConfigField import ConfigField, MACHINE, DIST, PACKAGE
from bombardier_core.static_data import OK, FAIL
from SystemStateSingleton import SystemState, ENABLE
from Exceptions import MachineTraceback, CommandError
import MultipleChoice, Integer
system_state = SystemState()
import libUi, time

def setup_test():
    fresh_status_yaml = """ 
clientVersion: 0.70-596
install-progress: {}
local-packages: []
status: {newInstall: 'True'}
timestamp: 1251918530.6349609"""

    status_file = "/opt/spkg/localhost/status.yml"
    open(status_file, "w").write(fresh_status_yaml)

class Package(PinshCmd.PinshCmd):
#    '''bomsh# package Testpackagetype4 install localhost 
#       [OK, ['install OK: TestPackageType4-7', 'verify OK: TestPackageType4-7']]
    '''
       bomsh# package Testpackagetype4 install localhost 
       [OK, ['install OK: TestPackageType4-7', 'verify OK: TestPackageType4-7', 'Finished install for TestPackageType4.']]
       bomsh# package TestPackagetype4 echo localhost 
       [OK, {'test file exists': 'True', 'contents of test file': 'The Quick Brown Fox Jumped over the Lazy Dog.'}]
    '''
#       bomsh# package Testpackagetype4 uninstall localhost 
#       [OK, ['uninstall OK: TestPackageType4-7']]
#       bomsh# package bogusPackage install localhost 
#       [FAIL, []]
#    '''

    def __init__(self):
        """Top-level object has a 'test' child: test the machine
        """
        PinshCmd.PinshCmd.__init__(self, "package")
        self.help_text = "package\tcommands that operate on a single package"
        self.cmd_owner = 1

        self.package_field = ConfigField(data_type=PACKAGE)
        self.children = [self.package_field]

        package_actions = PackageActionField.PackageActionField()
        self.package_field.children = [package_actions]

        self.machine_field = ConfigField(data_type=MACHINE)
        package_actions.children = [self.machine_field]

        self.auth = ENABLE

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

    def check_field_name(self, tokens, no_flag, field_object, token_index):
        possible_field_names = field_object.preferred_names(tokens, token_index)
        field_name = possible_field_names[0]
        if no_flag:
            raise CommandError("NO cannot be used here")
        if len(tokens) < 3:
            raise CommandError("Incomplete command.")
        if len(possible_field_names) == 0:
            raise CommandError("Unknown %s name: %s" % (field_object.directory, tokens[tokens_index]))
        if len(possible_field_names) > 1:
            raise CommandError("Ambiguous %s name: %s" % (field_object.directory, tokens[tokens_index]))
        return field_name

    def cmd(self, tokens, no_flag):
        """
        tokens -- all of the keywords passed in the command string, parsed
        no_flag -- whether the 'no' keyword was used in the command string
        """

        package_name = self.check_field_name(tokens, no_flag, self.package_field, 1)
        action = tokens[2].lower()
        machine_name = self.check_field_name(tokens, no_flag, self.machine_field, 3)

        url = "/json/package_action/%s" % package_name
        post_data = {"machine": machine_name,
                     "action": action}

        try:
            job_name = system_state.cnm_connector.get_job(url, post_data)
            output = self.watch_job(job_name)
            return output["command_status"], output["command_output"]
        except MachineTraceback, m_err:
            libUi.process_traceback(m_err)
