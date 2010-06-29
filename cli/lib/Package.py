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
managing packages, and subclasses the [PinshCmd] module
"""

__author__ =  'Peter Banka'
__version__ = '1.0'

import PinshCmd
from ConfigField import ConfigField, PACKAGE
from bombardier_core.static_data import OK, FAIL
from SystemStateSingleton import SystemState, ENABLE
from Exceptions import MachineTraceback, CommandError
import Edit
import Show
system_state = SystemState()
import libUi

def setup_test():
    fresh_status_yaml = """ 
client_version: 1.00-634
core_version: 1.00-629
install-progress: {}
timestamp: 1251918530.6349609"""

    status_file = "/opt/spkg/localhost/status.yml"
    open(status_file, "w").write(fresh_status_yaml)

class Package(PinshCmd.PinshCmd):
    '''
       bomsh# package TestPackage build
       [OK, ['install OK: TestPackageType4-7', 'verify OK: TestPackageType4-7', 'Finished installing']]
    '''

    def __init__(self):
        'Top-level object'
        PinshCmd.PinshCmd.__init__(self, "package")
        self.help_text = "package\tcommands that operate on a given package"
        self.cmd_owner = 1
        self.children = []
        self.auth = ENABLE
        self.package_field = ConfigField(data_type=PACKAGE)
        self.children = [self.package_field]

        build = PinshCmd.PinshCmd("build", "build a package from source control")
        edit = PinshCmd.PinshCmd("edit", "edit the configuration for this package")
        show = PinshCmd.PinshCmd("show", "display the configuration for this package")
        self.package_field.children = [build, edit, show]

    def check_package_name(self, command_line):
        """Get a real package name from the command_line, assuming the
        package name is the second field"""
        possible_package_names = self.package_field.preferred_names(command_line, 1)
        package_name = possible_package_names[0]
        if command_line.no_flag:
            raise CommandError("NO cannot be used here")
        if len(possible_package_names) == 0:
            raise CommandError("Unknown package name: %s" % command_line[2])
        if len(possible_package_names) > 1:
            raise CommandError("Ambiguous package name: %s" % command_line[2])
        return package_name

    def cmd(self, command_line):
        """
        command_line -- all of the keywords passed in the command string, parsed
        """
        if len(command_line) < 2:
            raise CommandError("Incomplete command.")
        package_name = self.check_package_name(command_line)
        if len(command_line) == 2:
            system_state.push_prompt(["package", package_name])
            return OK, []

        command = command_line[2].lower()

        if command == "show":
            return Show.Package().cmd(["show", "package", package_name], 0)

        if command == "edit":
            return Edit.Package().cmd(["edit", "package", package_name], 0)

        if command == "build":
            svn_password = ''
            svn_user = libUi.get_default("svn user", '')
            if svn_user:
                svn_password = libUi.pwd_input("svn password: ")
            try:
                cnm = system_state.cnm_connector
                return cnm.package_build_job(package_name, svn_user,
                                             svn_password, command_line.bg_flag)
            except MachineTraceback, m_err:
                libUi.process_traceback(m_err)
                return FAIL, []

