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
from ConfigField import ConfigField, MACHINE, DIST, PACKAGE
from bombardier_core.static_data import OK, FAIL
from SystemStateSingleton import SystemState, ENABLE
from PackageField import PackageField, FIX, PURGE, NOT_INSTALLED, INSTALLED
import PackageActionField
from Exceptions import MachineTraceback, CommandError
import Integer
from Ssh import Ssh
from Show import Status, Summary
import Edit
import Show
system_state = SystemState()
import libUi, time, yaml

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

    def check_package_name(self, tokens, no_flag):
        possible_package_names = self.package_field.preferred_names(tokens, 1)
        package_name = possible_package_names[0]
        if no_flag:
            raise CommandError("NO cannot be used here")
        if len(possible_package_names) == 0:
            raise CommandError("Unknown package name: %s" % tokens[2])
        if len(possible_package_names) > 1:
            raise CommandError("Ambiguous package name: %s" % tokens[2])
        return package_name

    def cmd(self, tokens, no_flag):
        """
        tokens -- all of the keywords passed in the command string, parsed
        no_flag -- whether the 'no' keyword was used in the command string
        """
        if len(tokens) < 2:
            raise CommandError("Incomplete command.")
        package_name = self.check_package_name(tokens, no_flag)
        if len(tokens) == 2:
            system_state.push_prompt(["package", package_name])
            return OK,[]

        command = tokens[2].lower()

        if command == "show":
            return Show.Package().cmd(["show", "package", package_name], 0)

        if command == "edit":
            return Edit.Package().cmd(["edit", "package", package_name], 0)

        if command == "build":
            svn_password = ''
            svn_user = libUi.get_default("svn user", '')
            if svn_user:
                svn_password = libUi.pwd_input("svn password: ")
            url = "/json/package_build/%s" % package_name
            post_data = {"svn_user": svn_user,
                         "svn_password": svn_password,
                         "debug": True,
                         "prepare": True}
            try:
                job_name = system_state.cnm_connector.get_job(url, post_data)
                return system_state.cnm_connector.watch_jobs([job_name])
            except MachineTraceback, m_err:
                libUi.process_traceback(m_err)
                return FAIL,[]

