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
managing users, and subclasses the [PinshCmd] module
"""

__author__ =  'Peter Banka'
__version__ = '1.0'

import PinshCmd
from ConfigField import ConfigField, MACHINE, DIST, PACKAGE, USER
from bombardier_core.static_data import OK, FAIL
from SystemStateSingleton import SystemState, ENABLE
from PackageField import PackageField, FIX, PURGE, NOT_INSTALLED, INSTALLED
import PackageActionField
from Exceptions import MachineTraceback, CommandError
import Integer
from Ssh import Ssh
from Show import Status, Summary
import Edit
system_state = SystemState()
import libUi, time, yaml

class User(PinshCmd.PinshCmd):

    def __init__(self):
        'Top-level object'
        PinshCmd.PinshCmd.__init__(self, "user")
        self.help_text = "user\tcommands for managing users"
        self.cmd_owner = 1
        self.children = []
        self.auth = ENABLE
        self.user_field = ConfigField(data_type=USER)
        self.children = [self.user_field]

        delete = PinshCmd.PinshCmd("delete", "delete\tdelete this user (can't be yourself)")
        edit = PinshCmd.PinshCmd("edit", "edit\tedit the configuration for this user")
        set_password = PinshCmd.PinshCmd("set-password", "set-password\tset the password for this user")
        self.user_field.children = [delete, edit, set_password]

    def check_user_name(self, tokens, no_flag):
        possible_user_names = self.user_field.preferred_names(tokens, 1)
        user_name = possible_user_names[0]
        if no_flag:
            raise CommandError("NO cannot be used here")
        if len(possible_user_names) == 0:
            raise CommandError("Unknown user name: %s" % tokens[2])
        if len(possible_user_names) > 1:
            raise CommandError("Ambiguous user name: %s" % tokens[2])
        return user_name

    def cmd(self, tokens, no_flag):
        """
        tokens -- all of the keywords passed in the command string, parsed
        no_flag -- whether the 'no' keyword was used in the command string
        """
        if len(tokens) < 2:
            raise CommandError("Incomplete command.")
        user_name = self.check_user_name(tokens, no_flag)
        if len(tokens) == 2:
            system_state.push_prompt(["user", user_name])
            return OK,[]

        command = tokens[2].lower()

        if command == "edit":
            return Edit.User().cmd(["edit", "user", user_name], 0)

        elif command == "set-password":
            password = libUi.pwd_input("password for %s: " % user_name)
            url = "/json/user/name/%s" % user_name
            post_data = { "password": password }
            try:
                output = system_state.cnm_connector.service_yaml_request(url, post_data=post_data)
                return output["command_status"], output["command_output"]
            except MachineTraceback, m_err:
                libUi.process_traceback(m_err)
                return FAIL,[]

        elif command == "delete":
            if system_state.username == user_name:
                return FAIL, ["Cannot delete your own user object."]
            url = "/json/user/name/%s" % user_name
            prompt = 'Delete user "%s" -- are you sure' % user_name
            if libUi.ask_yes_no(prompt, libUi.NO) == libUi.NO:
                return FAIL, ["Aborted"]
            try:
                output = system_state.cnm_connector.service_yaml_request(url, delete=True)
                return output["command_status"], output["command_output"]
            except MachineTraceback, m_err:
                libUi.process_traceback(m_err)
                return FAIL,[]

            return Edit.User().cmd(["edit", "user", user_name], 0)

