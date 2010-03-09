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

'''A [PinshCmd] command which allows a user to edit data on the server'''


import yaml, tempfile, os, libUi
import PinshCmd, ConfigField
from bombardier_core.static_data import OK, FAIL
from SystemStateSingleton import SystemState
system_state = SystemState()

class EditType(PinshCmd.PinshCmd):
    '''A thing that can be edited.'''
    def __init__(self, name, help_text):
        PinshCmd.PinshCmd.__init__(self, name, help_text)
        self.config_field = None
        self.cmd_owner = 1

    def cmd(self, tokens, no_flag):
        '''Edit the thing that the user is interested in'''
        if no_flag:
            return FAIL, []
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        current_dict = self.config_field.get_specific_data(tokens, 2)
        conf_str = yaml.dump(current_dict, default_flow_style=False)
        fd,fn = tempfile.mkstemp(suffix=".yml", text=True)
        fh = os.fdopen(fd, 'w+b')
        fh.write(conf_str)
        fh.close()
        os.system("%s %s" % (system_state.editor, fn))
        post_data = yaml.load(open(fn).read())
        if post_data == current_dict:
            return FAIL, ['', "No changes made. Not submitting to server."]
        submit = libUi.ask_yes_no("Commit changes to server", libUi.YES)
        if submit:
            status, output = self.config_field.post_specific_data(tokens, 2, post_data)
            os.unlink(fn)
            return OK, [output]
        else:
            msg = "Discarded changes. Edits can be found here: %s" % fn
            return OK, [msg]

class Machine(EditType):
    'Edit basic machine configuration data from the server'
    def __init__(self):
        EditType.__init__(self, "machine", "machine\tedit the configuration for one machine")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.MACHINE)
        self.children = [self.config_field]

class Include(EditType):
    'Displays include information from the server'
    def __init__(self):
        EditType.__init__(self, "include", "include\tedit a shared include file")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.INCLUDE)
        self.children = [self.config_field]

class Package(EditType):
    'Displays package data'
    def __init__(self):
        EditType.__init__(self, "package", "package\tedit package metadata")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.PACKAGE)
        self.children = [self.config_field]

class User(EditType):
    'Edit user data from the server'
    def __init__(self):
        EditType.__init__(self, "user", "user\tedit a user")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.USER)
        self.children = [self.config_field]

class Bom(EditType):
    'Displays bill-of-materials ("bom") data from the server'
    def __init__(self):
        EditType.__init__(self, "bom", "bom\tedit a bill of materials")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.BOM)
        self.children = [self.config_field]

class Edit(PinshCmd.PinshCmd):
    '''Edit a file'''
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "edit")
        self.help_text = "edit\tmodify components of the system"
        machine = Machine()
        include = Include()
        package = Package()
        user = User()
        bom = Bom()
        self.children = [machine, include, bom, package, user]
        self.cmd_owner = 1

