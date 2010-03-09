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

'''A [PinshCmd] command which allows a user to create a new data item
on the server'''

import yaml, tempfile, os, libUi
import PinshCmd, ConfigField
from bombardier_core.static_data import OK, FAIL
from SystemStateSingleton import SystemState
system_state = SystemState()

class CreateType(PinshCmd.PinshCmd):
    '''A thing that can be created.'''
    def __init__(self, name, help_text):
        PinshCmd.PinshCmd.__init__(self, name, help_text)
        self.config_field = None
        self.cmd_owner = 1

    def cmd(self, tokens, no_flag):
        '''Create a thing that the user is interested in'''
        if no_flag:
            return FAIL, []
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]

        conf_str = self.config_field.get_default_data()
        #conf_str = yaml.dump(current_dict, default_flow_style=False)
        fd,fn = tempfile.mkstemp(suffix=".yml", text=True)
        fh = os.fdopen(fd, 'w+b')
        fh.write(conf_str)
        fh.close()
        os.system("vim %s" % fn)
        post_data = yaml.load(open(fn).read())
        submit = libUi.ask_yes_no("Commit changes to server", libUi.YES)
        if submit:
            status, output = self.config_field.post_specific_data(tokens, 2, post_data)
            os.unlink(fn)
            return OK, [output]
        else:
            msg = "Discarded changes. Edits can be found here: %s" % fn
            return OK, [msg]

class Machine(CreateType):
    'Create a default machine configuration data from the server'
    def __init__(self):
        CreateType.__init__(self, "machine", "machine\tcreate a new machine configuration")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.MACHINE, new=True)
        self.children = [self.config_field]

class Include(CreateType):
    'Create an include file on the server'
    def __init__(self):
        CreateType.__init__(self, "include", "include\tcreate a shared include file")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.INCLUDE, new=True)
        self.children = [self.config_field]

class User(CreateType):
    'Create a package file'
    def __init__(self):
        CreateType.__init__(self, "user", "user\tcreate a new user to log in to Bombardier")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.USER, new=True)
        self.children = [self.config_field]

class Package(CreateType):
    'Create a package file'
    def __init__(self):
        CreateType.__init__(self, "package", "package\tcreate new package metadata")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.PACKAGE, new=True)
        self.children = [self.config_field]

class Bom(CreateType):
    'create new bill-of-materials ("bom") file from the server'
    def __init__(self):
        CreateType.__init__(self, "bom", "bom\tcreate a bill of materials")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.BOM, new=True)
        self.children = [self.config_field]

class Create(PinshCmd.PinshCmd):
    '''Create a file'''
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "create")
        self.help_text = "create\tcreate a new system component"
        machine = Machine()
        include = Include()
        package = Package()
        user = User()
        bom = Bom()
        self.children = [machine, include, bom, package, user]
        self.cmd_owner = 1
