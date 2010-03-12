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

'''A [PinshCmd] command which allows a user to display data'''


import readline
import yaml
import PinshCmd, ConfigField, Integer
from bombardier_core.static_data import OK, FAIL
from SystemStateSingleton import SystemState
system_state = SystemState()

TERM_OVERCOUNT = 8 # For some reason, the term width seems too long...

class ShowType(PinshCmd.PinshCmd):
    '''A thing that can be shown.'''
    def __init__(self, name, help_text):
        PinshCmd.PinshCmd.__init__(self, name, help_text)
        self.config_field = None
        self.cmd_owner = 1

    def cmd(self, tokens, no_flag):
        '''Show the thing that the user is interested in'''
        if no_flag:
            return FAIL, []
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        current_dict = self.config_field.get_specific_data(tokens, 2)
        return OK, yaml.dump(current_dict, default_flow_style=False).split('\n')

class Merged(ShowType):
    '''Merged objects take all the configuration for a machine and show it to
    the user, folding in include and bom data'''
    def __init__(self):
        ShowType.__init__(self, "merged", "merged\tdisplay a merged configuration")
        self.config_field = ConfigField.ConfigField()
        self.children = [self.config_field]

class Status(ShowType):
    'Displays status of a machine'
    def __init__(self):
        ShowType.__init__(self, "status", "status\tshows installation status")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.STATUS)
        self.children = [self.config_field]

class Summary(ShowType):
    'Displays status of a machine'
    def __init__(self):
        ShowType.__init__(self, "summary", "summary\tshows installation status summary")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.SUMMARY)
        self.children = [self.config_field]

class Dist(ShowType):
    'Displays basic machine configuration data from the server'
    def __init__(self):
        ShowType.__init__(self, "dist", "dist\tshow a python distribution tar-ball")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.DIST)
        self.children = [self.config_field]

class Machine(ShowType):
    'Displays basic machine configuration data from the server'
    def __init__(self):
        ShowType.__init__(self, "machine", "machine\tshow the configuration for one machine")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.MACHINE)
        self.children = [self.config_field]

class Include(ShowType):
    'Displays include information from the server'
    def __init__(self):
        ShowType.__init__(self, "include", "include\tshow a shared include file")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.INCLUDE)
        self.children = [self.config_field]

class Bom(ShowType):
    'Displays bill-of-materials ("bom") data from the server'
    def __init__(self):
        ShowType.__init__(self, "bom", "bom\tshow a bill of materials")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.BOM)
        self.children = [self.config_field]

class Package(ShowType):
    'Displays information about a package from the server'
    def __init__(self):
        ShowType.__init__(self, "package", "package\tdisplay information about a given package")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.PACKAGE)
        self.children = [self.config_field]

class User(ShowType):
    'Displays information about a user from the server'
    def __init__(self):
        ShowType.__init__(self, "user", "user\tdisplay information about a given user")
        self.config_field = ConfigField.ConfigField(data_type=ConfigField.USER)
        self.children = [self.config_field]

class History(PinshCmd.PinshCmd):
    'Displays command-line history (NOT from the server)'
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "history")
        self.help_text = "history\tdisplay the history of commands"
        self.integer  = Integer.Integer(min_value=1, max_value=1000)
        self.children = [self.integer]
        self.cmd_owner = 1

    def cmd(self, tokens, _no_flag):
        "Shows the history for this user's bomsh session and before"
        if len(tokens) == 2 or tokens[-1].strip()=='':
            number = 20
        else:
            try:
                number = int(tokens[2])
            except ValueError:
                return FAIL, ["%s is not a number." % tokens[2]]
        hlen = readline.get_current_history_length()
        if hlen < number:
            number = hlen
        output = []
        for i in range(hlen-number, hlen):
            output.append("%4d\t%s" % (i, readline.get_history_item(i)))
        return OK, output


class Show(PinshCmd.PinshCmd):
#       bomsh# show machine localhost
#       [OK, ['default_user: root', 'ip_address: 127.0.0.1', 'packages:', '- TestPackageType4', 'platform: linux', 'test:', '  directory: /tmp/testthing', '  value: hello_kitty', '']]
#       bomsh# show summary localhost
#       [OK, ['broken: []', 'installed:', '- TestPackageType4-7', 'not_installed: []', 'status: 0', '']]
    '''bomsh# show bom qa
       [OK, ['- new_tomcat', '- new_apache', '- new_database', '']]
       bomsh# show dist test
       [OK, ['fields:', 'name: test', '']]
    '''
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "show")
        self.help_text = "show\tdisplay components of the system"
        history = History()
        merged = Merged()
        machine = Machine()
        include = Include()
        status = Status()
        package = Package()
        user    = User()
        dist    = Dist()
        bom = Bom()
        summary = Summary()
        self.children = [merged, machine, include, bom, history, package, user, dist, status, summary]
        self.cmd_owner = 1

