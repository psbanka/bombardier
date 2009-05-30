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

# FIXME: THIS IS BROKEN
class Status(PinshCmd.PinshCmd):
    '''Displays status information. SHOULD PULL FROM THE SERVER, BUT IS
    BROKEN'''
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "status")
        self.help_text = "status\tstatus of a host"
        #self.bom_host_field = BomHostField.BomHostField()
        #self.children = [self.bom_host_field]
        self.cmd_owner = 1

    @classmethod
    def printify(cls, input_objects):
        'pretty-print status information'
        text_list = list(input_objects)
        text_list.sort()
        output = []
        if not text_list:
            return []
        max_length = max( [ len(t) for t in text_list ] )
        columns = (system_state.termwidth - TERM_OVERCOUNT) / max_length
        for i in range(0, len(text_list), columns):
            newLine = ''
            for item in text_list[i:i+columns]:
                newLine += item.ljust(max_length+2)
            output.append(newLine)
        return output

    def cmd(self, _tokens, _no_flag):
        'prints status information, but does not work'
#        if no_flag:
#            return FAIL, []
#        if len(tokens) < 3:
#            return FAIL, ["Incomplete command."]
#        host_name = tokens[2]
#        status_file = os.path.join(system_state.server_home, "status", "%s.yml" % host_name)
#        if not os.path.isfile(status_file):
#            return FAIL, ["No status on file (%s)" % status_file]
#        #installed, broken = PackageField.getNamesFromProgress(host_name, False)
#        total_packages = PackageField.getPackageNamesFromBom(host_name)
#        missing = []
#        accounted_packages = list(installed.union(broken))
#        for item in total_packages:
#            found = False
#            for package_name in accounted_packages:
#                if package_name.startswith(item):
#                    found = True
#                    break
#            if not found:
#                missing.append(item)
#        if installed:
#            output = ["Installed:", [self.printify(list(installed))]]
#        else:
#            output = ["Installed:", [["NONE"]]]
#        if broken:
#            output += ["Broken:", [self.printify(list(broken))]]
#        else:
#            output += ["Broken:", [["NONE"]]]
#        if missing:
#            output += ["Not Installed:", [self.printify(list(missing))]]
#        else:
#            output += ["Not Installed:", [["NONE"]]]
#        return OK, output
        return OK, []


class Show(PinshCmd.PinshCmd):
    '''bomsh# show bom qa
       [OK, ['- new_tomcat', '- new_apache', '- new_database', '']]
       bomsh# show machine localhost
       [OK, ['default_user: 208048363', "include:", '- foo', '- bar', '- spam', 'ip_address: 127.0.0.1', 'platform: win32', '']]
    '''
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "show")
        self.help_text = "show\tdisplay components of the system"
        history = History()
        merged = Merged()
        machine = Machine()
        include = Include()
        #status = Status()
        package = Package()
        bom = Bom()
        self.children = [merged, machine, include, bom, history, package]
        self.cmd_owner = 1

