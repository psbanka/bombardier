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

'''A [PinshCmd] object that provides a field with automated completion.
Will be a basic package action or one that the package exposes'''


import PinshCmd
from bombardier_core.static_data import PARTIAL, COMPLETE, NO_MATCH
from SystemStateSingleton import SystemState
import CnmConnector
system_state = SystemState()

class PackageActionField(PinshCmd.PinshCmd):
    '''This class can provide command-line completion for actions if it
    exposes methods'''
    def __init__(self, name = "packageActionField"):
        'Just sets up the basics'
        PinshCmd.PinshCmd.__init__(self, name, token_delimeter = '')
        self.help_text = "<packageActionField>\t"\
                         "an action that can be taken on a package"
        self.cmd_owner = 0

    def get_help_text(self, command_line):
        "Produce context-sensitive help text"
        package_name = command_line[-2]
        package_data = self.get_package_data(package_name)
        actions = package_data.get("executables")
        help_lines = []
        if type(actions) == type([]):
            for action in actions:
                if action.lower().startswith(command_line[-1].lower()):
                    help_lines.append("%s\tRemote executable" % action)
        else:
            for action in actions.keys():
                if action.lower().startswith(command_line[-1].lower()):
                    description = actions[action]
                    help_lines.append("%s\t%s" % (action, description))
        return '\n'.join(help_lines)

    def get_package_name(self, package_name):
        'returns a list of all packages that might match'
        url = CnmConnector.PACKAGE_SEARCH_PATH % package_name
        data = system_state.cnm_connector.service_yaml_request(url)
        package_list = [ x.get("fields").get("name") for x in data ]
        return package_list

    def get_package_data(self, package_name):
        'returns a list of all configuration objects of this type'
        url = CnmConnector.PACKAGE_NAME_PATH % package_name
        data = system_state.cnm_connector.service_yaml_request(url)
        return data

    def preferred_names(self, command_line, index):
        '''Provide a list of names that the system would prefer to use, other
        than that which was typed in by the user. For example, 'sho mach localh'
        will return 'localhost' for the machine name if strict is off,
        otherwise, it will return 'localh'.

        '''
        package_name = command_line[index-1]
        package_data = self.get_package_data(package_name)
        actions = package_data.get("executables")
        possible_actions = []
        for action in actions:
            if action.lower().startswith(command_line[index].lower()):
                possible_actions.append(action)
        #print "possible actions:",possible_actions
        return possible_actions

    def match(self, command_line, index):
        '''Determines if what has been typed in by the user matches a
        possible package action'''
        possible_matches = self.acceptable_names(command_line, index)
        if not possible_matches:
            return NO_MATCH, 1
        if len(possible_matches) > 1:
            return PARTIAL, 1
        return COMPLETE, 1
