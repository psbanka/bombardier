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
For stuff in a machine's status field'''

import PinshCmd
from bombardier_core.static_data import PARTIAL, COMPLETE, NO_MATCH
from SystemStateSingleton import SystemState
from Exceptions import MachineStatusException
system_state = SystemState()

FIX=1
PURGE=2
INSTALLED=3
NOT_INSTALLED=4

def clean_version_numbers(installed_set):
    output = set()
    for package_name in installed_set:
        fields = package_name.split('-')
        basic_package_name = '-'.join(fields[:-1])
        output.update([basic_package_name])
    return output

class PackageField(PinshCmd.PinshCmd):
    '''Some times you want to maintain package status data on a machine.'''
    def __init__(self, action_type):
        '''
        action_type -- FIXABLE or PURGABLE
        machine_name_index -- which token in the set of arguments passed
                              to preferred_names is the machine name
        package_name_index -- which token is the package_name to be completed
        '''
        PinshCmd.PinshCmd.__init__(self, "PackageField")
        self.help_text = "<PackageField>\t"\
                         "A package name that comes from machine status"
        self.action_type = action_type
        self.cmd_owner = 0
        self.machine_name_index = 1

    def possible_names(self, machine_name):
        'returns a list of all self.data_type things'
        url = "json/summary/name/%s" % (machine_name)
        data = system_state.cnm_connector.service_yaml_request(url)
        #print "PN: data:" % data
        if data.get("command_status") == "FAIL":
            raise MachineStatusException(data.get("command_output"))
        broken = set(data.get("broken", []))
        not_installed = set(data.get("not_installed", []))
        full_installed = set(data.get("installed", []))
        installed = clean_version_numbers(full_installed)
        #print "action_type: %s" % self.action_type
        if self.action_type == FIX:
            return list(broken.union(not_installed))
        if self.action_type == PURGE:
            return list(broken.union(full_installed))
        if self.action_type == INSTALLED:
            return list(installed)
        if self.action_type == NOT_INSTALLED:
            return list(not_installed)

    def get_package_names(self, complete_machine_name,
                           incomplete_package_name):
        '''someone typed in something like 'show machine localho'. Our job is
        to figure out that localhost is the object that needs to be found and
        to return the dictionary for that object.'''
        possible_names = self.possible_names(complete_machine_name)
        #print "OBJECT NAMES:",possible_names
        package_names = []
        for possible_name in possible_names:
            if possible_name.lower().startswith(incomplete_package_name.lower()):
                #print "%s starts with the same as incomplete_package_name: %s" % (possible_name, incomplete_package_name)
                package_names.append(possible_name)
        if len(package_names) == 0:
            return []
        if incomplete_package_name in package_names:
            package_names = [incomplete_package_name]
        if len(package_names) > 1:
            return package_names
        package_name = package_names[0]
        return [package_name]

    def preferred_names(self, tokens, index):
        '''Provide a list of names that the system would prefer to use, other
        than that which was typed in by the user. For example, 'machine local status fix package-1-1'
        will return 'localhost' for the machine name if strict is off,
        otherwise, it will return 'localh'.'''
        #print "PN: Tokens being passed (%s)" % tokens
        complete_machine_name = tokens[self.machine_name_index]
        incomplete_package_name = tokens[index]
        package_names = self.get_package_names(complete_machine_name,
                                               incomplete_package_name)
        return package_names

    def match(self, tokens, index):
        '''Determines if what has been typed in by the user matches a
        configuration item that the system is keeping track of.'''
        possible_matches = self.acceptable_names(tokens, index)
        if not possible_matches:
            return NO_MATCH, 1
        if len(possible_matches) > 1:
            return PARTIAL, 1
        return COMPLETE, 1
