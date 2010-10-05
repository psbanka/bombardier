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

class RestoreTargetField(PinshCmd.PinshCmd):
    "This class provides command-line completion for restore targets"
    def __init__(self, name = "RestoreTargetField"):
        'Just sets up the basics'
        PinshCmd.PinshCmd.__init__(self, name, token_delimeter = '')
        self.help_text = "<RestoreTargetField>\t"\
                         "a timestamp for a restore action"
        self.cmd_owner = 0

    def preferred_names(self, command_line, index):
        '''Provide a list of names that the system would prefer to use, other
        than that which was typed in by the user. For example, 'sho mach localh'
        will return 'localhost' for the machine name if strict is off,
        otherwise, it will return 'localh'.

        '''
        cnm = system_state.cnm_connector
        machine_name = command_line[index-3]
        package_name = command_line[index-1]

        restore_targets = cnm.get_restore_targets(machine_name, package_name)
        possible_targets = []
        for target in restore_targets.get("targets", []):
            if target.lower().startswith(command_line[index].lower()):
                possible_targets.append(target)
        return possible_targets

    def match(self, command_line, index):
        '''Determines if what has been typed in by the user matches a
        possible package action'''
        possible_matches = self.acceptable_names(command_line, index)
        if not possible_matches:
            return NO_MATCH, 1
        if len(possible_matches) > 1:
            return PARTIAL, 1
        return COMPLETE, 1
