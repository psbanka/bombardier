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

'''A [PinshCmd] field which provides for selecting from a group'''

import PinshCmd
from bombardier_core.static_data import NO_MATCH, COMPLETE, PARTIAL

class MultipleChoice(PinshCmd.PinshCmd):
    '''A Field which allows the user a simple way of choosing from a group'''
    def __init__(self, choices = None, help_text = None):
        '''
        choices -- a list of possible choices
        help_text -- a list of what each choice means
        '''
        PinshCmd.PinshCmd.__init__(self, "multiple")
        self.help_text = ""
        self.choices = choices
        if not choices:
            self.help_text = "error\t--This module needs attention--"
            return
        if len(choices) != len(help_text):
            self.help_text = "error\t--This module needs attention--"
            return
        for index in range(0, len(choices)):
            self.help_text += choices[index]+'\t'+help_text[index]+'\n'
        self.help_text = self.help_text[:-1]
        self.cmd_owner = 0

    def match(self, tokens, index):
        '''determines if the user typed in an option that the system
        recognizes'''
        possible_choices = self.preferred_names(tokens, index)
        if len(possible_choices) == 0:
            return NO_MATCH, 1
        if len(possible_choices) == 1:
            return COMPLETE, 1
        return PARTIAL, 1

    def preferred_names(self, tokens, index):
        '''provides the full name of the choice if a partial one
        was given.'''
        possible_choices = []
        if tokens[index] == '':
            return self.choices
        for choice in self.choices:
            if choice.find(tokens[index]) == 0:
                possible_choices.append(choice)
        return possible_choices
