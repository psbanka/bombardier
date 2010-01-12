#!/usr/bin/env python

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

'''A [PinshCmd] that is used for bomsh completion testing'''

import PinshCmd, Integer
import MultipleChoice
from bombardier_core.static_data import OK, FAIL
from SystemStateSingleton import SystemState
system_state = SystemState()
from Exceptions import AmbiguousCommand, UnknownCommand

class CliTest(PinshCmd.PinshCmd):
    'Used in unit testing only'
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "cli")
        self.help_text = "cli\tdo not use this"
        self.spammy = PinshCmd.PinshCmd("spammy", "spammy\tTHIS IS A TEST")
        self.spammy2  = PinshCmd.PinshCmd("spammy2", "spammy2\tTHIS IS A TEST")
        self.spotty  = PinshCmd.PinshCmd("spotty", "spotty\tTHIS IS A TEST")
        help_text = ["AA", "BB", "CC"]
        self.options = MultipleChoice.MultipleChoice(choices = help_text,
                                                     help_text = help_text)
        self.children = [self.spammy, self.spammy2, self.spotty]
        self.num = Integer.Integer(min_value = 0, max_value = 1000, name = "<int>")
        self.spammy.children = [self.num]
        self.spammy2.children = [self.num]
        self.spotty.children = [self.options]
        self.cmd_owner = 1

    def cmd(self, tokens, no_flag):
        'Used in unit testing only'
        if no_flag:
            return OK, ["YOU SAID NO"]
        if len(tokens) < 3:
            return FAIL, ["YOU NEED TO TYPE IN SOME MORE STUFF."]

        command = self.find_one_child(tokens, 1)
        return [OK, [tokens[0], command, tokens[2]]]



