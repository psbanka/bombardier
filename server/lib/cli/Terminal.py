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

'''A [PinshCmd] that is used to keep track of terminal settings.'''

import PinshCmd, Integer
import MultipleChoice
from bombardier_core.static_data import NO_COLOR, LIGHT, DARK, OK, FAIL, COMPLETE
from SystemStateSingleton import SystemState
system_state = SystemState()

class Terminal(PinshCmd.PinshCmd):
    'If you want to set your terminal, use this'
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "terminal")
        self.help_text = "terminal\tchange settings for the current terminal"
        self.length = PinshCmd.PinshCmd("length", "length\tnumber of lines the terminal can display")
        self.width  = PinshCmd.PinshCmd("width", "width\tnumber of columns the terminal can disply")
        self.color  = PinshCmd.PinshCmd("color", "color\tset terminal color")
        help_text = ["black and white", "colors for light background", "colors for dark background"]
        color_options = MultipleChoice.MultipleChoice(choices = ["none", "light", "dark"],
                                                     help_text = help_text)
        self.children = [self.length, self.width, self.color]
        self.num = Integer.Integer(min_value = 0, max_value = 1000, name = "<int>")
        self.length.children = [self.num]
        self.width.children = [self.num]
        self.color.children = [color_options]
        self.cmd_owner = 1

    def cmd(self, tokens, no_flag):
        'Someone wants to execute a command to change their terminal'
        if no_flag:
            if tokens[1].lower().startswith('c'):
                system_state.termcolor = NO_COLOR
                return OK, ["Terminal set to black and white."]
            else:
                return FAIL, ["'No' not applicable in this context"]
        if len(tokens) < 3:
            msg  = ["terminal length: %d" % system_state.termlen]
            msg += ["terminal width: %d" % system_state.termwidth]
            if system_state.termcolor != NO_COLOR:
                if system_state.termcolor == LIGHT:
                    msg += ["terminal color-scheme: light"]
                if system_state.termcolor == DARK:
                    msg += ["terminal color-scheme: dark"]
            return OK, msg
        value = tokens[2]
        if tokens[1].lower().startswith('c'):
            if value.lower().startswith('n'):
                system_state.termcolor = NO_COLOR
                return OK, ["Terminal set to black and white."]
            elif value.lower().startswith('l'):
                system_state.termcolor = LIGHT
                return OK, ["Terminal set to light background."]
            elif value.lower().startswith('d'):
                system_state.termcolor = DARK
                return OK, ["Terminal set to dark background."]
            else:
                return FAIL, ["Unknown terminal color."]

        if self.num.match([value], 0) != (COMPLETE, 1):
            return FAIL, ["Please choose a value between 0 and 1000"]

        if tokens[1].lower().startswith('l'):
            system_state.termlen = int(value)
        elif tokens[1].lower().startswith('w'):
            system_state.termwidth = int(value)
        return OK, []
