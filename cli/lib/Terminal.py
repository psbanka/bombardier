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
from bombardier_core.static_data import NO_COLOR, LIGHT, DARK
from bombardier_core.static_data import OK, FAIL, COMPLETE
from bombardier_core.static_data import LOG_LEVEL_LOOKUP, LOG_REVERSE_LOOKUP
from SystemStateSingleton import SystemState
from Exceptions import CommandError
system_state = SystemState()

class Terminal(PinshCmd.PinshCmd):
    'If you want to set your terminal, use this'
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "terminal")
        self.help_text = "configure the terminal"

        log_level = PinshCmd.PinshCmd("log-level")
        log_level.help_text = "set degree of logging to display to terminal"
        length = PinshCmd.PinshCmd("length")
        length.help_text = "number of lines the terminal can display"
        width  = PinshCmd.PinshCmd("width")
        width.help_text = "number of columns the terminal can disply"
        color  = PinshCmd.PinshCmd("color", "set terminal color")
        self.children = [log_level, length, width, color]

        log_choices = ["debug", "info", "warning", "error", "critical"]
        choice_help = ["Maximum debugging", "Normal amounts of logs",
                       "Reduced logging", "Much reduced logging",
                       "Minimum logging"]
        choice_field = MultipleChoice.MultipleChoice(choices=log_choices,
                                                     help_text=choice_help)
        log_level.children = [choice_field]

        color_choices = ["none", "light", "dark"]
        log_help = ["black and white", "colors for light background",
                     "colors for dark background"]
        color_options = MultipleChoice.MultipleChoice(choices=color_choices,
                                                      help_text = log_help)
        color.children = [color_options]

        self.num = Integer.Integer(min_value = 0, max_value = 1000,
                                   name = "<int>")
        length.children = [self.num]
        width.children = [self.num]
        self.cmd_owner = 1

    def _set_color(self, value):
        "Set terminal color"
        status = OK
        output = []
        if value == "none":
            system_state.termcolor = NO_COLOR
            output = ["Terminal set to black and white."]
        elif value == "light":
            system_state.termcolor = LIGHT
            output = ["Terminal set to light background."]
        elif value == "dark":
            system_state.termcolor = DARK
            output = ["Terminal set to dark background."]
        else:
            status = FAIL
            output = ["Unknown terminal color."]
        return status, output

    def _set_size(self, setting, value):
        "Set length or width of termial"
        status = OK
        output = []
        if self.num.match([value], 0) != (COMPLETE, 1):
            status = FAIL
            output = ["Please choose a value between 0 and 1000"]
        else:
            if setting == "length":
                system_state.termlen = int(value)
            elif setting.startswith('w'):
                system_state.termwidth = int(value)
            status = OK
            output = ["Terminal set."]
        return status, output

    def cmd(self, command_line):
        'Someone wants to execute a command to change their terminal'
        setting = command_line[1].lower()

        status = FAIL
        output = []

        if command_line.no_flag:
            if setting == "color":
                system_state.termcolor = NO_COLOR
                status = OK
                output = ["Terminal set to black and white."]
            else:
                status = FAIL
                output = ["'No' not applicable in this context"]
        else:
            if len(command_line) < 3:
                output  = ["terminal length: %d" % system_state.termlen]
                output += ["terminal width: %d" % system_state.termwidth]
                if system_state.termcolor != NO_COLOR:
                    if system_state.termcolor == LIGHT:
                        output += ["terminal color-scheme: light"]
                    if system_state.termcolor == DARK:
                        output += ["terminal color-scheme: dark"]
                output += ["logging level: %s" \
                            % LOG_REVERSE_LOOKUP[system_state.log_level]]
                status = OK
            else:
                value = command_line[2].lower()

                if setting == "log-level":
                    new_log_level = LOG_LEVEL_LOOKUP[value.upper()]
                    system_state.log_level = new_log_level
                    status = OK
                    output = ["Logging output set to %s" % command_line[2]]

                elif setting == "color":
                    status, output = self._set_color(value)
                
                elif setting in ["length", "width"]:
                    status, output = self._set_size(setting, value)

                else:
                    raise CommandError("Unknown command")

            return status, output
