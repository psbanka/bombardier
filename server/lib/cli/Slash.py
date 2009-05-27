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

'''A special [PinshCmd] command which is the root of all other commands. Slash
Does not do anything itself, but merely has other PinshCmd objects which are
its children'''

import traceback, StringIO
import exceptions
import sys
import PinshCmd, libUi
from SystemStateSingleton import SystemState
system_state = SystemState()
from bombardier_core.static_data import FAIL, OK


class Slash(PinshCmd.PinshCmd):
    '''Root-level PinshCmd object, has no name.'''
    def __init__(self, children):
        PinshCmd.PinshCmd.__init__(self, "")
        self.children = children
        self.cmd_owner = 1
        self.help_text = ''
        self.fp_out = sys.stdout #FIXME: Push into system_state
        self.fp_err = sys.stderr

    def set_output(self, fp_out):
        'set the output file handler'
        self.fp_out = fp_out

    def set_err(self, fp_err):
        'set the error file handler'
        self.fp_err = fp_err

    def process_command(self, command):
        'When somebody hits return in the shell, this method handles it.'
        try:
            no_flag, help_flag, tokens, comment = libUi.process_input(command)
            if help_flag: # Process the [?] key first
                self.find_help(tokens, 0)
                return None
            if len(tokens) == 0:
                # somebody just pressed return for no reason
                return OK, []
            else:
                status, output = self.run(tokens, no_flag, self)
                #if comment:
                    #makeComment(comment) # MISSING
            libUi.user_output(output, status, self.fp_out, self.fp_err)
            return status, output
        except exceptions.SystemExit:
            #if system_state.comment_commands:
                #makeComment()
            sys.exit(0)
        except Exception, err:
            self.fp_err.write( " %%%% Error detected in %s (%s)." % (command, err))
            tb_str = StringIO.StringIO()
            traceback.print_exc(file=tb_str)
            tb_str.seek(0)
            data = tb_str.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            self.fp_err.write(ermsg)
            self.fp_err.write("\n")
        return FAIL, ["process_command Excepted"]



