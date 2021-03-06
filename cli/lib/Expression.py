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

'''A [PinshCmd] command which allows a user to enter free text as an argument'''


import PinshCmd
from bombardier_core.static_data import PARTIAL

class Expression(PinshCmd.PinshCmd):
    "A PinshCmd that can look like anything"
    def __init__(self, name = "expression"):
        PinshCmd.PinshCmd.__init__(self, name)
        self.help_text = name+"\tan arbitrary expression"
        self.level = 99
        self.cmd_owner = 0

    def match(self, command_line, index):
        "PinshCmd interface"
        #print "exp-match %s\n" % ( len(command_line) - index )
        return PARTIAL, len(command_line) - index

    def preferred_names(self, command_line, _index):
        "PinshCmd interface"
        #print command_line[-1],'\n'
        return [command_line[-1], command_line[-1]]
