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

""" PinshCmd for dispatcher service control and status
"""

__author__ =  'Peter Banka, Shawn Sherwood'
__version__ = '1.0'

import PinshCmd
from bombardier_core.static_data import OK, FAIL
from SystemStateSingleton import SystemState, ENABLE
from Exceptions import CommandError, ServerException
from Expression import Expression
system_state = SystemState()

class Dispatcher(PinshCmd.PinshCmd):
    '''bomsh# dispatcher status
       [OK, ['uptime: ==UNKNOWN==', 'active_jobs: []']]
       bomsh# dispatcher stop
       [OK, ['Dispatcher stopped']]
       bomsh# dispatcher stop
       [OK, ['Dispatcher stopped']]
       bomsh# dispatcher start
       [OK, ['Dispatcher started']]
       bomsh# dispatcher start
       [OK, ['Dispatcher already started']]
    '''

    def __init__(self):
        """Top-level object has a multiple choice child
        """
        PinshCmd.PinshCmd.__init__(self, "dispatcher")
        self.help_text = "dispatcher\tdispatcher control commands and status"
        self.cmd_owner = 1

        start = PinshCmd.PinshCmd("start", "start\tstart the dispatcher")
        stop = PinshCmd.PinshCmd("stop", "stop\tstop the dispatcher")
        status = PinshCmd.PinshCmd("status", "status\tstatus of the dispatcher")
        attach = PinshCmd.PinshCmd("attach", "attach\tattach to an already-running dispatcher")
        dispatcher_uri = Expression("uri")
        attach.children = [dispatcher_uri]

        self.children = [start, stop, status, attach]
        self.auth = ENABLE

    def cmd(self, tokens, no_flag):
        """
        tokens -- all of the keywords passed in the command string, parsed
        no_flag -- whether the 'no' keyword was used in the command string
        """
        action = tokens[1].lower()
        post_data = {}
        if action == "attach":
            if len(tokens) < 3:
                raise CommandError("Incomplete command")
            post_data["uri"]= tokens[2]
        return system_state.cnm_connector.dispatcher_control(action, post_data)

