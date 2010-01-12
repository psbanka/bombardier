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
import yaml
from bombardier_core.static_data import OK, FAIL
from bombardier_core.static_data import PARTIAL, COMPLETE, NO_MATCH
from SystemStateSingleton import SystemState, ENABLE
from Exceptions import CommandError, ServerException
from Expression import Expression
system_state = SystemState()

class JobNameField(PinshCmd.PinshCmd):
    def __init__(self):
        "Allows a guy to find a job"
        PinshCmd.PinshCmd.__init__(self, "<job_name>")
        self.help_text = "<job_name>\tname of an active job"
        self.cmd_owner = 0

    def get_jobs(self, server_output):
        data = yaml.load('\n'.join(server_output))
        active_jobs = data.get("active_jobs", [])
        return active_jobs

    def preferred_names(self, tokens, index):
        '''Provide a list of names that the system would prefer to use, other
        than that which was typed in by the user. For example, 'sho mach localh'
        will return 'localhost' for the machine name if strict is off,
        otherwise, it will return 'localh'.

        '''
        post_data = {}
        status, data = system_state.cnm_connector.dispatcher_control("status", post_data)
        active_jobs = self.get_jobs(data)
        tokens[index] = tokens[index].replace('"', '')
        if len(active_jobs) == 0:
            return []
        possible_names = []
        for job_name in active_jobs:
            if job_name.startswith(tokens[index]):
                possible_names.append(job_name)
        return possible_names

    def match(self, tokens, index):
        '''Determines if what has been typed in by the user matches a
        configuration item that the system is keeping track of.'''
        possible_matches = self.acceptable_names(tokens, index)
        if not possible_matches:
            return NO_MATCH, 1
        if len(possible_matches) > 1:
            return PARTIAL, 1
        return COMPLETE, 1


class Job(PinshCmd.PinshCmd):
    '''The idea is this: bomsh# job view foo
       would produce this: [OK, ['Job stopped']]
       But it can't be tested easily from this test harness.
    '''

    def __init__(self):
        """Top-level object has a multiple choice child
        """
        PinshCmd.PinshCmd.__init__(self, "job")
        self.help_text = "job\tjob control commands and status"
        self.cmd_owner = 1

        view = PinshCmd.PinshCmd("view", "view\tview an active -running job")
        kill = PinshCmd.PinshCmd("kill", "kill\tkill a job that is active")
        job_name_field = JobNameField()

        self.children = [view, kill]
        view.children = [job_name_field]
        kill.children = [job_name_field]
        self.auth = ENABLE

    def cmd(self, tokens, no_flag):
        """
        tokens -- all of the keywords passed in the command string, parsed
        no_flag -- whether the 'no' keyword was used in the command string
        """
        if len(tokens) < 3:
            raise CommandError("Incomplete command")
        job_name = tokens[2]
        if tokens[1] == "kill":
            post_data = {"timeout": "10"}
            url = "/json/job/kill/%s" % job_name
            output = system_state.cnm_connector.service_yaml_request(url, post_data=post_data)
            return output["command_status"], output["command_output"]
        elif tokens[1] == "view":
            output = system_state.cnm_connector.watch_job(job_name)
            return output["command_status"], output["command_output"]
        raise CommandError("Unknown command: %s" % tokens[1])
            
