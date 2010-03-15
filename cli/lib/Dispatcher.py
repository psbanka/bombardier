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
from Exceptions import CommandError, UnexpectedDataException
import Expression
import libUi
system_state = SystemState()

from JobNameField import JobNameField

class JobCommand(PinshCmd.PinshCmd):
    '''The idea is this: bomsh# dispatcher job view foo
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
        connector = system_state.cnm_connector
        if len(tokens) < 4:
            raise CommandError("Incomplete command")
        job_name = tokens[3]
        if tokens[2] == "kill":
            post_data = {"post_data": "hello"}
            url = "/json/job/kill/%s" % job_name
            output = connector.service_yaml_request(url, post_data=post_data)
            return output["command_status"], output["command_output"]
        elif tokens[2] == "view":
            status, output = system_state.cnm_connector.watch_jobs([job_name])
            return status, output
            #return output["command_status"], output["command_output"]
        raise CommandError("Unknown command: %s" % tokens[1])
            
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

        start = PinshCmd.PinshCmd("start", "start the dispatcher")
        stop = PinshCmd.PinshCmd("stop", "stop the dispatcher")
        status = PinshCmd.PinshCmd("status", "status of the dispatcher")
        attach = PinshCmd.PinshCmd("attach")
        attach.help_text = "attach to an already-running dispatcher"
        restart = PinshCmd.PinshCmd("restart", "restart the dispatcher")
        job = JobCommand()
        key = PinshCmd.PinshCmd("configuration-key")
        key.help_text = "set the key for encrypting configuation items"
        server_home = PinshCmd.PinshCmd("server-home")
        server_home.help_text = "set the root directory for configuration"
        server_home.children = [Expression.Expression("path")]
        self.children = [start, stop, status, attach, restart, job,
                         key, server_home]

        dispatcher_uri = Expression.Expression("uri")
        attach.children = [dispatcher_uri]
        key.children = [Expression.Expression("password")]

        self.auth = ENABLE

    def cmd(self, tokens, no_flag):
        """
        tokens -- all of the keywords passed in the command string, parsed
        no_flag -- whether the 'no' keyword was used in the command string
        """
        connector = system_state.cnm_connector
        action = tokens[1].lower()
        post_data = {}

        if action == "server-home":
            if len(tokens) != 3:
                raise CommandError("Incomplete command.")
            server_home = tokens[2]
            try:
                connector.sync_server_home(server_home)
            except UnexpectedDataException, ude:
                return FAIL, [str(ude)] 
            return OK, ["Server home set to %s" % server_home]

        elif action in ["configuration-key", "restart"]:
            cmd_output = []
            if len(tokens) == 2:
                prompt = "Enter CI decryption password: "
                configuration_key = libUi.pwd_input(prompt)
            else:
                configuration_key = tokens[2]
            if action == "restart":
                _status, output = connector.dispatcher_control("stop",
                                                                   post_data)
                cmd_output.append(output)
                status, output = connector.dispatcher_control("start",
                                                                  post_data)
                cmd_output.append(output)
                if status != OK:
                    return FAIL, output

            url = "/json/dispatcher/set-password"
            post_data = {"password": configuration_key}
            cmd_output_dict = connector.service_yaml_request(url,
                                                            post_data=post_data)
            cmd_output.append(cmd_output_dict["command_output"])
            return cmd_output_dict["command_status"], cmd_output
            
        elif action == "attach":
            if len(tokens) < 3:
                raise CommandError("Incomplete command")
            post_data["uri"] = tokens[2]

        elif action in ["start", "stop", "status"]:
            return connector.dispatcher_control(action, post_data)

        raise CommandError("Unknown command")

