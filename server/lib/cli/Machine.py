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

"""This provides a top-level command for the bomsh shell. This is for
managing machines in the network, and subclasses the [PinshCmd] module
"""

__author__ =  'Peter Banka'
__version__ = '1.0'

import PinshCmd
from ConfigField import ConfigField, MACHINE, DIST
from bombardier_core.static_data import OK, FAIL
from SystemStateSingleton import SystemState, ENABLE
system_state = SystemState()
import libUi, time

class Machine(PinshCmd.PinshCmd):
    '''bomsh# machine localhost test
       [OK, ['foo']]
       bomsh# machine localhost dist test
       [OK, ['localhost updated with test']]
    '''
    def __init__(self):
        """Top-level object has a 'test' child: test the machine
        """
        PinshCmd.PinshCmd.__init__(self, "machine")
        self.help_text = "machine\tcommands that operate on a given machine"
        self.cmd_owner = 1

        self.machine_field = ConfigField(data_type=MACHINE)
        self.children = [self.machine_field]

        self.test = PinshCmd.PinshCmd("test")
        self.dist = PinshCmd.PinshCmd("dist")
        self.init = PinshCmd.PinshCmd("init")

        self.machine_field.children = [self.test, self.dist, self.init]

        self.dist_field = ConfigField(data_type=DIST)
        self.dist.children = [self.dist_field]
        self.auth = ENABLE


    def cmd(self, tokens, no_flag):
        """
        tokens -- all of the keywords passed in the command string, parsed
        no_flag -- whether the 'no' keyword was used in the command string
        """

        if no_flag:
            return FAIL, []
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        possible_machine_names = self.machine_field.preferred_names(tokens, 1)
        if len(possible_machine_names) == 0:
            return FAIL, ["Unknown machine name: %s" % tokens[2]]
        if len(possible_machine_names) > 1:
            return FAIL, ["Ambiguous machine name: %s" % tokens[2]]

        machine_name = possible_machine_names[0]

        command = tokens[2].lower()
        if command not in ['test', 'dist', 'init']:
            return FAIL, ["Unknown command: %s" % command]

        post_data = {}
        if command == 'test':
            url = "json/machine/start_test/%s" % machine_name
        elif command == 'dist':
            url = "json/machine/dist/%s" % machine_name
            post_data = {"dist": tokens[-1]}
        elif command == 'init':
            url = "json/machine/init/%s" % machine_name

        out = system_state.cnm_connector.service_yaml_request(url, post_data=post_data)
        if "traceback" in out:
            return FAIL, out["traceback"]

        job_name = out.get("job_name")
        libUi.info("Job name: %s" % job_name)

        output = {"alive": 1}
        while output.get("alive", 0):
            time.sleep(0.25)
            url = "json/job/poll/%s" % job_name
            output = system_state.cnm_connector.service_yaml_request(url)
            if "traceback" in output:
                return FAIL, output["traceback"]
            new_output = output["new_output"]
            if new_output:
                libUi.process_cnm(new_output)
        libUi.info("Joining...")
        url = "json/job/join/%s" % job_name
        output = system_state.cnm_connector.service_yaml_request(url)
        if "traceback" in output:
            return FAIL, output["traceback"]
        if output.get("status") == FAIL:
            return FAIL, []
        return OK, [output["command_output"]]
