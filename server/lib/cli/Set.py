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

'''A [PinshCmd] command which allows a user to configure the system'''


import sys, os, time, glob, md5, random
import PinshCmd, libUi, Expression
#from bombardier_server.cli.SystemStateSingleton import SystemState
from SystemStateSingleton import SystemState
system_state = SystemState()

class Set(PinshCmd.PinshCmd):
    '''bomsh# set configuration-key abc123
       [OK, ['Configuration key set.']]
       bomsh# set configuration-key foobar
       [FAIL, ['Invalid configuration key.']]
    '''
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "set", "set\tset a configuration value")
        # TOP LEVEL
        help = "configuration-key\tset the key for encrypting configuation items"
        self.configuration_key = PinshCmd.PinshCmd("configuration-key", help)
        self.configuration_key.children = [Expression.Expression("password")]
        self.children = [self.configuration_key]
        self.level = 0
        self.cmd_owner = 1

    def cmd(self, tokens, no_flag):
        if len(tokens) == 2:
            configuration_key = libUi.pwd_input("Enter CI decryption password: ")
        else:
            configuration_key = tokens[2]
        url = "/json/dispatcher/set_password/"
        post_data = {"password": configuration_key}
        connector = system_state.cnm_connector
        output = connector.service_yaml_request(url, post_data=post_data)
        return output["command_status"], output["command_output"]


