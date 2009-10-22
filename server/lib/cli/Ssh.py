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

"""This provides a top-level command for the bomsh shell. Used to connect
directly from the CLI to a machine, bypassing the CNM."""

__author__ =  'Peter Banka'
__version__ = '1.0'

import PinshCmd
from ConfigField import ConfigField, MACHINE
from bombardier_core.static_data import OK, FAIL
from SystemStateSingleton import SystemState
system_state = SystemState()

import os

SSH = "/usr/bin/ssh"

class Ssh(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "ssh")
        self.help_text = "ssh\tssh to another host"
        self.config_field = ConfigField(data_type=MACHINE)
        self.children = [self.config_field]
        self.cmd_owner = 1
        self.log_command = True

    def cmd(self, tokens, no_flag):
        if no_flag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        host_name = tokens[1]

        current_dict = self.config_field.get_specific_data(tokens, 1)
        ip_address = current_dict.get("ip_address")
        default_user = current_dict.get("default_user")

        if not default_user or not ip_address:
            msg  = ["ip_address and default_user are required in the system"]
            msg += ["configuration in order to access this host."]
            return FAIL, msg
        else:
            os.system('%s %s@%s'%(SSH,default_user,ip_address))
        return OK, []
