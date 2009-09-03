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
import PinshCmd, libUi, ConfigField, Expression, Integer, MultipleChoice, Variable
from bombardier_core.libCipher import change_pass, DecryptionException, InvalidData, VALID_CHARS
from bombardier_server.cli.SystemStateSingleton import SystemState, ENABLE, USER, F0
system_state = SystemState()

import yaml, syck

class UnsupportedTypeException(Exception):
    def __init__(self, destObject):
        Exception.__init__(self)
        self.badType = type(destObject)
    def __str__(self):
        return "Cannot support conversion to type %s." % (self.badType)
    def __repr__(self):
        return "Cannot support conversion to type %s." % (self.badType)

def make_same_type(current_value, new_value):
    if len(current_value) == 0:
        return new_value
    if type(current_value[0]) == type(new_value):
        return new_value
    if type(current_value[0]) == type(1):
        return int(new_value)
    raise UnsupportedTypeException(current_value[0])

def set_lock(no_flag, lock_name):
    lock_pack = "%s/%s" % (mode.global_config["tmpPath"], lock_name)
    if no_flag:
        setTime = 0
        if not os.path.isfile(lock_pack):
            return FAIL, ["Lock was not set."]
        try:
            setTime = float(open(lock_pack).read().strip())
        except ValueError:
            pass
        try:
            os.unlink(lock_pack)
        except OSError:
            return FAIL, ["Unable to remove lock file %s" % lock_pack]
        if setTime:
            return OK, ["Lock %s removed (%4.1f seconds old)" % (lock_name, time.time()-setTime)]
        return OK, ["Lock %s removed." % lock_name]

    if os.path.isfile(lock_pack):
        setTime = 0
        try:
            setTime = float(open(lock_pack).read().strip())
            return FAIL, ["Lock %s is already set (%4.1f seconds old)" % (lock_name, time.time()-setTime)]
        except ValueError:
            return FAIL, ["Lock %s is already set." % lock_name]
    try:
        open(lock_pack, 'w').write(str(time.time()))
    except IOError, e:
        return FAIL, ["Lock file %s cannot be set (%s)" % (lock_name, str(e))]
    return OK, ["Lock %s has been set." % lock_name]



class Set(PinshCmd.PinshCmd):
    '''bomsh# set bom qa foozle
       [OK, ['- new_tomcat', '- new_apache', '- new_database', '']]
       bomsh# show machine localhost.ip_address 127.0.0.2
       [OK, ['default_user: 208048363', "include:", '- foo', '- bar', '- spam', 'ip_address: 127.0.0.1', 'platform: win32', '']]
    '''
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "set", "set\tset a configuration value")
        # TOP LEVEL
        self.password = PinshCmd.PinshCmd("password","password\tset your password")

        self.level = 0
        self.cmd_owner = 1

    def cmd(self, tokens, no_flag):
        master_pass = libUi.pwd_input("Enter CI decryption password: ")
        try:
            url = "/json/dispatcher/set_password/"
            post_data = {"password": master_pass}
            output = system_state.cnm_connector.service_yaml_request(url, post_data)
            return output["status"], []
        except MachineTraceback, m_err:
            libUi.process_traceback(m_err)


