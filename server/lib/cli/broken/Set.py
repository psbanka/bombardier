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

def cleanup():
    directories = ["include", "client"]
    for directory in directories:
        full_glob_path = os.path.join(system_state.server_home, directory, "*.yml.new")
        files = glob.glob(full_glob_path)
        for file_name in files:
            os.unlink(file_name)

def re_encrypt_files(old_password, new_password):
    print "Re-encrypting files...",
    directories = ["include", "client"]
    renameFiles = []
    for directory in directories:
        full_glob_path = os.path.join(system_state.server_home, directory, "*.yml")
        files = glob.glob(full_glob_path)
        for file_name in files:
            base_name = file_name.split(os.path.sep)[-1]
            new_file_name = base_name + ".new"
            sys.stdout.write('.')
            sys.stdout.flush()
            try:
                old_data = syck.load(open(file_name).read())
            except syck.error, e:
                err_str = e[0]
                msg  = ["Could not parse file under configuration control"]
                msg += ["File name: %s" % file_name]
                msg += ["Error: %s" % (err_str)]
                cleanup()
                return FAIL, msg
            try:
                new_data = change_pass(old_data, old_password, new_password)
            except DecryptionException, de:
                msg  = ["Could not decrypt file under configuration control"]
                msg += ["File name: %s" % file_name]
                msg += ["Error: %s" % (de)]
            except InvalidData, id:
                msg  = ["Could not perform encryption for file under configuration control"]
                msg += ["File name: %s" % file_name]
                msg += ["Error: %s" % (id)]
                return FAIL, msg
            newFilePath = os.path.join(system_state.server_home, directory, new_file_name)
            fh = open(newFilePath, 'w')
            fh.write(yaml.dump(new_data, default_flow_style=False))
            renameFiles.append((file_name, newFilePath))
    overallStatus = OK
    errorFiles = []
    for old_name,new_name in renameFiles:
        status1 = os.system("mv %s %s" % (new_name, old_name))
        status2 = os.system("chgrp %s %s 2> /dev/null" % (mode.defaultGroup, old_name))
        status3 = os.system("chmod 660 %s 2> /dev/null" % (old_name))
        if status1 != OK or status2 != OK or status3 != OK:
            errorFiles.append(old_name)
            overallStatus = FAIL
    if overallStatus != OK:
        return FAIL, ["Files that failed to be updated: %s" % errorFiles]
    return OK, ["Password updated"]

def setPassword(re_encrypt):
    if libUi.askYesNo("Reset master password", NO) != YES:
        return OK, ["No action taken"]
    if mode.auth == ADMIN or 'password' not in mode.global_config:
        old_password = mode.password
        # MASTER PASSWORD HAS NEVER BEEN SET
        master_pass_1 = libUi.pwd_input("Enter new master password: ")
        if master_pass_1 == '':
            return FAIL, ["Aborted"]
        master_pass_2 = libUi.pwd_input("Verify new master password: ")
        if master_pass_1 != master_pass_2:
            return FAIL, ['Passwords do not match']
    else:
        return FAIL, ["Must be done from enable mode"]
    new_password = master_pass_1

    if mode.auth == ADMIN:
        if re_encrypt:
            status, msg = re_encrypt_files(old_password, new_password)
            if status != OK:
                return FAIL, msg
    if mode.auth != ADMIN:
        mode.auth = ADMIN
        mode.push_prompt("#", Mode.ENABLE)
    mode.password = master_pass_1
    salt = ''.join(random.sample(VALID_CHARS, 20))
    make_md5 = md5.new()
    make_md5.update(salt)
    make_md5.update(mode.password)
    mode.write_config("password", (salt, make_md5.hexdigest()))
    print
    return OK, ["Password set"]

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
        self.client = PinshCmd.PinshCmd("client","client\tchange the configuration of one client")
        self.bom = PinshCmd.PinshCmd("bom","bom\tchange a bill of materials")
        self.include = PinshCmd.PinshCmd("include","include\tchange an include file")
        self.password = PinshCmd.PinshCmd("password","password\tset your password")
        without_re_encryption= PinshCmd.PinshCmd("without-reencryption","without-reencryption\tDon't re-encrypt data files")
        self.password.children = [without_re_encryption]
        self.lock = PinshCmd.PinshCmd("lock", "lock\tset a lock")
        self.children = [self.client, self.bom, self.include, self.password, self.lock]

        expression = Expression.Expression()
        encrypt = PinshCmd.PinshCmd("encrypt","encrypt\tencrypt the confiuration value")
        encrypt.children = [expression]

        # CLIENT
        self.client_config_field = ConfigField.ConfigField(data_type=ConfigField.CLIENT)
        self.client.children = [self.client_config_field]
        self.client_config_field.children = [expression, encrypt]

        # INCLUDE
        self.include_config_field = ConfigField.ConfigField(data_type=ConfigField.INCLUDE)
        self.include.children += [self.include_config_field]
        self.include_config_field.children = [expression]

        # BOM
        self.bom_config_field = ConfigField.ConfigField(data_type=ConfigField.BOM)
        self.bom.children += [self.bom_config_field]
        self.bom_config_field.children = [expression]

        # LOCK
        self.lock.children = [Variable.Variable()]

        self.level = 0
        self.cmd_owner = 1

    def get_field_object(self, tokens):
        configType = tokens[1].lower()
        if configType.startswith('c'):
            field_object = self.client_config_field
        elif configType.startswith('i'):
            field_object = self.include_config_field
        elif configType.startswith('b'):
            field_object = self.bom_config_field
        else:
            field_object = None
        return field_object

    def get_new_value(self, tokens, encrypt, default=''):
        if type(default) != type("string"):
            default = ''
        if encrypt:
            new_value = libUi.pwd_input("Enter value to encrypt: ")
        elif len(tokens) < 4:
            new_value = libUi.get_default("Enter value", default)
        else:
            new_value = ' '.join(tokens[3:])
        return new_value

    def modify_list(self, tokens, current_value, new_value, field_object, encrypt):
        new_value = make_same_type(current_value, new_value)
        current_value.append(new_value)
        try:
            status, output = field_object.set_value(tokens, 2, current_value, encrypt)
            return status, output + ["%s appended to list" % new_value]
        except:
            return FAIL, ["Unable to set. Check your configuration path."]

    def modify_scalar(self, tokens, new_value, field_object, encrypt):
        status, output = field_object.set_value(tokens, 2, new_value, encrypt)
        return status, output + ["Value set."]

    def set_config_value(self, tokens, no_flag):
        field_object = self.get_field_object(tokens)
        if no_flag:
            current_value = field_object.getSpecificData(tokens, 2)
            if type(current_value) == type(['list']):
                item = tokens[3]
                if item in current_value:
                    current_value.remove(item)
                    status, output = field_object.set_value(tokens, 2, current_value, False)
                    return status, output + ["%s removed from list" % item]
                try:
                    if int(item) in current_value:
                        current_value.remove(int(item))
                        status, output = self.set_value(tokens, 2, current_value, False)
                        return status, output + ["%s removed from list" % item]
                except:
                    pass
                return FAIL, ["%s is not in the current list of values." % item]
            return field_object.removeValue(tokens, 2)
        encrypt = False
        if len(tokens) == 4 and tokens[-1].lower() == 'encrypt':
            encrypt = True
        if not field_object:
            return FAIL, ["Unknown configuration item: %s" % tokens[2]]
        current_value = field_object.getSpecificData(tokens, 2)
        new_value = self.get_new_value(tokens, encrypt, current_value)
        if type(current_value) == type(['list']):
            return self.modify_list(tokens, current_value, new_value, field_object, encrypt)
        elif type(current_value) in [type('string'), type(1), type(True)]:
            if current_value == new_value and not encrypt:
                return FAIL, ["New value and old value are identical."]
            return self.modify_scalar(tokens, new_value, field_object, encrypt)

    def cmd(self, tokens, no_flag):
        if tokens[1].lower().startswith('l'):
            if len(tokens) < 2:
                return FAIL, ["Incomplete command; must provide lock name"]
            lock_name = tokens[2]
            return set_lock(no_flag, lock_name)
        if tokens[1].lower().startswith('p'):
            if len(tokens) == 3 and tokens[2].lower().startswith('w'):
                return setPassword(re_encrypt=False)
            return setPassword(re_encrypt=True)
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        return self.set_config_value(tokens, no_flag)


class Lock(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "lock")
        self.helpText = "lock\tset a lock"
        #self.children = [FileNameField.FileNameField(mode.global_config["tmpPath"], crusty=False)]
        self.children = [Variable.Variable()]
        self.level = 0
        self.cmd_owner = 1

