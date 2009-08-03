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

'''A [PinshCmd] object that provides a field with automated completion.
Supports access to 'configurable items' on the bombardier web server'''

import PinshCmd
from bombardier_core.static_data import OK, FAIL, PARTIAL, COMPLETE, NO_MATCH
from SystemStateSingleton import SystemState
system_state = SystemState()

MERGED = 1
MACHINE = 2
INCLUDE = 3
BOM = 4
PACKAGE = 5
USER = 6

class ConfigField(PinshCmd.PinshCmd):
    '''The server keeps track of several types of configuration data.
    This class can provide command-line completion for those data objects'''
    def __init__(self, name = "configField", data_type=MERGED, strict=True):
        '''
        name -- Not used except in debugging
        data_type -- which type of configuration data is this object supposed
                     to match? Can be MERGED, MACHINE, INCLUDE, BOM, or PACKAGE
        strict -- The preferred_names() method can be either liberal such that
                  'local' will match 'localhost' or can be strict such that
                  'local' will not match 'localhost'
        '''
        PinshCmd.PinshCmd.__init__(self, name, token_delimeter = '')
        self.help_text = "<configurationField>\t"\
                         "a dot-delimeted configuration value"
        self.data_type = data_type
        self.cmd_owner = 0
        self.strict = strict # take only exact matches
        if data_type == MACHINE:
            self.directory = "machine"
        if data_type == MERGED:
            self.directory = "merged"
        if data_type == INCLUDE:
            self.directory = "include"
        if data_type == BOM:
            self.directory = "bom"
        if data_type == PACKAGE:
            self.directory = "package"
        if data_type == USER:
            self.directory = "user"

    def get_object_list(self):
        'returns a list of all self.data_type things'
        url = "json/%s/search/" % self.directory
        data = system_state.cnm_connector.service_yaml_request(url)
        if self.directory == "user":
            object_list = [ x.get("fields").get("username") for x in data ]
        else:
            object_list = [ x.get("fields").get("name") for x in data ]
        return object_list

    def get_data(self, first_token_name):
        'returns a list of all configuration objects of this type'
        url = "json/%s/name/%s" % (self.directory, first_token_name)
        data = system_state.cnm_connector.service_yaml_request(url)
        return data

    def get_top_level_data(self, tokens, index):
        '''someone typed in something like 'show machine localho'. Our job is
        to figure out that localhost is the object that needs to be found and
        to return the dictionary for that object.
        '''
        partial_first = tokens[index].split('.')[0]
        object_names = self.get_object_list()
        first_token_names = []
        for ftn in object_names:
            if ftn.lower().startswith(partial_first.lower()):
                first_token_names.append(ftn)
        if len(first_token_names) == 0:
            return [], {}
        if tokens[index] in first_token_names:
            first_token_names = [tokens[index]]
        if len(first_token_names) > 1:
            return first_token_names, {}
        first_token_name = first_token_names[0]
        data = self.get_data(first_token_name)
        return [first_token_name], data

    def remove_one_item(self, current_value, tokens, index):
        "NOT WORKING WITH WEB SERVICE"
#        item = tokens[index+1]
#        if item in current_value:
#            current_value.remove(item)
#            status, output = self.set_value(tokens, 2, current_value)
#            return status, output + ["%s removed from list" % item]
#        try:
#            if int(item) in current_value:
#                current_value.remove(int(item))
#                status, output = self.set_value(tokens, 2, current_value)
#                return status, output + ["%s removed from list" % item]
#        except:
#            pass
#        return FAIL, ["%s is not in the current list of values." % item]

    def remove_value(self, tokens, index):
        "NOT WORKING WITH WEB SERVICE"
#        if self.data_type == MERGED:
#            return FAIL, []
#
#        try:
#            first_token_names, clear_data = self.get_top_level_data(tokens, index)
#            first_token_names, enc_data = self.get_top_level_data(tokens, index)
#        except TypeError:
#            return FAIL, "Unable to read data from server"
#        if not clear_data:
#            return FAIL, []
#        if type(clear_data) == type(["list"]):
#            return self.remove_one_item(clear_data, tokens, index)
#        first_token_name = first_token_names[0]
#        config_name = self.preferred_names(tokens, index)
#        if config_name == []:
#            config_name = [tokens[index]] # if this item doesn't exist
#        exec_string = "del enc_data"
#        current_dict = enc_data
#        output = []
#        for config_value in config_name[0].split('.')[1:]:
#            if config_value in current_dict:
#                current_dict = current_dict.get(config_value)
#                exec_string += "['%s']" % config_value
#            else:
#                config_value = "enc_" + config_value
#                current_dict = current_dict.get(config_value)
#                exec_string += "['%s']" % config_value
#        exec( exec_string )
#        string = yaml.dump(enc_data, default_flow_style=False)
#        file_name = "%s/%s.yml" % (self.directory, first_token_name)
#        open(file_name, 'w').write(string)
#        os.system("chgrp %s %s 2> /dev/null" % (system_state.defaultGroup, file_name))
#        os.system("chmod 660 %s 2> /dev/null" % (file_name))
        return OK, []


    def set_value(self, tokens, index, new_value):
        "NOT WORKING WITH WEB SERVICE"
#        if self.data_type == MERGED:
#            return FAIL, []
#        if new_value == "{}":
#            new_value = {}
#        if new_value == "[]":
#            new_value = []
#        first_token_names, clear_data = self.get_top_level_data(tokens, index)
#        first_token_names, enc_data = self.get_top_level_data(tokens, index)
#        if not clear_data:
#            return FAIL, []
#        first_token_name = first_token_names[0]
#        config_name = self.preferred_names(tokens, index)
#        if config_name == []:
#            config_name=[tokens[index]] # if this item doesn't exist
#        exec_string = "enc_data"
#        current_dict = clear_data
#        output = []
#        if type(current_dict) == type(['list']):
#            enc_data = new_value
#        else:
#            config_tokens = config_name[0].split('.')[1:]
#            for config_value in config_tokens[:-1]:
#                current_dict = current_dict.get(config_value)
#                exec_string += "['%s']" % config_value
#
#            config_value = config_tokens[-1]
#            current_dict = current_dict.get(config_value)
#            if current_dict == CENSORED or encrypt:
#                if current_dict and current_dict != CENSORED:
#                    self.remove_value(tokens, index)
#                    first_token_names, enc_data = self.get_top_level_data(tokens, index)
#                exec_string += "['enc_%s']" % config_value
#                if not system_state.password:
#                    return FAIL, ["Cannot encipher data except in enable mode"]
#                new_value = encrypt(new_value, system_state.password)
#                output = ["Encrypted sensitive data"]
#            else:
#                exec_string += "['%s']" % config_value
#
#            if type(new_value) == type('string'):
#                exec_string += " = \"%s\"" % new_value
#            else:
#                exec_string += " = %s" % new_value
#            exec( exec_string )
#        string = yaml.dump(enc_data, default_flow_style=False)
#        file_name = "%s/%s.yml" % (self.directory, first_token_name)
#        open(file_name, 'w').write(string)
#        os.system("chgrp %s %s > /dev/null" % (system_state.defaultGroup, file_name))
#        os.system("chmod 660 %s > /dev/null" % (file_name))
        return OK, []

    def get_specific_data(self, tokens, index):
        '''used with the show command to display data to the screen'''
        tokens[index] = tokens[index].replace('"', '')
        try:
            first_token_names, data = self.get_top_level_data(tokens, index)
        except TypeError:
            return FAIL, "Unable to read data from server"
        if len(first_token_names) != 1:
            return '' # EXPERIMENTAL CHANGE from []
        if len(tokens[index].split('.')) > 1:
            config_name = self.preferred_names(tokens, index)
            if len(config_name) == 0:
                return '' # EXPERIMENTAL CHANGE from []
            if len(config_name) > 1:
                return config_name
            current_dict = data
            for config_value in config_name[0].split('.')[1:]:
                config_value = config_value.replace('"', '')
                if type(current_dict) == type({}):
                    current_dict = current_dict.get(config_value)
                else:
                    return current_dict
        else:
            current_dict = data
        return current_dict

    def preferred_names(self, tokens, index):
        '''Provide a list of names that the system would prefer to use, other
        than that which was typed in by the user. For example, 'sho mach localh'
        will return 'localhost' for the machine name if strict is off,
        otherwise, it will return 'localh'.

        '''
        tokens[index] = tokens[index].replace('"', '')
        if not self.strict:
            return tokens[index:]
        if 1 == 1:
        #try:
            first_token_names, data = self.get_top_level_data(tokens, index)
        else:
        #except TypeError:
            return FAIL, "Unable to read data from server"
        if len(first_token_names) == 0:
            return []
        if len(first_token_names) > 1:
            return first_token_names
        first_token_name = first_token_names[0]
        if len(tokens[index].split('.')) == 1:
            return [first_token_name]
        config_values = tokens[index].split('.')[1:]
        #print "PN: config_values", config_values, data
        current_dict = None
        if type(data) == type({}):
            current_dict = data
            for config_value in config_values[:-1]:
                config_value = config_value.replace('"', '')
                new_current_dict = current_dict.get(config_value)
                if type(new_current_dict) in [type({})]:
                    current_dict = new_current_dict
                else:
                    current_dict = []
                    break
        #print "PN::: current_dict",current_dict
        possible_matches = []
        prefix = '.'.join(config_values[:-1])
        if current_dict == None:
            if prefix:
                return ["%s.%s" % (first_token_name, prefix)]
            else:
                return ["%s" % (first_token_name)]
        for item in current_dict:
            test_value = config_values[-1].replace('"','').lower()
            if item.lower().startswith(test_value):
                if prefix:
                    p_match = "%s.%s.%s" % (first_token_name, prefix, item)
                    possible_matches.append(p_match)
                else:
                    possible_matches.append("%s.%s" % (first_token_name, item))

        if possible_matches:
            return possible_matches
        return []

    def match(self, tokens, index):
        '''Determines if what has been typed in by the user matches a
        configuration item that the system is keeping track of.'''
        possible_matches = self.acceptable_names(tokens, index)
        if not possible_matches:
            return NO_MATCH, 1
        if len(possible_matches) > 1:
            return PARTIAL, 1
        return COMPLETE, 1
