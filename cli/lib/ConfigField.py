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
import random
import yaml
from bombardier_core.static_data import OK, FAIL, PARTIAL, COMPLETE, NO_MATCH
from Exceptions import UnexpectedDataException
from SystemStateSingleton import SystemState
system_state = SystemState()

MERGED = 1
MACHINE = 2
INCLUDE = 3
BOM = 4
PACKAGE = 5
USER = 6
DIST = 7
STATUS = 8
SUMMARY = 9

DEFAULT_MACHINE = """ip_address: 127.0.0.1
default_user: root
platform: rhel5
"""

DEFAULT_PACKAGE = """class_name: Module.Class
dependencies: []
injectors: {}
libs: {}
package-version: 5
release: 0
"""

DEFAULT_BOM = """
-
"""

DEFAULT_USER = """email: user@example.com
first_name: ''
is_active: true
is_superuser: false
last_name: ''
password: '%s'
"""

class ConfigField(PinshCmd.PinshCmd):
    '''The server keeps track of several types of configuration data.
    This class can provide command-line completion for those data objects'''
    def __init__(self, name = "configField", data_type=MERGED, strict=True,
                 new=False, machine_field=None):
        '''
        name -- Not used except in debugging
        data_type -- which type of configuration data is this object supposed
                     to match? Can be MERGED, MACHINE, INCLUDE, BOM, DIST, 
                     or PACKAGE
        strict -- The preferred_names() method can be either liberal such that
                  'local' will match 'localhost' or can be strict such that
                  'local' will not match 'localhost'
        '''
        PinshCmd.PinshCmd.__init__(self, name)
        self.help_text = "<configurationField>\t"\
                         "a dot-delimeted configuration value"
        self.data_type = data_type
        self.cmd_owner = 0
        self.strict = strict # take only exact matches
        self.machine_field = machine_field
        if data_type == MACHINE:
            self.directory = "machine"
        elif data_type == MERGED:
            self.directory = "merged"
        elif data_type == INCLUDE:
            self.directory = "include"
        elif data_type == BOM:
            self.directory = "bom"
        elif data_type == PACKAGE:
            self.directory = "package"
        elif data_type == USER:
            self.directory = "user"
        elif data_type == DIST:
            self.directory = "dist"
        elif data_type == STATUS:
            self.directory = "status"
        elif data_type == SUMMARY:
            self.directory = "summary"
        self.new = new

    def get_object_list(self):
        'returns a list of all self.data_type things'
        url = "json/%s/search/" % self.directory
        data = system_state.cnm_connector.service_yaml_request(url)
        #print "get_object_list: url: %s\ndata:%s\n" %(url, data)
        if self.directory == "user":
            object_list = [ x.get("fields").get("username") for x in data ]
        else:
            object_list = [ x.get("fields").get("name") for x in data ]
        return object_list

    def post_data(self, first_token_name, data):
        'posts data to modify the object'
        post_data = {"yaml": yaml.dump(data)}
        url = "json/%s/name/%s" % (self.directory, first_token_name)
        output = system_state.cnm_connector.service_yaml_request(url, post_data=post_data)
        return output

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
        #print "my directory: %s" % self.directory
        #print "get_top_lvl_data: tokens", tokens
        partial_first = tokens[index]
        object_names = self.get_object_list()
        #print "OBJECT NAMES:",object_names
        first_token_names = []
        for ftn in object_names:
            if ftn.lower().startswith(partial_first.lower()):
                #print "%s starts with the same as partial_first: %s" % (ftn, partial_first)
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

    def get_specific_data(self, tokens, index):
        '''used with the show command to display data to the screen'''
        tokens[index] = tokens[index].replace('"', '')
        try:
            first_token_names, data = self.get_top_level_data(tokens, index)
        except TypeError:
            return FAIL, "Unable to read data from server"
        if len(first_token_names) != 1:
            return '' # EXPERIMENTAL CHANGE from []
        return data


    def post_specific_data(self, tokens, index, new_data):
        '''used with the edit command to upload data to server'''
        tokens[index] = tokens[index].replace('"', '')
        if not self.new:
            try:
                first_token_names, data = self.get_top_level_data(tokens, index)
            except TypeError:
                return FAIL, "Unable to read data from server"
            if len(first_token_names) < 1:
                msg = "%s was not found on the server to upload to" % tokens[index]
                raise UnexpectedDataException(msg)
            url = "json/%s/name/%s" % ( self.directory, first_token_names[0])
        else:
            url = "json/%s/name/%s" % ( self.directory, tokens[index])
        post_data = {"yaml": new_data}
        output_dict = system_state.cnm_connector.service_yaml_request(url,
                                                               post_data=post_data)
        return output_dict["command_status"], output_dict["command_output"]

    def preferred_names(self, tokens, index):
        '''Provide a list of names that the system would prefer to use, other
        than that which was typed in by the user. For example, 'sho mach localh'
        will return 'localhost' for the machine name if strict is off,
        otherwise, it will return 'localh'.
        '''
        if self.machine_field:
            machine_name = tokens[self.machine_field]
            url = "json/machine/name/%s" % machine_name
            machine_config = system_state.cnm_connector.service_yaml_request(url)
            possible_names = machine_config.get(self.directory, [])
            output_names = []
            for name in possible_names:
                if name.lower().startswith(tokens[-1].lower()):
                    output_names.append(name)
            return output_names
            
        #print "PN: begin tokens, index = ", tokens, index
        tokens[index] = tokens[index].replace('"', '')
        if not self.strict:
            return tokens[index:]
        try:
            first_token_names, data = self.get_top_level_data(tokens, index)
        except TypeError:
            return FAIL, "Unable to read data from server"

        #print "PN: first_token_names: ", first_token_names
        if len(first_token_names) == 0:
            if self.new:
                return tokens[index:]
            return []
        if self.new:
            if not tokens[index] in first_token_names:
                return tokens[index:]
            else:
                return []
        if len(first_token_names) > 1:
            return first_token_names
        first_token_name = first_token_names[0]
        return [first_token_name]

    def match(self, tokens, index):
        '''Determines if what has been typed in by the user matches a
        configuration item that the system is keeping track of.'''
        possible_matches = self.acceptable_names(tokens, index)
        if not possible_matches:
            return NO_MATCH, 1
        if len(possible_matches) > 1:
            return PARTIAL, 1
        return COMPLETE, 1

    def get_default_data(self):
        if self.data_type == MACHINE:
            return DEFAULT_MACHINE
        elif self.data_type == USER:
            letters = [ chr( x ) for x in range(65, 91) ]
            random.shuffle(letters)
            default_user = DEFAULT_USER % ''.join(letters)
            return default_user
        elif self.data_type == BOM:
            return DEFAULT_BOM
        elif self.data_type == PACKAGE:
            return DEFAULT_PACKAGE
        return ""
