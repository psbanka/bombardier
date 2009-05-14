#!/usr/bin/python

# FIXME: This module is in horrible need of refactoring

import os
import PinshCmd, BomHostField
from bombardier_core.libCipher import decrypt, encrypt
import yaml, syck
from bombardier_core.static_data import OK, FAIL, CENSORED, PARTIAL, COMPLETE
from commands import getstatusoutput
from bombardier_server.cli.SystemStateSingleton import SystemState, ENABLE, USER, F0
system_state = SystemState()


MERGED = 1
CLIENT = 2
INCLUDE = 3
BOM = 4

class ConfigField(PinshCmd.PinshCmd):
    def __init__(self, name = "configField", data_type=MERGED, strict=True):
        PinshCmd.PinshCmd.__init__(self, name, token_delimeter = '')
        self.helpText = "<configurationField>\ta dot-delimeted configuration value"
        self.bomHostField = BomHostField.BomHostField()
        self.level = 99
        self.data_type = data_type
        self.cmdOwner = 0
        self.strict = strict # take only exact matches
        if data_type == CLIENT:
            self.directory = "client"
        if data_type == MERGED:
            self.directory = "merged"
        if data_type == INCLUDE:
            self.directory = "include"
        if data_type == BOM:
            self.directory = "bom"

    def get_machines(self):
        data = system_state.cnm_connector.service_yaml_request("json/%s/search/" % self.directory)
        machines = [ x.get("fields").get("name") for x in data ]
        return machines

    def get_data(self, first_token_name):
        url = "json/%s/name/%s" % (self.directory, first_token_name)
        data = system_state.cnm_connector.service_yaml_request(url)
        return data

    def get_top_level_data(self, tokens, index, decrypt):
        partial_first = tokens[index].split('.')[0]
        file_names = self.get_machines()
        first_token_names = [ fn for fn in file_names if fn.lower().startswith(partial_first.lower()) ]
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
        item = tokens[index+1]
        if item in current_value:
            current_value.remove(item)
            status, output = self.set_value(tokens, 2, current_value, False)
            return status, output + ["%s removed from list" % item]
        try:
            if int(item) in current_value:
                current_value.remove(int(item))
                status, output = self.set_value(tokens, 2, current_value, False)
                return status, output + ["%s removed from list" % item]
        except:
            pass
        return FAIL, ["%s is not in the current list of values." % item]

    def remove_value(self, tokens, index):
        if self.data_type == MERGED:
            return FAIL, []

        try:
            first_token_names, clear_data = self.get_top_level_data(tokens, index, True)
            first_token_names, enc_data = self.get_top_level_data(tokens, index, False)
        except TypeError:
            return FAIL, "Unable to read data from server"
        if not clear_data:
            return FAIL, []
        if type(clear_data) == type(["list"]):
            return self.remove_one_item(clear_data, tokens, index)
        first_token_name = first_token_names[0]
        config_name = self.preferred_names(tokens, index)
        if config_name == []:
            config_name=[tokens[index]] # if this item doesn't exist
        exec_string = "del enc_data"
        current_dict = enc_data
        output = []
        for config_value in config_name[0].split('.')[1:]:
            if config_value in current_dict:
                current_dict = current_dict.get(config_value)
                exec_string += "['%s']" % config_value
            else:
                config_value = "enc_" + config_value
                current_dict = current_dict.get(config_value)
                exec_string += "['%s']" % config_value
        exec( exec_string )
        string = yaml.dump(enc_data, default_flow_style=False)
        file_name = "%s/%s.yml" % (self.directory, first_token_name)
        open(file_name, 'w').write(string)
        os.system("chgrp %s %s 2> /dev/null" % (system_state.defaultGroup, file_name))
        os.system("chmod 660 %s 2> /dev/null" % (file_name))
        return OK, output


    def set_value(self, tokens, index, new_value, encrypt):
        # FIXME: Need remove_value when converting to encrypted
        if self.data_type == MERGED:
            return FAIL, []
        if new_value == "{}":
            new_value = {}
        if new_value == "[]":
            new_value = []
        first_token_names, clear_data = self.get_top_level_data(tokens, index, True)
        first_token_names, enc_data = self.get_top_level_data(tokens, index, False)
        if not clear_data:
            return FAIL, []
        first_token_name = first_token_names[0]
        config_name = self.preferred_names(tokens, index)
        if config_name == []:
            config_name=[tokens[index]] # if this item doesn't exist
        exec_string = "enc_data"
        current_dict = clear_data
        output = []
        if type(current_dict) == type(['list']):
            enc_data = new_value
        else:
            config_tokens = config_name[0].split('.')[1:]
            for config_value in config_tokens[:-1]:
                current_dict = current_dict.get(config_value)
                exec_string += "['%s']" % config_value

            config_value = config_tokens[-1]
            current_dict = current_dict.get(config_value)
            if current_dict == CENSORED or encrypt:
                if current_dict and current_dict != CENSORED:
                    self.remove_value(tokens, index)
                    first_token_names, enc_data = self.get_top_level_data(tokens, index, False)
                exec_string += "['enc_%s']" % config_value
                if not system_state.password:
                    return FAIL, ["Cannot encipher data except in enable mode"]
                new_value = encrypt(new_value, system_state.password)
                output = ["Encrypted sensitive data"]
            else:
                exec_string += "['%s']" % config_value

            if type(new_value) == type('string'):
                exec_string += " = \"%s\"" % new_value
            else:
                exec_string += " = %s" % new_value
            exec( exec_string )
        string = yaml.dump(enc_data, default_flow_style=False)
        file_name = "%s/%s.yml" % (self.directory, first_token_name)
        open(file_name, 'w').write(string)
        os.system("chgrp %s %s > /dev/null" % (system_state.defaultGroup, file_name))
        os.system("chmod 660 %s > /dev/null" % (file_name))
        return OK, output

    def get_specific_data(self, tokens, index):
        tokens[index] = tokens[index].replace('"', '')
        try:
            first_token_names, data = self.get_top_level_data(tokens, index, True)
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
        tokens[index] = tokens[index].replace('"', '')
        if not self.strict:
            return tokens[index:]
        if 1 == 1:
        #try:
            first_token_names, data = self.get_top_level_data(tokens, index, True)
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
                    possible_matches.append("%s.%s.%s" % (first_token_name, prefix, item))
                else:
                    possible_matches.append("%s.%s" % (first_token_name, item))

        if possible_matches:
            return possible_matches
        return []

    def match(self, tokens, index):
        possible_matches = self.acceptable_names(tokens, index)
        if not possible_matches:
            return NO_MATCH, 1
        if len(possible_matches) > 1:
            return PARTIAL, 1
        return COMPLETE, 1

if __name__ == "__main__":
    from libTest import startTest, runTest, endTest
    config_field = ConfigField(data_type = CLIENT, strict=True)
    status = OK
    startTest()
#    status = runTest(configField.preferred_names, [["lila"], 0], ["lilap"], status)
#    status = runTest(configField.preferred_names, [["bigdb.sql.servers"], 0], ["bigdb.sql.servers"], status)
#    status = runTest(configField.preferred_names, [["bigdb.sq"], 0], ["bigdb.sql"], status)
#    status = runTest(configField.preferred_names, [["bigdb.sql"], 0], ["bigdb.sql"], status)
#    status = runTest(configField.preferred_names, [["bigsam.ipAddress"], 0], ["bigsam.ipAddress"], status)
#    status = runTest(configField.preferred_names, [["bigsam.ipAddress."], 0], ["bigsam.ipAddress"], status)
#    status = runTest(configField.preferred_names, [["virtap.connectTest.connectionData.pro"], 0], ["virtap.connectTest.connectionData.proxy"], status)
#    status = runTest(configField.preferred_names, [["foo.foo"], 0], [], status)
#    status = runTest(configField.preferred_names, [["bigsam.thingy.majig"], 0], ['bigsam.thingy'], status)
#    status = runTest(configField.preferred_names, [["big"], 0], ["bigap", "bigsam", "bigdb"], status)
#    status = runTest(configField.preferred_names, [["sho","server","l"], 2], ["lilap", "lildb", "ltdb", "ldapserver"], status)
#    configField = ConfigField(data_type=CLIENT)
#    status = runTest(configField.preferred_names, [["lilap.s"], 0], ["lilap.sharedKeys"], status)


    #status = runTest(configField.set_value, [["lilap.sharedKeys"], 0, "yes", False], (OK, []), status)
    #status = runTest(configField.get_specific_data, [["lilap.sharedKeys"], 0], "yes", status)

    status = runTest(configField.set_value, [["lilap.nonsense"], 0, "yes", False], (OK, []), status)
    status = runTest(configField.get_specific_data, [["lilap.nonsense"], 0], "yes", status)
    status = runTest(configField.remove_value, [["lilap.nonsense"], 0], (OK, []), status)
    status = runTest(configField.set_value, [["lilap.nonsense"], 0, "{}", False], (OK, []), status)
    status = runTest(configField.set_value, [["lilap.nonsense.nonsense"], 0, "yes", False], (OK, []), status)
    status = runTest(configField.set_value, [["lilap.nonsense"], 0, "yes", False], (OK, []), status)
    system_state.password = "abcd1234"
    status = runTest(configField.set_value, [["lilap.nonsense"], 0, "yes", True], (OK, ['Encrypted sensitive data']), status)
    status = runTest(configField.set_value, [["lilap.nonsense"], 0, "yes", False], (OK, ['Encrypted sensitive data']), status)
    status = runTest(configField.remove_value, [["lilap.nonsense"], 0], (OK, []), status)

#    status = runTest(configField.set_value, [["bigdb.sharedKeys"], 0], (OK, []), status)
#    status = runTest(configField.preferred_names, [["lilap"], 0], ["lilap"], status)
#    status = runTest(configField.match, [["bigd"], 0], (COMPLETE, 1), status)
#    status = runTest(configField.match, [["bigdb", ""], 0], (COMPLETE, 1), status)
#    status = runTest(configField.match, [["lilap.foo"], 0], (NO_MATCH, 1), status)
#    status = runTest(configField.match, [["lilap"], 0], (COMPLETE, 1), status)
#    status = runTest(configField.match, [["lilap.ipAddress"], 0], (COMPLETE, 1), status)
#    status = runTest(configField.match, [["foo"], 0], (NO_MATCH, 1), status)
#    configField = ConfigField(data_type=INCLUDE)
#    status = runTest(configField.preferred_names, [["serviceNet.ca"], 0], ["serviceNet.cas"], status)
#    status = runTest(configField.preferred_names, [["servicenet"], 0], ["serviceNet"], status)
#    status = runTest(configField.get_specific_data, [["testInclude.thing1"], 0], '=== CENSORED ===', status)
#    status = runTest(configField.set_value, [["testInclude.thing1"], 0], (FAIL, ['Cannot encipher data except in enable mode']), status)
#    system_state.password = "abcd1234"
#    status = runTest(configField.set_value, [["testInclude.thing1"], 0], (OK, ['Encrypted sensitive data']), status)
#    status = runTest(configField.get_specific_data, [["testInclude.thing1"], 0], '=== CENSORED ===', status)
#    status = runTest(configField.get_specific_data, [["testInclude.platform"], 0], "win32", status)
#    status = runTest(configField.set_value, [["testInclude.platform"], 0], (OK, []), status)
#    status = runTest(configField.get_specific_data, [["testInclude.platform"], 0], "foo", status)
#    status = runTest(configField.set_value, [["testInclude.platform"], 0], (OK, []), status)
#    status = runTest(configField.get_specific_data, [["testInclude.platform"], 0], "win32", status)
#    status = runTest(configField.get_specific_data, [["testInclude.sql.databases"], 0], ['casDB', 'mcsDB', 'rmsDB', 'uhrDB'], status)
#    #status = runTest(configField.set_value, [["testInclude.sql.databases"], 0, ['casDB', 'mcsDB', 'rmsDB', 'uhrDB', 'foo']], (OK, []), status)
#    #status = runTest(configField.get_specific_data, [["testInclude.sql.databases"], 0], ['casDB', 'mcsDB', 'rmsDB', 'uhrDB', 'foo'], status)
#    #status = runTest(configField.set_value, [["testInclude.sql.databases"], 0, ['casDB', 'mcsDB', 'rmsDB', 'uhrDB']], (OK, []), status)
#    #status = runTest(configField.get_specific_data, [["testInclude.sql.databases"], 0], ['casDB', 'mcsDB', 'rmsDB', 'uhrDB'], status)
#    status = runTest(configField.get_specific_data, [["testInclude.platform"], 0], "win32", status)
#    status = runTest(configField.get_specific_data, [['testInclude.thingWithoutSpaces.thisIsAThing'], 0], "wuzz", status)
#    status = runTest(configField.get_specific_data, [['testInclude."thingWithoutSpaces"."thisIsAThing"'], 0], "wuzz", status)
#    status = runTest(configField.get_specific_data, [['testInclude."Thing With Spaces"."this is a thing"'], 0], "fuzz", status)
#    status = runTest(configField.get_specific_data, [['"testInclude.Thing With Spaces.this is a thing"'], 0], "fuzz", status)
#    configField = ConfigField(data_type=BOM)
#    status = runTest(configField.preferred_names, [["testB"], 0], ["testbom"], status)
#    status = runTest(configField.preferred_names, [["testbom.DbAuth"], 0], ["testbom"], status)
#    status = runTest(configField.preferred_names, [['sho', 'bom', 'testb'], 2], ["testbom"], status)
    endTest(status)
