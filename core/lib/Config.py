#!/cygdrive/c/Python24/python.exe

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

# Copyright (C) 2005-2010 Peter Banka et al

'''Config.py: This class provides the functionality of providing
heirarchichal configuration data through either a dictionary
information to be stored either on the server or locally in the
config.yml file in the bombardier home directory.'''


import yaml
from mini_utility import hash_dictionary, diff_dicts, get_spkg_path
import copy, os
from Exceptions import InvalidConfigData
from Logger import Logger
from static_data import OK, FAIL, CONFIG_FILE

class Config(dict):
    """This object retains the complete configuration for a system.
    It tries to impersonate a dictionary as well as provide a nice
    string interface to the data it holds"""

    def __init__(self, instance_name, config_data):
        '''
        instance -- the name of this machine
        config_data -- configuration information for this machine
        '''
        dict.__init__(self)
        self.instance_name = instance_name
        self.includes   = []
        self.automated  = False
        self.username   = None
        self.password   = None
        self.domain     = None
        if not config_data:
            config_data = {}
        self.data       = config_data
        self._read_local_config()

    def _read_local_config(self):
        '''If there is a cleartext configuration file on the system,
        we will read that and ignore the configuration sent to us from
        the server.'''
        spkg_path = get_spkg_path()
        if not spkg_path:
            return FAIL
        config_path = os.path.join(get_spkg_path(), self.instance_name,
                                  CONFIG_FILE)
        if not os.path.isfile(config_path):
            return FAIL
        msg = "DETECTED A CLEARTEXT LOCAL CONFIGURATION FILE:"\
              " IGNORING MANAGEMENT SERVER CONFIGURATION"
        Logger.warning(msg)
        file_handle = open(config_path, 'r')
        try:
            config_data = file_handle.read()
            self.data = yaml.load(config_data)
        except:
            return FAIL
        return OK

    def __getitem__(self, key):
        'for dictionary impersonation'
        return self.data[key]

    def __setitem__(self, key, value):
        'for dictionary impersonation'
        self.data[key] = value

    def keys(self):
        'for dictionary impersonation'
        return self.data.keys()

    def getInstance(self):
        'deprecated function'
        Logger.warning("getInstance is Deprecated: use get_instance instead")
        return self.get_instance()

    def get_instance(self):
        'getter'
        return self.instance_name

    def set_bom_pkgs(self, bom_pkns):
        'setter'
        self.data["packages"] = copy.deepcopy(bom_pkns)

    def get_bom_pkns(self):
        'getter'
        if self.data.has_key("packages"):
            return copy.deepcopy(self.data["packages"])
        return []

    def save_hash(self, path):
        '''
        Save off a hash of our config data for future reference
        path -- the place to find the configuration hash that was used the
                last time this package was configured or installed
        '''
        try:
            file_handle = open(path, 'w')
            hash_dict = hash_dictionary(self.data)
            hash_yaml = yaml.dump(hash_dict)
            file_handle.write(hash_yaml)
            file_handle.close()
        except IOError:
            return FAIL
        return OK

    def check_hash(self, path):
        '''
        We saved off a hash of our configuration data the last time we did some
        package maintenance. Now we're going to load that saved value and
        compare it with our current config. That will tell us what values have
        changed.
        path -- the place to find the configuration hash that was used the
                last time this package was configured or installed
        '''
        old_config = {}
        difference = {}
        try:
            yaml_string = open(path, 'r').read()
            old_config = yaml.load(yaml_string)
            new_config = hash_dictionary(self.data)
            difference = diff_dicts(old_config, new_config, check_values=True)
        except IOError:
            msg = "Could not load saved configuration data in %s" % path
            Logger.debug(msg)
        except IndexError:
            msg = "Could not compare configuration data in %s" % path
            Logger.debug(msg)
        except:
            Logger.warning("Bad yaml in file %s" % path)
        return difference

    def set(self, section, option, value):
        'dictionary impersonation'
        if type(self.data.get(section)) != type(dict()):
            self.data[section] = {}
        self.data[section][option] = value
        return OK

    def has_section(self, section_query):
        'ConfigParser impersonation'
        for section in self.data.keys():
            if section.lower() == section_query.lower():
                if type(self.data[section]) == type({}):
                    return True
        return False

    def has_option(self, section_query, option_query):
        'ConfigParser impersonation'
        for section in self.data.keys():
            if section.lower() == section_query.lower():
                section_data = self.data[section]
                if type(section_data) == type({}):
                    for option in section_data.keys():
                        if option.lower() == option_query.lower():
                            return True
        return False

    def get_raw(self, section, option, default=None, optional=True):
        '''Get any object from the configuration
        section -- this is a top-level value
        option -- a key under the top-level
        default -- a default value, if not set
        optional -- whether the system should require the value to
                    be set for a machine that implements a package
        '''
        if self.data.has_key(section):
            if self.data[section].has_key(option):
                return self.data[section][option]
        for key in self.data.keys():
            if key.lower() == section.lower():
                if type(self.data[key] == type({})):
                    for subkey in self.data[key].keys():
                        if subkey.lower() == option.lower():
                            return self.data[key][subkey]
        if default != None:
            if not self.data.has_key(section):
                self.data[section] = {}
            self.data[section][option] = default
            return default
        else:
            if self.data.has_key(section):
                msg = "Option %s not found for section %s" % (option, section)
                raise InvalidConfigData(msg, None, None)
            else:
                msg = "Section %s not found" % section
                raise InvalidConfigData(msg, None, None)

    def get(self, section, option, default='', optional=True):
        'dictionary impersonation'
        return str(self.get_raw(section, option, default))

    def get_dict(self, section, option, default = {}, optional = True):
        '''Get a dictionary object from the configuration
        section -- this is a top-level value
        option -- a key under the top-level
        default -- a default value, if not set
        optional -- whether the system should require the value to
                    be set for a machine that implements a package
        '''
        result = self.get_raw(section, option, default)
        if result.__class__ == {}.__class__:
            return result
        else:
            raise TypeError

    def _parse_section(self, section_string, default, optional):
        '''Provides 'dotted-value' access to configuration data
        section_string -- a string to get configuration 
                          (e.g. 'svn.server.home_directory')
        default -- default value
        optional -- whether it should be required by machine config
        '''
        sections = section_string.split('.')
        dat = self.data
        for section in sections:
            try:
                dat = dat[section]
            except KeyError:
                dat = None
                break
        if dat == None:
            if not optional:
                msg = "Option %s not found" % section_string
                raise InvalidConfigData(msg, None, None)
            dat = default
        return dat

    def _getobj(self, section_string, default, expected_type, optional):
        '''dotted-value access to a generic object
        section_string -- configuration section
        default -- default value
        expected_type -- type we should return
        optional -- whether it must be implemented
        '''
        value = self._parse_section(section_string, default, optional)
        if type(expected_type) == type("string"):
            if type(value) == type(1234) or type(value) == type(123.32):
                value = str(value)
        if type(value) == type(expected_type):
            return value
        raise InvalidConfigData(section_string, type(value),
                                type(expected_type))

    def listobj(self, section_string, default=[], optional=True):
        'Get a list object from the config'
        return self._getobj(section_string, default, [], optional)

    def string(self, section_string, default='', optional=True):
        '''Get a string object from the config
        >>> data = {"section": {"item1": "value1", "item2": 2, "item3": [1,2,3]}}
        >>> config = Config("tester", data)
        >>> config.string("section.item1")
        "value1"
        >>> config.string("section.item234", default = "not_defined")
        "not_defined"
        >>> config.string("section.item2")
        "2"
        '''
        return self._getobj(section_string, default, "string", optional)

    def integer(self, section_string, default=1, optional=True):
        '''Get a integer object from the config
        >>> data = {"section": {"item1": "value1", "item2": 2, "item3": [1,2,3]}}
        >>> config = Config("tester", data)
        >>> config.integer("section.item2")
        2
        '''
        return self._getobj(section_string, default, 1, optional)

    def boolean(self, section_string, default={}, optional=True):
        '''Get a boolean object from the config'
        >>> data = {"thing": True}
        >>> config = Config("tester", data)
        >>> config.boolean("section")
        True
        '''
        return self._getobj(section_string, default, True, optional)

    def dictionary(self, section_string, default={}, optional=True):
        '''Get a dictionary object from the config'
        >>> data = {"section": {"item1": "value1", "item2": 2, "item3": [1,2,3]}}
        >>> config = Config("tester", data)
        >>> config.dictionary("section")
        {"item1": "value1", "item2": 2, "item3": [1,2,3]}
        '''
        return self._getobj(section_string, default, {}, optional)

if __name__ == "__main__":
    import doctest
    doctest.testmod()

