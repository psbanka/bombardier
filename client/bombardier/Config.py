#!/cygdrive/c/Python24/python.exe

# Config.py: This class provides the functionality of providing
# heirarchichal configuration data through either a dictionary
# interface or a ConfigParser interface. It allows configuration
# information to be stored either on the server or locally in the
# config.yml file in the bombardier home directory.

# Copyright (C) 2005 Peter Banka

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

import ConfigParser, yaml
from staticData import *
import miniUtility, Logger
import random

def generatePassword():
    random.seed()
    characters = [ chr(x) for x in range(33,122) ]
    password = ''.join([ random.choice(characters) for x in range(0,PASSWORD_LENGTH)])
    return password

def getKey(data, index):
    obj = data
    for key in index:
        obj = obj.get(key)
        if obj == None:
            return
    return obj
            
def findIncludeList(data):
    includeList = []
    for key in data.keys():
        if key.upper() == "INCLUDE":
            includeList += data[key]
    return includeList

class Config(dict):

    """This object retains the complete configuration for a system. It
    provides that information either as a dictionary or a
    ConfigParser. It will download its own configuration from the
    repository as well as following any 'include' chains to create a
    single unified dictionary."""
    
    ### TESTED
    def __init__(self, filesystem, server, windows):
        self.filesystem = filesystem
        self.server     = server
        self.windows    = windows
        self.repository = None
        self.config     = ConfigParser.ConfigParser()
        self.includes    = []
        self.automated  = False
        self.data       = {}
        self.username   = None
        self.password   = None
        self.domain     = None
        self.freshStart = True

    def __getitem__(self, key):
        return self.data[key]

    def __setitem__(self, key, value):
        self.data[key] = value

    def keys(self):
        return self.data.keys()

    def getPackageGroups(self):
        groups   = []
        packages = []
        if self.data.has_key("bom"):
            groups = self.data["bom"]
        if self.data.has_key("packages"):
            packages = self.data["packages"]
        return groups, packages

    def saveHash(self, path):
        f = self.filesystem.open(path, 'w')
        hashDictionary = miniUtility.hashDictionary(self.data)
        hashYaml = yaml.dump(hashDictionary)
        f.write(hashYaml)
        f.close()
        return OK

    def checkHash(self, path):
        oldConfig = {}
        try:
            yamlString = self.filesystem.open(path, 'r').read()
            oldConfig = yaml.load(yamlString)
        except IOError:
            #Logger.warning("Could not load saved configuration data in %s" % path)
            pass
        except:
            Logger.warning("Bad yaml in file %s" % path)
        newConfig = miniUtility.hashDictionary(self.data)
        #oldConfig = miniUtility.hashDictionary(oldConfig)
        difference =  miniUtility.diffDicts(oldConfig, newConfig, checkValues=True)
        return difference

    def readLocalConfig(self):
        configPath = os.path.join(miniUtility.getSpkgPath(), CONFIG_FILE)
        if not os.path.isfile(configPath):
            return FAIL
        fh = open(configPath, 'r')
        try:
            configData = fh.read()
            self.data = yaml.load(configData)
        except:
            return FAIL
        return OK
            
    def freshen(self):
        self.data = {}
        status = self.readLocalConfig()
        if status == FAIL:
            self.data = self.server.configRequest()
            self.makeConfigObject()
        self.username = DEFAULT_USER
        self.password = generatePassword()
        self.domain   = DEFAULT_DOMAIN
        if self.data.has_key("system"):
            username = self.data["system"].get("serviceuser")
            if username:
                self.username = username
            password = self.data["system"].get("servicepasswd")
            if password:
                self.password = password
            domain = self.data["system"].get("servicedomain")
            if domain:
                self.domain = domain
        return OK

    def makeConfigObject(self):
        self.config = ConfigParser.ConfigParser()
        for section in self.data.keys():
            if not self.config.has_section(section):
                self.config.add_section(section)
            datum = self.data[section]
            if type(datum) == type(dict()):
                for option in datum.keys():
                    value = datum[option]
                    if type(value) != type(dict()):
                        self.config.set(section, option, value)

    ### TESTED
    def set(self, section, option, value):
        if type(self.data.get(section)) != type(dict()):
            if self.data.get(section) != None:
                ermsg = "Clobbering data in configuration due to yaml/ini incompatibilities"
                Logger.warning(ermsg)
            self.data[section] = {}
        self.data[section][option] = value
        self.makeConfigObject()
        return OK

    def has_section(self, sectionQuery):
        for section in self.data.keys():
            if section.lower() == sectionQuery.lower():
                if type(self.data[section]) == type({}):
                    return True
        return False

    def has_option(self, sectionQuery, optionQuery):
        for section in self.data.keys():
            if section.lower() == sectionQuery.lower():
                sectData = self.data[section]
                if type(sectData) == type({}):
                    for option in sectData.keys():
                        if option.lower() == optionQuery.lower():
                            return True
        return False

    def options(self, section):
        output = []
        if self.config.has_section(section):
            output = self.config.options(section)
        for includeName in self.includes.keys():
            output += self.includes[includeName].options(section)
        return output

    ### TESTED
    def get(self, section, option, default='', optional=True):
        return str(self.get_raw(section, option, default))

    def get_dict(self, section, option, default={}, optional=True):
        result = self.get_raw(section, option, default)
        if result.__class__ == {}.__class__:
            return result
        else:
            raise TypeError

    def get_raw(self, section, option, default='', optional=True):
        if self.data.has_key(section):
            if self.data[section].has_key(option):
                return self.data[section][option]
        for key in self.data.keys():
            if key.lower() == section.lower():
                if type(self.data[key] == type({})):
                    for subkey in self.data[key].keys():
                        if subkey.lower() == option.lower():
                            return self.data[key][subkey]
        if default:
            if not self.data.has_key(section):
                self.data[section] = {}
            self.data[section][option] = default
            return default
        else:
            if self.data.has_key(section):
                raise ConfigParser.NoOptionError(option, section)
            else:
                raise ConfigParser.NoSectionError(section)

