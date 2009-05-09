#!/cygdrive/c/Python24/python.exe

# Config.py: This class provides the functionality of providing
# heirarchichal configuration data through either a dictionary
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

import yaml
from old_static_data import *
from bombardier_common.miniUtility import hashDictionary, diffDicts, getSpkgPath
import random, copy
from Exceptions import InvalidConfigData
from bombardier_common.Logger import Logger

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

    """This object retains the complete configuration for a system."""

    ### TESTED
    def __init__(self, filesystem, instanceName, configData = {}):
        self.filesystem = filesystem
        self.instanceName = instanceName
        self.includes   = []
        self.automated  = False
        self.username   = None
        self.password   = None
        self.domain     = None
        self.data       = configData

        self.readLocalConfig()

    def __getitem__(self, key):
        return self.data[key]

    def __setitem__(self, key, value):
        self.data[key] = value

    def keys(self):
        return self.data.keys()

    def getInstance(self):
        return self.instanceName

    def setBomPackages(self, pDict):
        self.data["packages"] = copy.deepcopy(pDict)

    def getBomPackages(self):
        if self.data.has_key("packages"):
            return copy.deepcopy(self.data["packages"])
        return []

    def saveHash(self, path):
        f = self.filesystem.open(path, 'w')
        hashDictionary = hashDictionary(self.data)
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
        newConfig = hashDictionary(self.data)
        difference =  diffDicts(oldConfig, newConfig, checkValues=True)
        return difference

    def readLocalConfig(self):
        configPath = os.path.join(getSpkgPath(), self.instanceName, CONFIG_FILE)
        if not os.path.isfile(configPath):
            return FAIL
        msg = "DETECTED A CLEARTEXT LOCAL CONFIGURATION FILE: IGNORING MANAGEMENT SERVER CONFIGURATION"
        Logger.warning(msg)
        fh = open(configPath, 'r')
        try:
            configData = fh.read()
            self.data = yaml.load(configData)
        except:
            return FAIL
        return OK

    ### TESTED
    def set(self, section, option, value):
        if type(self.data.get(section)) != type(dict()):
            if self.data.get(section) != None:
                ermsg = "Clobbering data in configuration due to yaml/ini incompatibilities"
                Logger.warning(ermsg)
            self.data[section] = {}
        self.data[section][option] = value
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

    ### TESTED
    def get(self, section, option, default='', optional=True):
        pyChucker(optional)
        return str(self.get_raw(section, option, default))

    def get_dict(self, section, option, default={}, optional=True):
        pyChucker(optional)
        result = self.get_raw(section, option, default)
        if result.__class__ == {}.__class__:
            return result
        else:
            raise TypeError

    def get_raw(self, section, option, default=None, optional=True):
        pyChucker(optional)
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
                raise InvalidConfigData("Option %s not found for section %s" % (option, section), None, None)
            else:
                raise InvalidConfigData("Section %s not found" % section, None, None)

    def parseSection(self, sectionString, default, optional):
        sections = sectionString.split('.')
        d = self.data
        for section in sections:
            try:
                d = d[section]
            except:
                d = None
                break
        if d == None:
            if not optional:
                raise InvalidConfigData("Option %s not found" % sectionString, None, None)
            d = default
        return d

    def getobj(self, sectionString, default, expType, optional):
        value = self.parseSection(sectionString, default, optional)        
        if type(expType) == type("string"):
            if type(value) == type(1234) or type(value) == type(123.32):
                value = str(value)
        if type(value) == type(expType):
            return value
        raise InvalidConfigData(sectionString, type(value), type(expType))

    def listobj(self, sectionString, default=[], optional=True):
        return self.getobj(sectionString, default, [], optional)

    def string(self, sectionString, default='', optional=True):
        return self.getobj(sectionString, default, "string", optional)

    def integer(self, sectionString, default=1, optional=True):
        return self.getobj(sectionString, default, 1, optional)

    def dictionary(self, sectionString, default={}, optional=True):
        return self.getobj(sectionString, default, {}, optional)
