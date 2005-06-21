#!/cygdrive/c/Python23/python.exe

import ConfigParser, yaml
from bombardier.staticData import *
import bombardier.miniUtility as miniUtility
import bombardier.Logger as Logger

def findParentList(data):
    parentList = []
    for key in data.keys():
        if key.upper() == "PARENTS":
            parents = data[key]
            for index, value in parents.iteritems():
                parentList.append(value)
    return parentList

class Config(dict):

    """This object retains the complete configuration for a system. It
    provides that information either as a dictionary or a
    ConfigParser. It will download its own configuration from the
    repository as well as following any 'parent' chains to create a
    single unified dictionary."""
    
    ### TESTED
    def __init__(self, filesystem, server, windows):
        self.filesystem = filesystem
        self.server     = server
        self.windows    = windows
        self.repository = None
        self.config     = ConfigParser.ConfigParser()
        self.parents    = []
        self.automated  = False
        self.data       = {}
        self.username   = None
        self.password   = None
        self.domain     = None

    def __getitem__(self, key):
        return self.data[key]

    def __setitem__(self, key, value):
        self.data[key] = value

    def getPackageGroups(self):
        if self.data.has_key("packageGroups"):
            if type(self.data["packageGroups"]) == type({}):
                packageGroups = []
                for groupId,packageGroup in self.data["packageGroups"].items():
                    packageGroups.append(packageGroup)
                return packageGroups
        if self.data.has_key("system"):
            if self.data["system"].has_key("type"):
                packageGroup = self.data["system"]["type"]
                if type(packageGroup) == type("string"):
                    return ["base", packageGroup]
        return []

    ### TESTED
    def downloadConfig(self, configName):
        Logger.debug("Downloading configuration data...")
        newData = self.server.serviceYamlRequest("clientconfig", 
                                                 {"client": configName, "type":"YAML"})
        self.data = miniUtility.addDictionaries(self.data, newData)
        self.makeConfigObject()
        newParents = findParentList(newData)
        self.loadParents(newParents)
        dumpFile = self.filesystem.open("config.yml", 'w')
        yaml.dumpToFile(dumpFile, self.data)
        dumpFile.flush()
        dumpFile.close()
        return OK

    def loadParents(self, newParents):
        for parentName in newParents:
            if parentName not in self.parents:
                self.parents.append(parentName)
                self.downloadConfig(parentName)

    def freshen(self):
        savedData = self.data
        self.data = {}
        status = self.downloadConfig(self.filesystem.environ["COMPUTERNAME"])
        if status == FAIL:
            self.data = savedData
            return FAIL
        if self.data.has_key("system"):
            self.username = self.data["system"].get("serviceuser")
            if self.username == None:
                errmsg = "Configuration file has no 'system/username' "\
                         "value: will not be able to get console access"
                Logger.error(errmsg)
                self.password = None
                self.domain   = None
            else:
                self.password = self.data["system"].get("servicepasswd")
                self.domain   = self.data["system"].get("servicedomain")
        else:
            errmsg = "Configuration file has no 'system' value: "\
                     "will not be able to get console access"
            Logger.error(errmsg)
            self.username = None
            self.password = None
            self.domain   = None
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
                    else:
                        ermsg =  "incompatible types (ini/yaml) for (%s:%s)" % (section, option)
                        Logger.warning(ermsg)
            else:
                pass
                Logger.warning("incompatible types (ini/yaml) for (%s)" % (section))

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
        for parentName in self.parents.keys():
            output += self.parents[parentName].options(section)
        return output

    ### TESTED
    def get(self, section, option, default=''):
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

