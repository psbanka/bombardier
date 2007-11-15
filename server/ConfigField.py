#!/usr/bin/python

import glob

import PinshCmd, BomHostField, yaml
import Client
from commonUtil import *


MERGED = 1
CLIENT = 2
INCLUDE = 3
BOM = 4

class ConfigField(PinshCmd.PinshCmd):
    def __init__(self, name = "configField", dataType=MERGED):
        PinshCmd.PinshCmd.__init__(self, name, tokenDelimiter = '')
        self.helpText = "<configurationField>\ta dot-delimeted configuration value"
        self.bomHostField = BomHostField.BomHostField()
        self.level = 99
        self.dataType = dataType
        self.cmdOwner = 0

    def setValue(self, tokens, index, newValue):
        if self.dataType == MERGED:
            return FAIL
        elif self.dataType == CLIENT:
            hostNames = self.getHostNames(tokens, index)
            if len(hostNames) != 1:
                return FAIL
            hostName = hostNames[0]
            data = self.getClientData(hostName)
            currentDict = data
            configName = self.name(tokens, index)
            cmd = "data"
            for configValue in configName[0].split('.')[1:]:
                cmd += "['%s']" % configValue
                currentDict = currentDict.get(configValue)
            if type(newValue) == type('string'):
                thing = "%s = \"%s\"" % (cmd, newValue ) 
            if type(newValue) == type(['list']):
                thing = "%s = %s" % (cmd, newValue ) 
            exec( thing )
            self.writeClientData(hostName, data)
            return OK

    def getHostNames(self, tokens, index):
        hostNames = self.bomHostField.name([tokens[index].split('.')[0]], 0)
        if len(hostNames) == 0:
            return []
        return hostNames

    def writeClientData(self, hostName, data):
        #print yaml.dump(data, default_flow_style=False)
        open("deploy/client/%s.yml" % hostName, 'w').write(yaml.dump(data, default_flow_style=False))
        return OK

    def getClientData(self, hostName):
        return yaml.load(open("deploy/client/%s.yml" % hostName).read())

    def getConfigData(self, tokens, index):
        hostNames = self.getHostNames(tokens, index)
        if len(hostNames) == 0:
            return []
        if len(hostNames) > 1:
            return hostNames
        else:
            hostName = hostNames[0]
        if self.dataType == MERGED:
            client = Client.Client(hostName, '')
            client.get()
            data = client.data
        elif self.dataType == CLIENT:
            data = self.getClientData(hostName)
        if len(tokens[index].split('.')) > 1:
            configName = self.name(tokens, index)
            if len(configName) == 0:
                return []
            if len(configName) > 1:
                return []
            currentDict = data
            for configValue in configName[0].split('.')[1:]:
                currentDict = currentDict.get(configValue)
        else:
            currentDict = data
        return currentDict

    def name(self, tokens, index):
        if self.dataType in [MERGED, CLIENT]:
            hostNames = self.getHostNames(tokens, index)
            if len(tokens[index].split('.')) == 1:
                return hostNames
            if len(hostNames) == 0:
                return ''
            hostName = hostNames[0]
            completeFirstToken = hostName
        if self.dataType == INCLUDE:
            print "NEED TO CHECK INCLUDE FILE"
            includeFile = tokens[index].split('.')[0]
            completeFirstToken = includeFile
        if self.dataType == MERGED:
            client = Client.Client(hostName, '')
            client.get()
            data = client.data
        elif self.dataType == CLIENT:
            data = yaml.load(open("deploy/client/%s.yml" % hostName).read())
        elif self.dataType == INCLUDE:
            data = yaml.load(open("deploy/include/%s.yml" % includeFile).read())
        else:
            print "Unknown data type"
            return ''
        if len(tokens[index].split('.')) == 1:
            return data.keys()

        configValues = tokens[index].split('.')[1:]
        currentDict = data
        for configValue in configValues[:-1]:
            if type(currentDict) == type({}):
                currentDict = currentDict.get(configValue)
            else:
                return ''
            if not type(currentDict) in [ type({}), type(['list']) ]:
                return ''
        possibleMatches = []
        prefix = '.'.join(configValues[:-1])
        for item in currentDict:
            if item.lower().startswith(configValues[-1].lower()):
                if prefix:
                    possibleMatches.append("%s.%s.%s" % (completeFirstToken, prefix, item))
                else:
                    possibleMatches.append("%s.%s" % (completeFirstToken, item))
        
        if possibleMatches:
            return possibleMatches
        return ''

    def match(self, tokens, index):
        possibleMatches = self.name(tokens, index)
        if not possibleMatches:
            return NO_MATCH, 1
        if len(possibleMatches) > 1:
            return PARTIAL, 1
        return COMPLETE, 1

if __name__ == "__main__":
    from libTest import *
    configField = ConfigField()
    status = OK
    startTest()
    status = runTest(configField.name, [["lila"], 0], ["lilap"], status)
    status = runTest(configField.name, [["bigdb.sql.servers"], 0], ["bigdb.sql.servers"], status)
    status = runTest(configField.name, [["bigsam.ipAddre"], 0], ["bigsam.ipAddress"], status)
    status = runTest(configField.name, [["bigsam.ipAddress"], 0], ["bigsam.ipAddress"], status)
    status = runTest(configField.name, [["virtap.connectTest.connectionData.pro"], 0], ["virtap.connectTest.connectionData.proxy"], status)
    status = runTest(configField.name, [["foo.foo"], 0], '', status)
    status = runTest(configField.name, [["bigsam.thingy.majig"], 0], '', status)
    status = runTest(configField.name, [["big"], 0], ["bigap", "bigsam", "bigdb"], status)
    status = runTest(configField.name, [["sho","server","l"], 2], ["lilap", "lildb", "ltdb"], status)
    configField = ConfigField(dataType=CLIENT)
    status = runTest(configField.name, [["lilap.s"], 0], ["lilap.sharedKeys"], status)
    status = runTest(configField.setValue, [["lilap.sharedKeys"], 0, "yes"], OK, status)
    status = runTest(configField.setValue, [["bigdb.sharedKeys"], 0, ["yup"]], OK, status)
    data = configField.getConfigData(["bigdb.sharedKeys"], 0)
    assert data == ["yup"], data
    status = runTest(configField.setValue, [["bigdb.sharedKeys"], 0, "yup"], OK, status)
    configField = ConfigField(dataType=INCLUDE)
    status = runTest(configField.name, [["serviceNet.ca"], 0], ["serviceNet.cas"], OK, status)
    status = runTest(configField.name, [["servicenet.ca"], 0], ["serviceNet.cas"], OK, status)
    endTest(status)
