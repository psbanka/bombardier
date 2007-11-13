#!/usr/bin/python

import glob

import PinshCmd, BomHostField, yaml
import Client
from commonUtil import *



class ConfigField(PinshCmd.PinshCmd):
    def __init__(self, name = "configField"):
        PinshCmd.PinshCmd.__init__(self, name, tokenDelimiter = '.')
        self.helpText = "<configurationField>\ta dot-delimeted configuration value"
        self.bomHostField = BomHostField.BomHostField()
        self.level = 99
        self.cmdOwner = 0

    def getConfigData(self, tokens, index):
        hostNames = self.bomHostField.name([tokens[index].split('.')[0]], 0)
        if len(hostNames) == 0:
            return []
        if len(hostNames) > 1:
            return hostNames
            #return FAIL, ["No server %s" % tokens[index]]
        else:
            hostName = hostNames[0]
        client = Client.Client(hostName, '')
        client.get()
        if len(tokens[index].split('.')) > 1:
            configName = self.name(tokens, index)
            if len(configName) == 0:
                return []
                #return FAIL, ["Unknown configuration option"]
            if len(configName) > 1:
                #return FAIL, ["Incomplete configuration option"]
                return []
            currentDict = client.data
            for configValue in configName[0].split('.')[1:]:
                currentDict = currentDict.get(configValue)
        else:
            currentDict = client.data
        return currentDict

    def name(self, tokens, index):
        hostNames = self.bomHostField.name([tokens[index].split('.')[0]], 0)
        if len(hostNames) != 1:
            if len(tokens[index].split('.')) == 1:
                return hostNames 
            return ''
        client = Client.Client(hostName, '')
        client.get()
        if len(tokens[index].split('.')) == 1:
            return client.data.keys()

        configValues = tokens[index].split('.')[1:]
        currentDict = client.data
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
                    possibleMatches.append("%s.%s.%s" % (hostName, prefix, item))
                else:
                    possibleMatches.append("%s.%s" % (hostName, item))
        
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
    runTest(configField.name, [["bigdb", "sql.servers"], 1], ["sql.servers"], status)
    runTest(configField.name, [["bigsam", "ipAddre"], 1], ["ipAddress"], status)
    runTest(configField.name, [["virtap", "connectTest.connectionData.pro"], 1], ["connectTest.connectionData.proxy"], status)
    runTest(configField.name, [["foo", "foo"], 1], '', status)
    runTest(configField.name, [["bigsam", "thingy.majig"], 1], '', status)
    endTest(status)
