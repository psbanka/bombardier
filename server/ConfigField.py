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

    def name(self, tokens, index):
        hostNames = self.bomHostField.name(tokens, index-1)
        if len(hostNames) != 1:
            return ''
        hostName = hostNames[0]
        client = Client.Client(hostName, '')
        client.get()
        if tokens[index] == '':
            return client.data.keys()
        configValues = tokens[index].split('.')
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
                    possibleMatches.append("%s.%s" % (prefix, item))
                else:
                    possibleMatches.append(item)
        
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
