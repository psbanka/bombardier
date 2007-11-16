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
        if dataType in [CLIENT, MERGED]:
            self.directory = "deploy/client"
        if dataType == INCLUDE:
            self.directory = "deploy/include"
        if dataType == BOM:
            self.directory = "deploy/bom"


    def getTopLevelData(self, tokens, index):
        partialFirst = tokens[index].split('.')[0]
        yamlFiles = glob.glob("%s/*.yml" % self.directory)
        fileNames = []
        for filename in yamlFiles:
            fileNames.append(filename.split('/')[-1].split('.yml')[0])
        firstTokenNames = [ x for x in fileNames if x.lower().startswith(partialFirst.lower()) ]
        if len(firstTokenNames) == 0:
            return [], {}
        if len(firstTokenNames) > 1:
            return firstTokenNames, {}
        firstTokenName = firstTokenNames[0]
        if self.dataType == MERGED:
            client = Client.Client(firstTokenName, '')
            client.get()
            data = client.data
        else:
            data = yaml.load(open("%s/%s.yml" % (self.directory, firstTokenName)).read())
        return [firstTokenName], data

    def setValue(self, tokens, index, newValue): # CLOSE TO GENERIC
        if self.dataType == MERGED:
            return FAIL
        firstTokenNames, data = self.getTopLevelData(tokens, index)
        if not data:
            return FAIL
        firstTokenName = firstTokenNames[0]
        configName = self.name(tokens, index)
        cmd = "data"
        currentDict = data
        if type(currentDict) == type(['list']):
            data = newValue
        else:
            for configValue in configName[0].split('.')[1:]:
                cmd += "['%s']" % configValue
                currentDict = currentDict.get(configValue)
                if type(newValue) == type('string'):
                    thing = "%s = \"%s\"" % (cmd, newValue ) 
                else:
                    thing = "%s = %s" % (cmd, newValue)
            exec( thing )
        string = yaml.dump(data, default_flow_style=False)
        open("%s/%s.yml" % (self.directory, firstTokenName), 'w').write(string)
        return OK

    def getSpecificData(self, tokens, index):
        firstTokenNames, data = self.getTopLevelData(tokens, index)
        if len(firstTokenNames) != 1:
            return []
        if len(tokens[index].split('.')) > 1:
            configName = self.name(tokens, index)
            if len(configName) == 0:
                return []
            if len(configName) > 1:
                return configName
            currentDict = data
            for configValue in configName[0].split('.')[1:]:
                if type(currentDict) == type({}):
                    currentDict = currentDict.get(configValue)
                else:
                    return currentDict
        else:
            currentDict = data
        return currentDict

    def name(self, tokens, index):
        firstTokenNames, data = self.getTopLevelData(tokens, index)
        if len(firstTokenNames) == 0:
            return ''
        if len(firstTokenNames) > 1:
            return firstTokenNames
        firstTokenName = firstTokenNames[0]
        if len(tokens[index].split('.')) == 1:
            return [firstTokenName]
        configValues = tokens[index].split('.')[1:]
        currentDict = []
        if type(data) == type({}):
            currentDict = data
            for configValue in configValues[:-1]:
                newCurrentDict = currentDict.get(configValue)
                if type(newCurrentDict) in [type({})]:
                    currentDict = newCurrentDict
                else:
                    currentDict = []
                    break
        possibleMatches = []
        prefix = '.'.join(configValues[:-1])
        if not currentDict:
            if prefix:
                return ["%s.%s" % (firstTokenName, prefix)]
            else:
                return ["%s" % (firstTokenName)]
        for item in currentDict:
            if item.lower().startswith(configValues[-1].lower()):
                if prefix:
                    possibleMatches.append("%s.%s.%s" % (firstTokenName, prefix, item))
                else:
                    possibleMatches.append("%s.%s" % (firstTokenName, item))
        
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
    #status = runTest(configField.name, [["lila"], 0], ["lilap"], status)
    #status = runTest(configField.name, [["bigdb.sql.servers"], 0], ["bigdb.sql.servers"], status)
    #status = runTest(configField.name, [["bigdb.sq"], 0], ["bigdb.sql"], status)
    #status = runTest(configField.name, [["bigdb.sql"], 0], ["bigdb.sql"], status)
    #status = runTest(configField.name, [["bigsam.ipAddress"], 0], ["bigsam.ipAddress"], status)
    #status = runTest(configField.name, [["bigsam.ipAddress."], 0], ["bigsam.ipAddress"], status)
    #status = runTest(configField.name, [["virtap.connectTest.connectionData.pro"], 0], ["virtap.connectTest.connectionData.proxy"], status)
    #status = runTest(configField.name, [["foo.foo"], 0], '', status)
    #status = runTest(configField.name, [["bigsam.thingy.majig"], 0], ['bigsam.thingy'], status)
    #status = runTest(configField.name, [["big"], 0], ["bigap", "bigsam", "bigdb"], status)
    #status = runTest(configField.name, [["sho","server","l"], 2], ["lilap", "lildb", "ltdb"], status)
    #configField = ConfigField(dataType=CLIENT)
    #status = runTest(configField.name, [["lilap.s"], 0], ["lilap.sharedKeys"], status)
    #status = runTest(configField.setValue, [["lilap.sharedKeys"], 0, "yes"], OK, status)
    #status = runTest(configField.getSpecificData, [["lilap.sharedKeys"], 0], "yes", status)
    #status = runTest(configField.setValue, [["bigdb.sharedKeys"], 0, "yup"], OK, status)
    #status = runTest(configField.name, [["lilap"], 0], ["lilap"], status)
    #status = runTest(configField.match, [["bigd"], 0], (COMPLETE, 1), status)
    #status = runTest(configField.match, [["bigdb", ""], 0], (COMPLETE, 1), status)
    #status = runTest(configField.match, [["lilap.foo"], 0], (NO_MATCH, 1), status)
    #status = runTest(configField.match, [["lilap"], 0], (COMPLETE, 1), status)
    #status = runTest(configField.match, [["lilap.ipAddress"], 0], (COMPLETE, 1), status)
    #status = runTest(configField.match, [["foo"], 0], (NO_MATCH, 1), status)
    configField = ConfigField(dataType=INCLUDE)
    #status = runTest(configField.name, [["serviceNet.ca"], 0], ["serviceNet.cas"], OK, status)
    #status = runTest(configField.name, [["servicenet"], 0], ["serviceNet"], OK, status)
    #status = runTest(configField.getSpecificData, [["testInclude.platform"], 0], "win32", status)
    #status = runTest(configField.setValue, [["testInclude.platform"], 0, "foo"], OK, status)
    #status = runTest(configField.getSpecificData, [["testInclude.platform"], 0], "foo", status)
    #status = runTest(configField.setValue, [["testInclude.platform"], 0, "win32"], OK, status)
    #status = runTest(configField.getSpecificData, [["testInclude.platform"], 0], "win32", status)
    status = runTest(configField.getSpecificData, [["testInclude.sql.databases"], 0], ['casDB', 'mcsDB', 'rmsDB', 'uhrDB'], status)
    status = runTest(configField.setValue, [["testInclude.sql.databases"], 0, ['casDB', 'mcsDB', 'rmsDB', 'uhrDB', 'foo']], OK, status)
    status = runTest(configField.getSpecificData, [["testInclude.sql.databases"], 0], ['casDB', 'mcsDB', 'rmsDB', 'uhrDB', 'foo'], status)
    status = runTest(configField.setValue, [["testInclude.sql.databases"], 0, ['casDB', 'mcsDB', 'rmsDB', 'uhrDB']], OK, status)
    status = runTest(configField.getSpecificData, [["testInclude.sql.databases"], 0], ['casDB', 'mcsDB', 'rmsDB', 'uhrDB'], status)
    #status = runTest(configField.getSpecificData, [["testInclude.platform"], 0], "win32", status)
    #configField = ConfigField(dataType=BOM)
    #status = runTest(configField.name, [["testB"], 0], ["testbom"], OK, status)
    #status = runTest(configField.name, [["testbom.DbAuth"], 0], ["testbom"], OK, status)
    #status = runTest(configField.name, [['sho', 'bom', 'tes'], 2], ["testbom"], OK, status)
    endTest(status)
