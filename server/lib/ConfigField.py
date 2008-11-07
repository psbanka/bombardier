#!/usr/bin/python

import glob, os

import PinshCmd, BomHostField, libCipher
import yaml, syck
import Client
from commonUtil import *
from bombardier.staticData import CENSORED

MERGED = 1
CLIENT = 2
INCLUDE = 3
BOM = 4

class ConfigField(PinshCmd.PinshCmd):
    def __init__(self, name = "configField", dataType=MERGED, strict=True):
        PinshCmd.PinshCmd.__init__(self, name, tokenDelimiter = '')
        self.helpText = "<configurationField>\ta dot-delimeted configuration value"
        self.bomHostField = BomHostField.BomHostField()
        self.level = 99
        self.dataType = dataType
        self.cmdOwner = 0
        self.strict = strict # take only exact matches
        if dataType in [CLIENT, MERGED]:
            self.directory = os.path.join(mode.serverHome, "client")
        if dataType == INCLUDE:
            self.directory = os.path.join(mode.serverHome, "include")
        if dataType == BOM:
            self.directory = os.path.join(mode.serverHome, "bom")

    def getTopLevelData(self, tokens, index, decrypt):
        partialFirst = tokens[index].split('.')[0]
        yamlFiles = glob.glob("%s/*.yml" % self.directory)
        fileNames = []
        for filename in yamlFiles:
            fileNames.append(filename.split('/')[-1].split('.yml')[0])
        firstTokenNames = [ fn for fn in fileNames if fn.lower().startswith(partialFirst.lower()) ]
        if len(firstTokenNames) == 0:
            return [], {}
        if tokens[index] in firstTokenNames:
            firstTokenNames = [tokens[index]]
        if len(firstTokenNames) > 1:
            return firstTokenNames, {}
        firstTokenName = firstTokenNames[0]
        if self.dataType == MERGED:
            client = Client.Client(firstTokenName, '', mode.serverHome)
            client.get()
            data = client.data
        else:
            data = syck.load(open("%s/%s.yml" % (self.directory, firstTokenName)).read())
        if decrypt:
            data = libCipher.decrypt(data, '')
        return [firstTokenName], data

    def removeValue(self, tokens, index):
        if self.dataType == MERGED:
            return FAIL, []

#        if newValue in currentValue:
#            currentValue.remove(newValue)
#            status, output = fieldObject.setValue(tokens, 2, currentValue)
#            return status, output + ["%s removed from list" % newValue]
#        try:
#            if int(newValue) in currentValue:
#                currentValue.remove(int(newValue))
#                status, output = fieldObject.setValue(tokens, 2, currentValue)
#                return status, output + ["%s removed from list" % newValue]
#        except:
#            pass
#        return FAIL, ["%s is not in the current list of values." % newValue]
        
        firstTokenNames, clearData = self.getTopLevelData(tokens, index, True)
        firstTokenNames, encData = self.getTopLevelData(tokens, index, False)
        if not clearData:
            return FAIL, []
        firstTokenName = firstTokenNames[0]
        configName = self.preferredNames(tokens, index)
        if configName == []:
            configName=[tokens[index]] # if this item doesn't exist
        execString = "del encData"
        currentDict = encData
        output = []
        for configValue in configName[0].split('.')[1:]:
            if configValue in currentDict:
                currentDict = currentDict.get(configValue)
                execString += "['%s']" % configValue
            else:
                configValue = "enc_" + configValue
                currentDict = currentDict.get(configValue)
                execString += "['%s']" % configValue
        exec( execString )
        string = yaml.dump(encData, default_flow_style=False)
        open("%s/%s.yml" % (self.directory, firstTokenName), 'w').write(string)
        return OK, output


    def setValue(self, tokens, index, newValue, encrypt):
        # FIXME: Need RemoveValue when converting to encrypted
        if self.dataType == MERGED:
            return FAIL, []
        if newValue == "{}":
            newValue = {}
        if newValue == "[]":
            newValue = []
        firstTokenNames, clearData = self.getTopLevelData(tokens, index, True)
        firstTokenNames, encData = self.getTopLevelData(tokens, index, False)
        if not clearData:
            return FAIL, []
        firstTokenName = firstTokenNames[0]
        configName = self.preferredNames(tokens, index)
        if configName == []:
            configName=[tokens[index]] # if this item doesn't exist
        execString = "encData"
        currentDict = clearData
        output = []
        if type(currentDict) == type(['list']):
            encData = newValue
        else:
            configTokens = configName[0].split('.')[1:]
            for configValue in configTokens[:-1]:
                currentDict = currentDict.get(configValue)
                execString += "['%s']" % configValue

            configValue = configTokens[-1]
            currentDict = currentDict.get(configValue)
            if currentDict == CENSORED or encrypt:
                if currentDict and currentDict != CENSORED:
                    self.removeValue(tokens, index)
                    firstTokenNames, encData = self.getTopLevelData(tokens, index, False)
                execString += "['enc_%s']" % configValue
                if not mode.password:
                    return FAIL, ["Cannot encipher data except in enable mode"]
                newValue = libCipher.encrypt(newValue, mode.password)
                output = ["Encrypted sensitive data"]
            else:
                execString += "['%s']" % configValue

            if type(newValue) == type('string'):
                execString += " = \"%s\"" % newValue
            else:
                execString += " = %s" % newValue
            exec( execString )
        string = yaml.dump(encData, default_flow_style=False)
        open("%s/%s.yml" % (self.directory, firstTokenName), 'w').write(string)
        return OK, output

    def getSpecificData(self, tokens, index):
        tokens[index] = tokens[index].replace('"', '')
        firstTokenNames, data = self.getTopLevelData(tokens, index, True)
        if len(firstTokenNames) != 1:
            return '' # EXPERIMENTAL CHANGE from []
        if len(tokens[index].split('.')) > 1:
            configName = self.preferredNames(tokens, index)
            if len(configName) == 0:
                return '' # EXPERIMENTAL CHANGE from []
            if len(configName) > 1:
                return configName
            currentDict = data
            for configValue in configName[0].split('.')[1:]:
                configValue = configValue.replace('"', '')
                if type(currentDict) == type({}):
                    currentDict = currentDict.get(configValue)
                else:
                    return currentDict
        else:
            currentDict = data
        return currentDict

    def preferredNames(self, tokens, index):
        tokens[index] = tokens[index].replace('"', '')
        if not self.strict:
            return tokens[index:]
        firstTokenNames, data = self.getTopLevelData(tokens, index, True)
        if len(firstTokenNames) == 0:
            return []
        if len(firstTokenNames) > 1:
            return firstTokenNames
        firstTokenName = firstTokenNames[0]
        if len(tokens[index].split('.')) == 1:
            return [firstTokenName]
        configValues = tokens[index].split('.')[1:]
        #print "PN: configValues", configValues, data
        currentDict = None
        if type(data) == type({}):
            currentDict = data
            for configValue in configValues[:-1]:
                configValue = configValue.replace('"', '')
                newCurrentDict = currentDict.get(configValue)
                if type(newCurrentDict) in [type({})]:
                    currentDict = newCurrentDict
                else:
                    currentDict = []
                    break
        #print "PN::: currentDict",currentDict
        possibleMatches = []
        prefix = '.'.join(configValues[:-1])
        if currentDict == None:
            if prefix:
                return ["%s.%s" % (firstTokenName, prefix)]
            else:
                return ["%s" % (firstTokenName)]
        for item in currentDict:
            testValue = configValues[-1].replace('"','').lower()
            if item.lower().startswith(testValue):
                if prefix:
                    possibleMatches.append("%s.%s.%s" % (firstTokenName, prefix, item))
                else:
                    possibleMatches.append("%s.%s" % (firstTokenName, item))

        if possibleMatches:
            return possibleMatches
        return []

    def match(self, tokens, index):
        possibleMatches = self.acceptableNames(tokens, index)
        if not possibleMatches:
            return NO_MATCH, 1
        if len(possibleMatches) > 1:
            return PARTIAL, 1
        return COMPLETE, 1

if __name__ == "__main__":
    from libTest import startTest, runTest, endTest
    configField = ConfigField(dataType = CLIENT, strict=True)
    status = OK
    startTest()
#    status = runTest(configField.preferredNames, [["lila"], 0], ["lilap"], status)
#    status = runTest(configField.preferredNames, [["bigdb.sql.servers"], 0], ["bigdb.sql.servers"], status)
#    status = runTest(configField.preferredNames, [["bigdb.sq"], 0], ["bigdb.sql"], status)
#    status = runTest(configField.preferredNames, [["bigdb.sql"], 0], ["bigdb.sql"], status)
#    status = runTest(configField.preferredNames, [["bigsam.ipAddress"], 0], ["bigsam.ipAddress"], status)
#    status = runTest(configField.preferredNames, [["bigsam.ipAddress."], 0], ["bigsam.ipAddress"], status)
#    status = runTest(configField.preferredNames, [["virtap.connectTest.connectionData.pro"], 0], ["virtap.connectTest.connectionData.proxy"], status)
#    status = runTest(configField.preferredNames, [["foo.foo"], 0], [], status)
#    status = runTest(configField.preferredNames, [["bigsam.thingy.majig"], 0], ['bigsam.thingy'], status)
#    status = runTest(configField.preferredNames, [["big"], 0], ["bigap", "bigsam", "bigdb"], status)
#    status = runTest(configField.preferredNames, [["sho","server","l"], 2], ["lilap", "lildb", "ltdb", "ldapserver"], status)
#    configField = ConfigField(dataType=CLIENT)
#    status = runTest(configField.preferredNames, [["lilap.s"], 0], ["lilap.sharedKeys"], status)


    #status = runTest(configField.setValue, [["lilap.sharedKeys"], 0, "yes", False], (OK, []), status)
    #status = runTest(configField.getSpecificData, [["lilap.sharedKeys"], 0], "yes", status)

    status = runTest(configField.setValue, [["lilap.nonsense"], 0, "yes", False], (OK, []), status)
    status = runTest(configField.getSpecificData, [["lilap.nonsense"], 0], "yes", status)
    status = runTest(configField.removeValue, [["lilap.nonsense"], 0], (OK, []), status)
    status = runTest(configField.setValue, [["lilap.nonsense"], 0, "{}", False], (OK, []), status)
    status = runTest(configField.setValue, [["lilap.nonsense.nonsense"], 0, "yes", False], (OK, []), status)
    status = runTest(configField.setValue, [["lilap.nonsense"], 0, "yes", False], (OK, []), status)
    mode.password = "abcd1234"
    status = runTest(configField.setValue, [["lilap.nonsense"], 0, "yes", True], (OK, ['Encrypted sensitive data']), status)
    status = runTest(configField.setValue, [["lilap.nonsense"], 0, "yes", False], (OK, ['Encrypted sensitive data']), status)
    status = runTest(configField.removeValue, [["lilap.nonsense"], 0], (OK, []), status)

#    status = runTest(configField.setValue, [["bigdb.sharedKeys"], 0], (OK, []), status)
#    status = runTest(configField.preferredNames, [["lilap"], 0], ["lilap"], status)
#    status = runTest(configField.match, [["bigd"], 0], (COMPLETE, 1), status)
#    status = runTest(configField.match, [["bigdb", ""], 0], (COMPLETE, 1), status)
#    status = runTest(configField.match, [["lilap.foo"], 0], (NO_MATCH, 1), status)
#    status = runTest(configField.match, [["lilap"], 0], (COMPLETE, 1), status)
#    status = runTest(configField.match, [["lilap.ipAddress"], 0], (COMPLETE, 1), status)
#    status = runTest(configField.match, [["foo"], 0], (NO_MATCH, 1), status)
#    configField = ConfigField(dataType=INCLUDE)
#    status = runTest(configField.preferredNames, [["serviceNet.ca"], 0], ["serviceNet.cas"], status)
#    status = runTest(configField.preferredNames, [["servicenet"], 0], ["serviceNet"], status)
#    status = runTest(configField.getSpecificData, [["testInclude.thing1"], 0], '=== CENSORED ===', status)
#    status = runTest(configField.setValue, [["testInclude.thing1"], 0], (FAIL, ['Cannot encipher data except in enable mode']), status)
#    mode.password = "abcd1234"
#    status = runTest(configField.setValue, [["testInclude.thing1"], 0], (OK, ['Encrypted sensitive data']), status)
#    status = runTest(configField.getSpecificData, [["testInclude.thing1"], 0], '=== CENSORED ===', status)
#    status = runTest(configField.getSpecificData, [["testInclude.platform"], 0], "win32", status)
#    status = runTest(configField.setValue, [["testInclude.platform"], 0], (OK, []), status)
#    status = runTest(configField.getSpecificData, [["testInclude.platform"], 0], "foo", status)
#    status = runTest(configField.setValue, [["testInclude.platform"], 0], (OK, []), status)
#    status = runTest(configField.getSpecificData, [["testInclude.platform"], 0], "win32", status)
#    status = runTest(configField.getSpecificData, [["testInclude.sql.databases"], 0], ['casDB', 'mcsDB', 'rmsDB', 'uhrDB'], status)
#    #status = runTest(configField.setValue, [["testInclude.sql.databases"], 0, ['casDB', 'mcsDB', 'rmsDB', 'uhrDB', 'foo']], (OK, []), status)
#    #status = runTest(configField.getSpecificData, [["testInclude.sql.databases"], 0], ['casDB', 'mcsDB', 'rmsDB', 'uhrDB', 'foo'], status)
#    #status = runTest(configField.setValue, [["testInclude.sql.databases"], 0, ['casDB', 'mcsDB', 'rmsDB', 'uhrDB']], (OK, []), status)
#    #status = runTest(configField.getSpecificData, [["testInclude.sql.databases"], 0], ['casDB', 'mcsDB', 'rmsDB', 'uhrDB'], status)
#    status = runTest(configField.getSpecificData, [["testInclude.platform"], 0], "win32", status)
#    status = runTest(configField.getSpecificData, [['testInclude.thingWithoutSpaces.thisIsAThing'], 0], "wuzz", status)
#    status = runTest(configField.getSpecificData, [['testInclude."thingWithoutSpaces"."thisIsAThing"'], 0], "wuzz", status)
#    status = runTest(configField.getSpecificData, [['testInclude."Thing With Spaces"."this is a thing"'], 0], "fuzz", status)
#    status = runTest(configField.getSpecificData, [['"testInclude.Thing With Spaces.this is a thing"'], 0], "fuzz", status)
#    configField = ConfigField(dataType=BOM)
#    status = runTest(configField.preferredNames, [["testB"], 0], ["testbom"], status)
#    status = runTest(configField.preferredNames, [["testbom.DbAuth"], 0], ["testbom"], status)
#    status = runTest(configField.preferredNames, [['sho', 'bom', 'testb'], 2], ["testbom"], status)
    endTest(status)
