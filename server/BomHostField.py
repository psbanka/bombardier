#!/usr/bin/python

import glob

import PinshCmd, Client
from commonUtil import *

class BomHostField(PinshCmd.PinshCmd):
    def __init__(self, enabled = True):
        PinshCmd.PinshCmd.__init__(self, name = "bomhostname")
        self.helpText = "<hostname>\tthe name of a bombardier client"
        self.cmdOwner = 0
        self.enabled = enabled
        self.allowAny = False

    def possibleHostNames(self, hostName):
        if not self.enabled:
            yamlFiles = glob.glob(mode.dataPath+"/deploy/client/*.yml")
            allHostNames = []
            for filename in yamlFiles:
                allHostNames.append(filename.split('/')[-1].split('.yml')[0])
            hostNames = list(set(allHostNames) - set(mode.config["enabledSystems"]))
        else:
            hostNames = mode.config["enabledSystems"]
        possibleMatches = [ hn for hn in hostNames if hn.lower().startswith(hostName.lower()) ]
        return possibleMatches

    def ipAddress(self, hostName):
        if hostName in self.possibleHostNames(hostName):
            client = Client.Client(hostName, '', mode.dataPath)
            client.downloadClient()
            return client.data.get("ipAddress")
        return hostName

    def preferredNames(self, tokens, index):
        possibleMatches = self.possibleHostNames(tokens[index])
        if possibleMatches:
            return possibleMatches
        return []

    def acceptableNames(self, tokens, index):
        acceptableNames = self.preferredNames(tokens, index)
        if self.allowAny and tokens[index]:
            if tokens[index] not in acceptableNames:
                acceptableNames.insert(0, tokens[index])
        return acceptableNames

    def match(self, tokens, index):
        possibleMatches = self.acceptableNames(tokens, index)
        if not possibleMatches:
            return NO_MATCH, 1
        if len(possibleMatches) > 1:
            return PARTIAL, 1
        return COMPLETE, 1


if __name__ == "__main__":
    # A server must be enabled for the tests to work properly.
    from libTest import startTest, runTest, endTest
    hostField = BomHostField()
    status = OK
    startTest()
    status = runTest(hostField.match, [["bigdb"], 0], (COMPLETE, 1), status)
    status = runTest(hostField.match, [["bigsam"], 0], (COMPLETE, 1), status)
    status = runTest(hostField.match, [["foo"], 0], (NO_MATCH, 1), status)
    status = runTest(hostField.match, [[""], 0], (PARTIAL, 1), status)
    status = runTest(hostField.preferredNames, [["big"], 0], ['bigap', 'bigdb', 'bigsam'], status)
    hostField.allowAny = True
    status = runTest(hostField.acceptableNames, [["12.22.13.0"], 0], ['12.22.13.0'], status)
    endTest(status)
