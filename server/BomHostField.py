#!/usr/bin/python

import glob

import PinshCmd, Client
from commonUtil import *

def possibleHostNames(hostName):
    yamlFiles = glob.glob("deploy/client/*.yml")
    hostNames = []
    for filename in yamlFiles:
        hostNames.append(filename.split('/')[-1].split('.yml')[0])
    possibleMatches = [ x for x in hostNames if x.lower().startswith(hostName.lower()) ]
    return possibleMatches

class BomHostField(PinshCmd.PinshCmd):
    def __init__(self, name = "bomhostname"):
        PinshCmd.PinshCmd.__init__(self, name)
        self.helpText = "<hostname>\tthe name of a bombardier client"
        self.min = min
        self.max = max
        self.level = 99
        self.cmdOwner = 0

    def ipAddress(self, hostName):
        client = Client.Client(hostName, '')
        client.downloadClient()
        return client.data.get("ipAddress")

    def name(self, tokens, index):
        possibleMatches = possibleHostNames(tokens[index])
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
    hostField = BomHostField()
    status = OK
    startTest()
    status = runTest(hostField.match, [["bigdb"], 0], (COMPLETE, 1), status)
    status = runTest(hostField.match, [["bigsam"], 0], (COMPLETE, 1), status)
    status = runTest(hostField.match, [["foo"], 0], (NO_MATCH, 1), status)
    status = runTest(hostField.match, [[""], 0], (PARTIAL, 1), status)
    status = runTest(hostField.name, [["big"], 0], ['bigap', 'bigsam', 'bigdb'], status)
    endTest(status)
