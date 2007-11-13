#!/usr/bin/python

import glob

import PinshCmd
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

    def name(self, tokens, index):
        possibleMatches = possibleHostNames(tokens[index])
        #possibleMatches = possibleHostNames(tokens[-1])
        if possibleMatches:
            return possibleMatches
        return ''

    def match(self, tokens, index):
        if tokens[index] == '':
            return NO_MATCH, 1
        possibleMatches = possibleHostNames(tokens[index])
        if len(possibleMatches) == 0:
            return NO_MATCH, 1
        elif len(possibleMatches) == 1:
            return COMPLETE, 1
        else:
            return PARTIAL, 1

if __name__ == "__main__":
    from libTest import *
    hostField = BomHostField()
    status = OK
    startTest()
    runTest(hostField.match, [["bigdb"]], (COMPLETE, 1), status)
    runTest(hostField.match, [["bigsam"]], (COMPLETE, 1), status)
    runTest(hostField.match, [["foo"]], (NO_MATCH, 1), status)
    runTest(hostField.match, [[""]], (NO_MATCH, 1), status)
    endTest(status)
