#!/usr/bin/python

import sys

import PinshCmd
from commonUtil import *

BAD_CHARACTERS = ['*', '&', ' ', '(', '/', '%', '^', ')', '$', '#', '!', ';']

class HostField(PinshCmd.PinshCmd):
    def __init__(self, name = "hostname"):
        PinshCmd.PinshCmd.__init__(self, name)
        self.helpText = "<hostname>\tthe name of a host"
        self.min = min
        self.max = max
        self.level = 99
        self.cmdOwner = 0

    def match(self, tokens, index):
        possibleMatches = self.acceptableNames(tokens, index)
        if not possibleMatches:
            return NO_MATCH, 1
        if len(possibleMatches) > 1:
            return PARTIAL, 1
        return COMPLETE, 1

    def preferredNames(self, tokens, index):
        if tokens[index] == '':
            return []
        cleanedUp = tokens[index]
        for badCharacter in BAD_CHARACTERS:
            cleanedUp = cleanedUp.replace(badCharacter, '')
        return [cleanedUp]

if __name__ == "__main__":
    from libTest import startTest, runTest, endTest
    hostField = HostField()
    status = OK
    startTest()
    status = runTest(hostField.preferredNames, [["foo"], 0], ["foo"], status)
    status = runTest(hostField.preferredNames, [["www.foo.com"], 0], ["www.foo.com"], status)
    status = runTest(hostField.preferredNames, [["a;sdjf#!"], 0], ["asdjf"], status)
    status = runTest(hostField.preferredNames, [["www.^DH.com"], 0], ["www.DH.com"], status)
    status = runTest(hostField.match, [[""], 0], (NO_MATCH, 1), status)
    endTest(status)
