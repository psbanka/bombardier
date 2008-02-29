#!/usr/bin/python

import sys

import PinshCmd
from commonUtil import *

class Variable(PinshCmd.PinshCmd):
    def __init__(self, name = "variable"):
        PinshCmd.PinshCmd.__init__(self, name)
        self.helpText = name+"\ta variable, to be used later with $<variableName>"
        self.level = 99
        self.cmdOwner = 0

    def match(self, tokens, index):
        if tokens[index] == '':
            return NO_MATCH, 1
        okCharacters = set("0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")
        if set(tokens[index]) - okCharacters:
            return NO_MATCH, 1
        return PARTIAL, 1

    def name(self, tokens, index):
        if self.match(tokens, index) == (PARTIAL, 1):
            return [tokens[index]]
        return []

if __name__ == "__main__":
    from libTest import startTest, runTest, endTest
    variable = Variable()
    status = OK
    startTest()
    runTest(variable.match, [[""], 0], (NO_MATCH, 1), status)
    runTest(variable.match, [["abc123"], 0], (PARTIAL, 1), status)
    runTest(variable.match, [["~!%^&*"], 0], (NO_MATCH, 1), status)
    runTest(variable.match, [["kjd;lkadsjf"], 0], (NO_MATCH, 1), status)
    endTest(status)
