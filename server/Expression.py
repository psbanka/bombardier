#!/usr/bin/python

import sys,re

import PinshCmd
from commonUtil import *

class Expression(PinshCmd.PinshCmd):
    def __init__(self, name = "expression"):
        PinshCmd.PinshCmd.__init__(self, name)
        self.helpText = name+"\tan arbitrary expression"
        self.level = 99
        self.cmdOwner = 0

    def match(self, tokens, index):
        if tokens[index] == '':
            return NO_MATCH, 1
        return PARTIAL, 1

    def name(self, tokens):
        return ""

if __name__ == "__main__":
    from libTest import *
    exp = Expression()
    status = OK
    startTest()
    runTest(exp.match, [[""]], (NO_MATCH, 1), status)
    runTest(exp.match, [["abc"]], (PARTIAL, 1), status)
    runTest(exp.match, [["~!%^&*"]], (PARTIAL, 1), status)
    runTest(exp.match, [["kjd ;lkads jf"]], (PARTIAL, 1), status)
    endTest(status)
