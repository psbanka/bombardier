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
        #print "exp-match %s\n" % ( len(tokens) - index )
        return PARTIAL, len(tokens) - index

    def name(self, tokens, index):
        #print tokens[-1],'\n'
        return [tokens[-1], tokens[-1]]

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
