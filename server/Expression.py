#!/usr/bin/python

import sys

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

    def preferredNames(self, tokens, index):
        pyChucker(index)
        #print tokens[-1],'\n'
        return [tokens[-1], tokens[-1]]

if __name__ == "__main__":
    from libTest import startTest, runTest, endTest
    exp = Expression()
    status = OK
    startTest()
    status = runTest(exp.match, [[""], 0], (PARTIAL, 1), status)
    status = runTest(exp.match, [["abc"], 0], (PARTIAL, 1), status)
    status = runTest(exp.match, [["~!%^&*"], 0], (PARTIAL, 1), status)
    status = runTest(exp.match, [["kjd ;lkads jf"], 0], (PARTIAL, 1), status)
    endTest(status)
