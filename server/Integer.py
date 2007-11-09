#!/usr/bin/python

import sys,re

import PinshCmd
from commonUtil import *

class Integer(PinshCmd.PinshCmd):
    def __init__(self, min = 0, max = 100, name = "<integer>"):
        PinshCmd.PinshCmd.__init__(self, name)
        self.helpText = name+"\ta number between "+`min`+" and "+`max`
        self.level = 99
        self.min = min
        self.max = max
        self.cmdOwner = 0

    def match(self, tokens, index):
        if tokens[index] == '':
            return NO_MATCH, 1
        value = 0
        try:
            value = int(tokens[index])
            if value >= self.min and value <= self.max:
                return COMPLETE, 1
            else:
                return PARTIAL, 1
        except:
            pass
        return NO_MATCH, 1

    def name(self, tokens):
        return ""

if __name__ == "__main__":
    from libTest import *
    integer = Integer(10, 1000)
    status = OK
    startTest()
    runTest(integer.match, [[""]], (NO_MATCH, 1), status)
    runTest(integer.match, [["1"]], (PARTIAL, 1), status)
    runTest(integer.match, [["100"]], (COMPLETE, 1), status)
    runTest(integer.match, [["foofy"]], (NO_MATCH, 1), status)
    endTest(status)
