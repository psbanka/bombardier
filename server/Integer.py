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

    def name(self, tokens, index):
        try:
            value = int(tokens[index])
            if value >= self.min and value <= self.max:
                return [tokens[index]]
        except:
            pass
        return ''

if __name__ == "__main__":
    from libTest import *
    integer = Integer(10, 1000)
    status = OK
    startTest()
    #status = runTest(integer.name, [["a"], 0], '', status)
    status = runTest(integer.name, [["11"], 0], ['11'], status)
    status = runTest(integer.name, [["10"], 0], ['10'], status)
    status = runTest(integer.match, [[""], 0], (NO_MATCH, 1), status)
    status = runTest(integer.match, [["1"], 0], (PARTIAL, 1), status)
    status = runTest(integer.match, [["100"], 0], (COMPLETE, 1), status)
    status = runTest(integer.match, [["foofy"], 0], (NO_MATCH, 1), status)
    endTest(status)
