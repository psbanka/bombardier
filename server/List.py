#!/usr/bin/python

import sys,re

import PinshCmd
from commonUtil import *

class List(PinshCmd.PinshCmd):
    def __init__(self, name = "["):
        PinshCmd.PinshCmd.__init__(self, name)
        self.helpText = "[\tstart a list, end with a ']'"
        self.level = 99
        self.cmdOwner = 0

    def match(self, tokens, index):
        if tokens[index] != '[':
            return NO_MATCH, 1
        if tokens[-1] == ']':
            return COMPLETE, len(tokens) - index
        return PARTIAL, len(tokens) - index

    def name(self, tokens, index):
        return [tokens[-1], tokens[-1]]

if __name__ == "__main__":
    from libTest import *
    list = List()
    status = OK
    startTest()
    runTest(list.match, [['[', "foo"], 0], (PARTIAL, 2), status)
    runTest(list.match, [["cheeze"], 0], (NO_MATCH, 1), status)
    runTest(list.match, [["[", 'a', 'b', 'c'], 0], (PARTIAL, 4), status)
    runTest(list.match, [["[", 'a', 'b', 'c', ']'], 0], (COMPLETE, 5), status)
    runTest(list.match, [["[", ']'], 0], (COMPLETE, 2), status)
    endTest(status)
