#!/usr/bin/python

import sys,re

import PinshCmd
from commonUtil import *

class HostField(PinshCmd.PinshCmd):
    def __init__(self, name = "hostname"):
        PinshCmd.PinshCmd.__init__(self, name)
        self.helpText = "<hostname>\tthe name of a host"
        self.min = min
        self.max = max
        self.level = 99
        self.cmdOwner = 0

    def match(self, tokens):
        if tokens[0] == '':
            return NO_MATCH, 1
        tokens = tokens[0].split('.')
        for token in tokens:
            cleanedUp = token.replace('*','').replace('&','').replace(' ','').replace('(','').replace('/','')
            cleanedUp = cleanedUp.replace('%','').replace('^','').replace(')','').replace('$','').replace('#','')
            if cleanedUp != token:
                return NO_MATCH, 1
        return COMPLETE, 1

    def name(self, tokens):
        return ""

if __name__ == "__main__":
    from libTest import *
    hostField = HostField()
    status = OK
    startTest()
    runTest(hostField.match, [["foo"]], (COMPLETE, 1), status)
    runTest(hostField.match, [["www.foo.com"]], (COMPLETE, 1), status)
    runTest(hostField.match, [["a;sdjf#!"]], (NO_MATCH, 1), status)
    runTest(hostField.match, [[""]], (NO_MATCH, 1), status)
    endTest(status)
