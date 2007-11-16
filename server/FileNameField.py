#!/usr/bin/python

import sys,glob

import PinshCmd
from commonUtil import *

class FileNameField(PinshCmd.PinshCmd):
    def __init__(self, name = "fileNameField"):
        PinshCmd.PinshCmd.__init__(self, name, tokenDelimiter = '')
        self.helpText = "<file>\tthe name of a file"
        self.level = 99
        self.cmdOwner = 0

    def match(self, tokens, index):
        names = self.name(tokens, index) 
        if len(names) == 0:
            return NO_MATCH, 1
        if len(names) > 1:
            return PARTIAL, 1
        else:
            return COMPLETE, 1

    def name(self, tokens, index):
        names = glob.glob("%s*" % tokens[index])
        if len(names) == 0:
            return ''
        return names

if __name__ == "__main__":
    from libTest import *
    fileNameField = FileNameField()
    status = OK
    startTest()
    status = runTest(fileNameField.match, [["PinshCmd.py"], 0], (PARTIAL, 1), status)
    status = runTest(fileNameField.match, [["PinshCmd.pyc"], 0], (COMPLETE, 1), status)
    status = runTest(fileNameField.match, [["bom"], 0], (PARTIAL, 1), status)
    status = runTest(fileNameField.match, [["a;sdjf#!"], 0], (NO_MATCH, 1), status)
    status = runTest(fileNameField.match, [[""], 0], (PARTIAL, 1), status)
    status = runTest(fileNameField.match, [["/bin/bas"], 0], (COMPLETE, 1), status)
    status = runTest(fileNameField.match, [["/bin/bash"], 0], (COMPLETE, 1), status)
    endTest(status)
