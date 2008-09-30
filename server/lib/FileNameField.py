#!/usr/bin/python

import sys, glob

import PinshCmd
from commonUtil import *

class FileNameField(PinshCmd.PinshCmd):
    def __init__(self, startDir='', allowAny = False):
        PinshCmd.PinshCmd.__init__(self, "fileNameField", tokenDelimiter = '')
        self.helpText = "<file>\tthe name of a file"
        self.startDir = startDir
        self.cmdOwner = 0
        self.allowAny = allowAny

    def match(self, tokens, index):
        names = self.acceptableNames(tokens, index)
        if len(names) == 0:
            return NO_MATCH, 1
        if len(names) > 1:
            return PARTIAL, 1
        else:
            return COMPLETE, 1

    def preferredNames(self, tokens, index):
        if self.startDir:
            names = [name.split(self.startDir+'/')[-1] for name in glob.glob("%s/%s*" % (self.startDir, tokens[index]))]
        else:
            names = glob.glob("%s*" % tokens[index])
        return names

    def acceptableNames(self, tokens, index):
        names = self.preferredNames(tokens, index)
        if self.allowAny:
            if tokens[index] not in names:
                names.insert(0, tokens[index])
        return names

if __name__ == "__main__":
    from libTest import startTest, runTest, endTest
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
    fnf2 = FileNameField(startDir=mode.serverHome)
    status = runTest(fnf2.match, [["include"], 0], (COMPLETE, 1), status)
    status = runTest(fnf2.match, [["include/test"], 0], (PARTIAL, 1), status)
    status = runTest(fnf2.preferredNames, [["include/testI"], 0], ["include/testInclude.yml"], status)
    endTest(status)
