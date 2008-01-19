#!/usr/bin/python

import sys,glob

import PinshCmd
from commonUtil import *

class FileNameField(PinshCmd.PinshCmd):
    def __init__(self, startDir=''):
        PinshCmd.PinshCmd.__init__(self, "fileNameField", tokenDelimiter = '')
        self.helpText = "<file>\tthe name of a file"
        self.startDir = startDir
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
        if self.startDir:
            names = glob.glob("%s/%s*" % (self.startDir, tokens[index]))
            return [name.split(self.startDir+'/')[-1] for name in names]
            #return [name.partition(self.startDir)[2][1:] for name in names]
        else:
            names = glob.glob("%s*" % tokens[index])
        if len(names) == 0:
            return ''
        return names

if __name__ == "__main__":
    from libTest import *
    fileNameField = FileNameField()
    status = OK
    startTest()
    #status = runTest(fileNameField.match, [["PinshCmd.py"], 0], (PARTIAL, 1), status)
    #status = runTest(fileNameField.match, [["PinshCmd.pyc"], 0], (COMPLETE, 1), status)
    #status = runTest(fileNameField.match, [["bom"], 0], (PARTIAL, 1), status)
    #status = runTest(fileNameField.match, [["a;sdjf#!"], 0], (NO_MATCH, 1), status)
    #status = runTest(fileNameField.match, [[""], 0], (PARTIAL, 1), status)
    #status = runTest(fileNameField.match, [["/bin/bas"], 0], (COMPLETE, 1), status)
    #status = runTest(fileNameField.match, [["/bin/bash"], 0], (COMPLETE, 1), status)
    fnf2 = FileNameField(startDir="deploy")
    #status = runTest(fnf2.match, [["include"], 0], (COMPLETE, 1), status)
    #status = runTest(fnf2.match, [["include/test"], 0], (PARTIAL, 1), status)
    status = runTest(fnf2.name, [["include/testI"], 0], ["include/testInclude.yml"], status)
    endTest(status)
