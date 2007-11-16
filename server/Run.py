#!/usr/bin/python

import sys

import PinshCmd, FileNameField
from commonUtil import *

class Run(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "run")
        self.helpText = "run\trun a bomsh script"
        self.fileNameField = FileNameField.FileNameField()
        self.children = [self.fileNameField]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag: pass
        fileNames = self.fileNameField.name(tokens, 1)
        if len(fileNames) == 0:
            return FAIL, ["invalid file: %s" % tokens[1]]
        if len(fileNames) > 1:
            if os.path.isfile(tokens[1]):
                fileName = tokens[1]
            return FAIL, ["Ambiguous file: %s" % tokens[1]]
        else:
            fileName = fileNames[0]
        lines = open(fileName).readlines()
        status = OK
        for line in lines:
            cmdStatus, cmdOutput = slash.processCommand(line.strip())
            if status == OK:
                status = cmdStatus
        return status, ['Script complete']

