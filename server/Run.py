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
        fileName = tokens[1]
        lines = open(fileName).readlines()
        status = OK
        mode.batch = True
        for line in lines:
            cmdStatus, cmdOutput = slash.processCommand(line.strip())
            if status == OK:
                status = cmdStatus
        mode.batch = False
        return status, ['Script complete']

