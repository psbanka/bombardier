#!/usr/bin/python

import sys, os

import PinshCmd, FileNameField
from commonUtil import *

class Edit(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "edit")
        self.helpText = "edit\tedit a configuration file"
        self.fileNameField = FileNameField.FileNameField(mode.dataPath+"/deploy")
        self.children = [self.fileNameField]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        if noFlag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        fileName = mode.dataPath+"/deploy/%s" % tokens[1]
        editor = mode.editor
        os.system("%s %s" % (editor, fileName))
        return OK, []
