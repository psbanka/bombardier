#!/usr/bin/python

import sys, os

import PinshCmd, FileNameField, pexpect
from commonUtil import *

class Edit(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "edit")
        self.helpText = "edit\tedit a configuration file"
        self.fileNameField = FileNameField.FileNameField("deploy")
        self.children = [self.fileNameField]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        names = self.fileNameField.name(tokens, 1)
        if len(names) < 1:
            return FAIL, ["Unknown filename: %s" % tokens[1]]
        if len(names) > 1:
            if not os.path.isfile("deploy/%s" % tokens[1]):
                return FAIL, ["Ambiguous filename: %s" % tokens[1]]
            else:
                names = [tokens[1]]
        fileName = "deploy/%s" % names[0]
        editor = mode.config.get("editor", "/usr/bin/vim")
        os.system("%s %s" % (editor, fileName))
        return OK, []
