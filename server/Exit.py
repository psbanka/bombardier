#!/usr/bin/python

import sys

import PinshCmd, Mode
from commonUtil import *

class Exit(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "exit")
        self.helpText = "exit\texit current mode"
        self.level = Mode.ALL
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if len(mode.state) > 1:
            extraClasses = mode.newClasses[-1]
            for i in range(0,extraClasses):
                slash.children.pop()
        if mode.popPrompt() == FAIL:
            sys.exit(0)
        else:
            return OK, []
