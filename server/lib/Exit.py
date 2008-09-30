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
        pyChucker(tokens, noFlag)
        if len(mode.state) > 1:
            extraClasses = mode.newClasses[-1]
            while extraClasses > 0:
                slash.children.pop()
                extraClasses -= 1
        if mode.popPrompt() == FAIL:
            mode.clearBomConnections()
            sys.exit(0)
        else:
            return OK, []
