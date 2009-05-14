#!/usr/bin/python

import sys, time

import PinshCmd, Integer
from commonUtil import *

class Sleep(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "sleep")
        self.helpText = "sleep\twait a specific number of seconds"
        self.children = [Integer.Integer(min=1, max=1000)]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        if noFlag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        sleepTime = int(tokens[1])
        time.sleep(sleepTime)
        return OK, []
