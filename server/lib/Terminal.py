#!/usr/bin/python

import sys

import PinshCmd, Integer
from commonUtil import *

class Terminal(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "terminal")
        self.helpText = "terminal\tchange settings for the current terminal"
        self.length = PinshCmd.PinshCmd("length", "length\tnumber of lines the terminal can display")
        self.width  = PinshCmd.PinshCmd("width", "width\tnumber of columns the terminal can disply")
        self.children = [self.length, self.width]
        self.num = Integer.Integer(min = 0, max = 1000, name = "<int>")
        self.length.children = [self.num]
        self.width.children = [self.num]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        pyChucker(noFlag, slash)
        if len(tokens) < 3:
            msg  = ["Current terminal length: %d" % mode.termlen]
            msg += ["Current terminal width: %d" % mode.termwidth]
            return OK, msg
        value = tokens[2]
        if self.num.match([value], 0) != (COMPLETE, 1):
            return FAIL, ["Please choose a value between 0 and 1000"]

        if tokens[1].lower().startswith('l'):
            mode.termlen = int(value)
        elif tokens[1].lower().startswith('w'):
            mode.termwidth = int(value)
        return OK, []
