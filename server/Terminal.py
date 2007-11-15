#!/usr/bin/python

import sys

import PinshCmd, Integer
from commonUtil import *

class Terminal(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "terminal")
        self.helpText = "terminal\tchange settings for the current terminal"
        self.length = PinshCmd.PinshCmd("length", "length\tnumber of lines the terminal can display")
        self.children = [self.length]
        self.num = Integer.Integer(min = 0, max = 1000, name = "<int>")
        self.length.children = [self.num]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if len(tokens) < 3:
            return OK, ["Current terminal length: "+`mode.termlen`]
        termlen = tokens[2]
        if self.num.match([termlen]) != (COMPLETE, 1):
            return FAIL, ["Please choose a terminal length between 0 and 1000"]
        try:
            mode.termlen = int(termlen)
            return OK, []
        except:
            return FAIL, ["Please choose a terminal length between 0 and 1000"]
