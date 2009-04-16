#!/usr/bin/python

import sys

import PinshCmd, Integer
from commonUtil import *
import MultipleChoice

class Terminal(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "terminal")
        self.helpText = "terminal\tchange settings for the current terminal"
        self.length = PinshCmd.PinshCmd("length", "length\tnumber of lines the terminal can display")
        self.width  = PinshCmd.PinshCmd("width", "width\tnumber of columns the terminal can disply")
        self.color  = PinshCmd.PinshCmd("color", "color\tset terminal color")
        helpText = ["black and white", "colors for light background", "colors for dark background"]
        colorOptions = MultipleChoice.MultipleChoice(choices = ["none", "light", "dark"], 
                                                     helpText = helpText)
        self.children = [self.length, self.width, self.color]
        self.num = Integer.Integer(min = 0, max = 1000, name = "<int>")
        self.length.children = [self.num]
        self.width.children = [self.num]
        self.color.children = [colorOptions]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            if tokens[1].lower().startswith('c'):
                mode.termcolor = NO_COLOR
                return OK, ["Terminal set to black and white."]
            else:
                return FAIL, ["'No' not applicable in this context"]
        if len(tokens) < 3:
            msg  = ["terminal length: %d" % mode.termlen]
            msg += ["terminal width: %d" % mode.termwidth]
            if mode.termcolor != NO_COLOR:
                if mode.termcolor == LIGHT:
                    msg += ["terminal color-scheme: light"]
                if mode.termcolor == DARK:
                    msg += ["terminal color-scheme: dark"]
            return OK, msg
        value = tokens[2]
        if tokens[1].lower().startswith('c'):
            if value.lower().startswith('n'):
                mode.termcolor = NO_COLOR
                return OK, ["Terminal set to black and white."]
            elif value.lower().startswith('l'):
                mode.termcolor = LIGHT
                return OK, ["Terminal set to light background."]
            elif value.lower().startswith('d'):
                mode.termcolor = DARK
                return OK, ["Terminal set to dark background."]
            else:
                return FAIL, ["Unknown terminal color."]

        if self.num.match([value], 0) != (COMPLETE, 1):
            return FAIL, ["Please choose a value between 0 and 1000"]

        if tokens[1].lower().startswith('l'):
            mode.termlen = int(value)
        elif tokens[1].lower().startswith('w'):
            mode.termwidth = int(value)
        return OK, []
