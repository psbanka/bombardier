#!/usr/bin/python

import sys
import yaml
import time
import PinshCmd, Expression
from getpass import getpass
from commonUtil import *

class Echo(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "echo")
        self.helpText = "echo\tprints out a line"
        self.expression = Expression.Expression()
        self.children = [self.expression]
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return FAIL, []
        return OK, [' '.join(tokens[1:]).strip()]


class Comment(Echo):
    def __init__(self):
        Echo.__init__(self)
        self.myName = "comment"
        self.helpText = "comment\tenter a comment into the bomsh log"

    def cmd(self, tokens, noFlag, slash):
        comment = ' '.join(tokens[1:])
        logComment(comment)
        mode.commentRequired = False

class Pause(Echo):    
    def __init__(self):
        Echo.__init__(self)
        self.myName = "pause"
        self.helpText = "pause\twait for a <return>"

    def cmd(self, tokens, noFlag, slash):
        raw_input("Press Enter to continue\n")
        
