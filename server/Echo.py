#!/usr/bin/python

import sys
import PinshCmd, Expression
import libUi
from commonUtil import *

class Echo(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "echo")
        self.helpText = "echo\tprints out a line"
        self.expression = Expression.Expression()
        self.children = [self.expression]
        self.logCommand = True
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        if noFlag:
            return FAIL, []
        return OK, [' '.join(tokens[1:]).strip()]

class Comment(Echo):
    def __init__(self):
        Echo.__init__(self)
        self.myName = "comment"
        self.helpText = "comment\tenter a comment into the bomsh log"

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash, noFlag)
        if len(tokens) < 2 or tokens[1] == '':
            if not mode.batch:
                print "\n\nCOMMANDS:"
                libUi.userOutput(mode.commentCommands, OK)
                comment = raw_input("Enter a comment for this change:\n> ")
            else:
                comment = ""
        else:
            comment = ' '.join(tokens[1:])
        logComment(comment)
        mode.commentCommands = []

class Pause(Echo):    
    def __init__(self):
        Echo.__init__(self)
        self.myName = "pause"
        self.helpText = "pause\twait for a <return>"

    def cmd(self, tokens, noFlag, slash):
        pyChucker(tokens, noFlag, slash)
        if not mode.batch:
            raw_input("Press Enter to continue\n")
        
