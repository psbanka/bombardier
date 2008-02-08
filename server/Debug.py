#!/usr/bin/python

import sys

import PinshCmd, BomHostField, libCmd, pexpect
from commonUtil import *

class Debug(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "debug")
        self.helpText = "debug\tturn package debugging on or off"
        self.children = []
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        output = []
        if noFlag:
            if mode.debug == True:
                output =["Debugging turned off."] 
            else:
                output =["Debugging already off."] 
            mode.debug = False
            return OK, output
        if mode.debug == False:
            output =["Debugging turned on."] 
        else:
            output =["Debugging already on."] 
        mode.debug = True
        return OK, output
