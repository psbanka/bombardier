#!/usr/bin/python

import sys, signal

import PinshCmd
from libCmd import forkCmd
from commonUtil import *

class Debug(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "debug")
        self.helpText = "debug\tturn package debugging on or off"
        package = PinshCmd.PinshCmd("package", "package\tturn on detailed package debugging")
        log = PinshCmd.PinshCmd("log", "log\tturn on log debugging")
        self.children = [log, package]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash, tokens)
        output = []
        if len(tokens) == 1:
            return FAIL, ["Incomplete command"]
        if tokens[1].lower().startswith('p'):
            return self.packageDebug(noFlag)
        elif tokens[1].lower().startswith('l'):
            return self.logDebug(noFlag)
        
    def logDebug(self, noFlag):
        if noFlag:
            if mode.childProcesses:
                os.kill(mode.childProcesses.pop(), signal.SIGTERM)
                return OK, ["logging stopped"]
            else:
                return OK, []
        pid = forkCmd("tail", ["tail", "-f", "/var/log/messages"])
        if pid != 0:
            mode.childProcesses.append(pid)
            return OK, ["log debugging started..."]
        return FAIL, ["unable to start log debugging."]

    def packageDebug(self, noFlag):
        if noFlag:
            if mode.debug:
                output =["Debugging turned off."] 
            else:
                output =["Debugging already off."] 
            mode.debug = False
            return OK, output
        if not mode.debug:
            output =["Debugging turned on."] 
        else:
            output =["Debugging already on."] 
        mode.debug = True
        return OK, output
