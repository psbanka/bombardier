#!/usr/bin/python

import sys, os

import PinshCmd, HostField, Integer
from commonUtil import *

TELNET = "/usr/bin/telnet"

class Telnet(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "telnet")
        self.helpText = "telnet\ttelnet to another host"
        self.hostField = HostField.HostField()
        self.children = [self.hostField]
        self.port = Integer.Integer(min = 1, max = 65535, name = "port")
        self.hostField.children = [self.port]
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        if noFlag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command"]
        address = tokens[1]
        if self.hostField.match([address]) != (COMPLETE, 1):
            return FAIL, ["Invalid address: "+address]
        if len(tokens) == 3:
            port = tokens[2]
            if self.port.match([port]) != (COMPLETE, 1):
                return FAIL, ["Inalid port: "+port]
            os.system(TELNET+" "+address+" "+port)
        else:
            os.system(TELNET+" "+address)
        return OK, []
