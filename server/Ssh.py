#!/usr/bin/python

import sys, os

import PinshCmd, BomHostField, Client
from commonUtil import *

SSH = "/usr/bin/ssh"

class Ssh(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "ssh")
        self.helpText = "ssh\tssh to another host"
        self.bomHostField = BomHostField.BomHostField()
        self.children = [self.bomHostField]
        self.cmdOwner = 1
        self.logCommand = True

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        if noFlag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        hostName = tokens[1] 
        if self.bomHostField.match(tokens, 1) != (COMPLETE, 1):
            return FAIL, ["Invalid destination: ", hostName]
        client = Client.Client(hostName, '', mode.dataPath)
        client.get()
        username = client.data.get("defaultUser")
        address  = client.data.get("ipAddress")
        if username and address:
            os.system(SSH+" "+username+"@"+address)
        return OK, []
