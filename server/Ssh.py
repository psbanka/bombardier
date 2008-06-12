#!/usr/bin/python

import sys, os

import PinshCmd, BomHostField, Client, Expression
from commonUtil import *

SSH = "/usr/bin/ssh"

class Ssh(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "ssh")
        self.helpText = "ssh\tssh to another host"
        self.bomHostField = BomHostField.BomHostField()
        self.expression = Expression.Expression("ssh command")
        self.children = [self.bomHostField]
        self.bomHostField.children = [self.expression]
        self.cmdOwner = 1
        self.logCommand = True

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        if noFlag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        hostName = tokens[1] 
        client = Client.Client(hostName, '', mode.dataPath)
        client.get()
        username = client.data.get("defaultUser")
        address  = client.data.get("ipAddress")
        if len(tokens) > 2:
            command = '"%s"'%(' '.join(tokens[2:]))
        else:
            command = ''
        if username and address:
            os.system('%s %s@%s %s'%(SSH,username,address,command))
        return OK, []
