#!/usr/bin/python

import sys, os

import PinshCmd, HostField, Expression
from commonUtil import *

SSH = "/usr/bin/ssh"

class Ssh(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "ssh")
        self.helpText = "ssh\tssh to another host"
        self.hostField = HostField.HostField()
        self.children = [self.hostField]
        self.expression = Expression.Expression()
        self.expression.helpText = "username\tname of user to connect as"
        self.hostField.children = [self.expression]
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        address = tokens[1] 
        if self.hostField.match([address]) != (COMPLETE, 1):
            return FAIL, ["Invalid destination: ", address]
        if len(tokens) == 3:
            name = tokens[2]
            os.system(SSH+" "+name+"@"+address)
        else:
            os.system(SSH+" "+address)
        return OK, []

if __name__ == "__main__":
    from libTest import *
    import Mode
    mode = Mode.Mode(1, "#")
    status = OK
    ssh = Ssh()
    startTest()

    status = testMe(ssh, "ssh mail.thebankas.com peter", OK, "", status)
    #status = testMe(ssh, "ssh 192.168.0.11 peter", OK, "", status)
    endTest(status)
