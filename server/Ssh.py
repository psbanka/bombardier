#!/usr/bin/python

import sys, os

import PinshCmd, BomHostField, Expression, Client
from commonUtil import *

SSH = "/usr/bin/ssh"

class Ssh(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "ssh")
        self.helpText = "ssh\tssh to another host"
        self.bomHostField = BomHostField.BomHostField()
        self.children = [self.bomHostField]
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        hostName = tokens[1] 
        if self.bomHostField.match(tokens, 1) != (COMPLETE, 1):
            return FAIL, ["Invalid destination: ", address]
        client = Client.Client(hostName, '')
        client.get()
        username = client.data.get("defaultUser")
        address  = client.data.get("ipAddress")
        if username and address:
            os.system(SSH+" "+username+"@"+address)
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
