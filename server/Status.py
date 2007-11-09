#!/usr/bin/python

import sys
from bcs import BombardierRemoteClient, STATUS
import PinshCmd, BomHostField, libCmd
from commonUtil import *

DEBUG = 0

class Status(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "status")
        self.helpText = "status\tdetermine the status of a host"
        self.bomHostField = BomHostField.BomHostField() # FIXME: want a real machine
        self.children = [self.bomHostField]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        target = tokens[1]
        if self.bomHostField.match(tokens, 1) != (COMPLETE, 1):
            return FAIL, ["Invalid host: "+target]
        target = self.bomHostField.name(["status", target])[0]
        r = BombardierRemoteClient(target, STATUS, [], '', '')
        status = r.reconcile()
        if status == FAIL:
            return FAIL, ["Host is screwed up"]
        else:
            return OK, ['']

if __name__ == "__main__":
    pass
    #from libTest import *
    #status = startTest()
    #ping = Ping()
#
    #status = testMe(ping, "ping 4.2.2.2", OK, "4.2.2.2 is alive", status)
    #status = testMe(ping, "ping www.yahoo.com", OK, "www.yahoo.com is alive", status)
    #endTest(status)
