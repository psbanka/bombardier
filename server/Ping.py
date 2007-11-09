#!/usr/bin/python

import sys

import PinshCmd, HostField, libCmd
from commonUtil import *

DEBUG = 0

PING = "/bin/ping"

class Ping(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "ping")
        self.helpText = "ping\tping a host to determine connectivity"
        self.hostField = HostField.HostField()
        self.children = [self.hostField]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        target = tokens[1]
        if self.hostField.match(tokens, 1) != (COMPLETE, 1):
            return FAIL, ["Invalid address: "+target]
        status, output = libCmd.runcmd(PING+" -c 1 "+target, 0)
        if status == FAIL:
            return FAIL, ["Host is not reachable"]
        else:
            return OK, [output]

if __name__ == "__main__":
    from libTest import *
    status = startTest()
    ping = Ping()

    status = testMe(ping, "ping 4.2.2.2", OK, "4.2.2.2 is alive", status)
    status = testMe(ping, "ping www.yahoo.com", OK, "www.yahoo.com is alive", status)
    endTest(status)
