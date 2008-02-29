#!/usr/bin/python

import sys

import PinshCmd, BomHostField, pexpect
from commonUtil import *

PING = "/bin/ping"

class Ping(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "ping")
        self.helpText = "ping\tping a host to determine connectivity"
        self.bomHostField = BomHostField.BomHostField()
        self.children = [self.bomHostField]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        if noFlag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        hostNames = self.bomHostField.name(tokens, 1)
        if len(hostNames) == 0:
            return FAIL, ["Unknown host %s" % tokens[1]]
        if len(hostNames) > 1:
            return FAIL, ["Ambiguous host %s" % tokens[1]]
        hostName = hostNames[0]
        ipAddress = self.bomHostField.ipAddress(hostName)
        s = pexpect.spawn(PING+" -c 1 "+ipAddress, timeout=3)
        i = s.expect([pexpect.TIMEOUT, "PING "], timeout=3)
        if i == 1:
            sys.stdout.write("pinging %s" % self.bomHostField.ipAddress(hostName))
            sys.stdout.flush()
        else:
            return FAIL, ["Error in ping command (%s)" % PING]
        for j in range(3):
            pyChucker(j)
            i = s.expect([pexpect.EOF, pexpect.TIMEOUT], timeout=1)
            if i == 1:
                sys.stdout.write('.')
                sys.stdout.flush()
        s.close()
        print
        if s.exitstatus != None and s.exitstatus != 0:
            return FAIL, ["Host is not reachable."]
        else:
            return OK, ["Host is alive."]
