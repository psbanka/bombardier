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
        self.bomHostField.allowAny = True
        self.children = [self.bomHostField]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        if noFlag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        hostName = tokens[1]
        ipAddress = self.bomHostField.ipAddress(hostName)
        s = pexpect.spawn(PING+" -c 1 "+ipAddress, timeout=3)
        i = s.expect([pexpect.TIMEOUT, "PING ", "ping: unknown host"], timeout=9)
        if i == 1:
            sys.stdout.write("pinging %s" % self.bomHostField.ipAddress(hostName))
            sys.stdout.flush()
        elif i == 2:
            return FAIL, ["Unknown host: %s" % ipAddress]
        else:
            return FAIL, ["Error running %s" % PING]
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
