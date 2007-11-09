#!/usr/bin/python

import sys
from bcs import BombardierRemoteClient, INSTALL
import PinshCmd, libCmd
import BomHostField, PackageField
from commonUtil import *

DEBUG = 0

class Install(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "install")
        self.helpText = "install\tinstall a package"
        self.bomHostField = BomHostField.BomHostField()
        self.packageField = PackageField.InstallPackageField()
        self.children = [self.bomHostField, self.packageField]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return FAIL, []
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        hostName = tokens[1]
        if self.bomHostField.match(tokens, 1) != (COMPLETE, 1):
            return FAIL, ["Invalid host: "+hostName]
        packageName = tokens[2]
        if self.packageField.match(tokens, 2) != (COMPLETE, 1):
            return FAIL, ["Invalid package: "+packageName]
        r = BombardierRemoteClient(hostName, INSTALL, [packageName], '', '')
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

