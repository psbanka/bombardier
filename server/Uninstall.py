#!/usr/bin/python

import sys
from bcs import BombardierRemoteClient, UNINSTALL
import PinshCmd, libCmd
import BomHostField, PackageField
from commonUtil import *

DEBUG = 0

class Uninstall(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "uninstall")
        self.helpText = "uninstall\tuninstall a package"
        self.bomHostField = BomHostField.BomHostField()
        self.packageField = PackageField.UninstallPackageField()
        self.children = [self.packageField]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return FAIL, []
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        hostName = tokens[1]
        if self.bomHostField.match([hostName]) != (COMPLETE, 1):
            return FAIL, ["Invalid host: "+hostName]
        hostName = self.bomHostField.name(["uninstall", hostName])[0]
        packageName = tokens[2]
        if self.packageField.match([hostName, packageName]) != (COMPLETE, 2):
            return FAIL, ["Invalid package: "+packageName]
        hostName = self.packageField.name(["uninstall", hostName, packageName])[0]
        r = BombardierRemoteClient(hostName, UNINSTALL, [packageName], '', '')
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

