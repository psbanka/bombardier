#!/usr/bin/python

import sys
from bcs import BombardierRemoteClient, UNINSTALL, INSTALL, VERIFY, EXECUTE
import PinshCmd, libCmd
import BomHostField, PackageField
from commonUtil import *

DEBUG = 0

class PackageCommand(PinshCmd.PinshCmd):
    def __init__(self, name):
        PinshCmd.PinshCmd.__init__(self, name)
        self.helpText = "VIRTUAL\tTHIS AINT NO COMMAND"
        self.bomHostField = BomHostField.BomHostField()
        self.children = [self.bomHostField]
        self.action = None
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
        hostName = self.bomHostField.name(["command", hostName])[0]
        packageName = tokens[2]
        if self.packageField.match(tokens, 2) != (COMPLETE, 1):
            return FAIL, ["Invalid package: "+packageName]
        packageName = self.packageField.name(["command", hostName, packageName])[0]
        r = BombardierRemoteClient(hostName, self.action, [packageName], '', '')
        status = r.reconcile()
        if status == FAIL:
            return FAIL, ["Host is screwed up"]
        else:
            return OK, ['']


class Install(PackageCommand):
    def __init__(self):
        PackageCommand.__init__(self, "install")
        self.helpText = "install\tinstall a package"
        self.packageField = PackageField.InstallPackageField()
        self.bomHostField.children = [self.packageField]
        self.action = INSTALL

class Uninstall(PackageCommand):
    def __init__(self):
        PackageCommand.__init__(self, "uninstall")
        self.helpText = "uninstall\tuninstall a package"
        self.packageField = PackageField.UninstallPackageField()
        self.bomHostField.children = [self.packageField]
        self.action = UNINSTALL

if __name__ == "__main__":
    pass
    #from libTest import *
    #status = startTest()
    #ping = Ping()
#
    #status = testMe(ping, "ping 4.2.2.2", OK, "4.2.2.2 is alive", status)
    #status = testMe(ping, "ping www.yahoo.com", OK, "www.yahoo.com is alive", status)
    #endTest(status)

