#!/usr/bin/python

import sys
from bcs import BombardierRemoteClient, UNINSTALL, INSTALL, VERIFY, EXECUTE, PURGE, FIX, CONFIGURE
import PinshCmd, libCmd
import BomHostField, PackageField, ScriptField
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
        self.scriptName = ''

    def processObject(self, hostName, packageName, tokens):
        r = BombardierRemoteClient(hostName, self.action, [packageName], '', mode.password)
        return r

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return FAIL, []
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        hostName = tokens[1]
        if self.bomHostField.match(tokens, 1) != (COMPLETE, 1):
            return FAIL, ["Invalid host: "+hostName]
        hostName = self.bomHostField.name(["command", hostName], 1)[0]
        packageName = tokens[2]
        if self.packageField.match(tokens, 2) != (COMPLETE, 1):
            return FAIL, ["Invalid package: "+packageName]
        packageName = self.packageField.name(["command", hostName, packageName], 2)[0]
        r = self.processObject(hostName, packageName, tokens)
        if r == None:
            return FAIL, ["Unable to run command."]
        status = r.reconcile()
        if status == FAIL:
            return FAIL, ["Host is screwed up"]
        else:
            return OK, ['']

class Install(PackageCommand):
    def __init__(self):
        PackageCommand.__init__(self, "install")
        self.helpText = "install\tinstall a package"
        self.packageField = PackageField.InstallablePackageField()
        self.bomHostField.children = [self.packageField]
        self.action = INSTALL
        self.logCommand = True

class Reconfigure(PackageCommand):
    def __init__(self):
        PackageCommand.__init__(self, "reconfigure")
        self.helpText = "reconfigure\treconfigure a package"
        self.packageField = PackageField.InstalledPackageField()
        self.bomHostField.children = [self.packageField]
        self.action = CONFIGURE
        self.logCommand = True

class Verify(PackageCommand):
    def __init__(self):
        PackageCommand.__init__(self, "verify")
        self.helpText = "verify\tverify a package"
        self.packageField = PackageField.InstalledPackageField()
        self.bomHostField.children = [self.packageField]
        self.action = VERIFY

class Uninstall(PackageCommand):
    def __init__(self):
        PackageCommand.__init__(self, "uninstall")
        self.helpText = "uninstall\tuninstall a package"
        self.packageField = PackageField.InstalledPackageField()
        self.bomHostField.children = [self.packageField]
        self.action = UNINSTALL
        self.logCommand = True

class Purge(PackageCommand):
    def __init__(self):
        PackageCommand.__init__(self, "purge")
        self.helpText = "purge\tremove a package from a client's list"
        self.packageField = PackageField.PurgablePackageField()
        self.bomHostField.children = [self.packageField]
        self.action = PURGE

class Fix(PackageCommand):
    def __init__(self):
        PackageCommand.__init__(self, "fix")
        self.helpText = "fix\tfix a broken package"
        self.packageField = PackageField.FixablePackageField()
        self.bomHostField.children = [self.packageField]
        self.action = FIX

class Execute(PackageCommand):
    def __init__(self):
        PackageCommand.__init__(self, "execute")
        self.helpText = "execute\texecute a script within a package"
        self.packageField = PackageField.InstalledPackageField()
        self.bomHostField.children = [self.packageField]
        self.scriptField = ScriptField.ScriptField()
        self.packageField.children = [self.scriptField]
        self.action = EXECUTE

    def processObject(self, hostName, packageName, tokens):
        scriptNames = self.scriptField.name(tokens, len(tokens)-1)
        if len(scriptNames) != 1:
            return None
        r = BombardierRemoteClient(hostName, self.action, [packageName], scriptNames[0], mode.password)
        return r

if __name__ == "__main__":
    pass

