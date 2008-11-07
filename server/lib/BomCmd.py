#!/usr/bin/python

import sys, os, time
from staticData import *
import Client
import PinshCmd
import BomHostField, PackageField, ScriptField, LongList
from RemoteClient import IncompleteConfigurationException, EnableRequiredException
from commonUtil import *
from Mode import HostNotEnabledException
from Client import ClientConfigurationException
import yaml
import syck

def ennumerate(configDictOrList, currentPath):
    configList = []
    if len(configDictOrList) == 0:
        return [currentPath]
    for thing in configDictOrList:
        if type(configDictOrList) == type({}):
            index = thing
        else:
            index = configDictOrList.index(thing)
        value = configDictOrList[index]
        vt = type(value)
        myCurrentPath = "%s/%s" % (currentPath, index)
        if vt in [ type('string'), type(1) ]:
            configList.append("%s/%s" % (myCurrentPath, value))
        else:
            configList += ennumerate(value, myCurrentPath)
    return configList

class BomCmd(PinshCmd.PinshCmd):
    def __init__(self, name):
        PinshCmd.PinshCmd.__init__(self, name)
        self.bomHostField = BomHostField.BomHostField()
        self.children = [self.bomHostField]
        self.action = None
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash, tokens)
        start = time.time()
        if noFlag:
            return FAIL, []
        if tokens[0].startswith("pac"):
            tokens = tokens[1:]
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        hostName = tokens[1]
        if self.action == RECONCILE and mode.password == '':
            return FAIL, ["Reconcile must be run in enable mode"]
        try:
            r = mode.getBomConnection(hostName, slash.fpOut)
        except HostNotEnabledException:
            return FAIL, ["Host not enabled for this user"]
        try:
            status, output = r.process(self.action, [], '', mode.debug)
        except ClientConfigurationException:
            return FAIL, []
        if status == FAIL:
            return FAIL, output
        else:
            return OK, ['', 'Command took %5.2f seconds' % (time.time() - start)]

class Status(BomCmd):
    def __init__(self):
        BomCmd.__init__(self, "status")
        self.helpText = "status\tdetermine the status of a host"
        self.action = STATUS

class Reconcile(BomCmd):
    def __init__(self):
        BomCmd.__init__(self, "reconcile")
        self.helpText = "reconcile\treconcile a host with its bill of materials"
        self.action = RECONCILE
        self.logCommand = True
        #self.auth = ADMIN

class DryRun(BomCmd):
    def __init__(self):
        BomCmd.__init__(self, "dry-run")
        self.helpText = "dry-run\ttest reconcile"
        self.action = DRY_RUN
        self.logCommand = True

class PackageCommand(PinshCmd.PinshCmd):
    def __init__(self, name):
        PinshCmd.PinshCmd.__init__(self, name)
        self.helpText = "VIRTUAL\tTHIS AINT NO COMMAND"
        self.bomHostField = BomHostField.BomHostField()
        self.children = [self.bomHostField]
        self.action = None
        self.packageField = None
        self.level = 0
        self.cmdOwner = 1
        self.scriptName = ''
        self.requireDecryption = False
        self.removeVersion = True

    def processObject(self, r, packageNames, tokens):
        pyChucker(tokens)
        try:
            status, output = r.process(self.action, packageNames.split(), '', mode.debug)
        except ClientConfigurationException:
            return FAIL, []
        return status, output

    def checkEncryption(self, serverName, packageNames):
        if mode.password != '':
            return OK
        client = Client.Client(serverName, '', mode.serverHome)
        client.get()
        encryptedDict = client.getEncryptedEntries()
        if encryptedDict:
            packageInfoPath = os.path.join(mode.serverHome, "packages", "packages.yml")
            packageData = syck.load(open(packageInfoPath).read())
            if self.removeVersion:
                packageNames = [ '-'.join(pkgName.split('-')[:-1]) for pkgName in packageNames.split() ]
            else:
                packageNames = packageNames.split()
            for packageName in packageNames:
                configDict = packageData[packageName]["configuration"]
                configList = ennumerate(configDict, '')
                for configItem in configList:
                    for encryptedItem in encryptedDict:
                        if encryptedItem.startswith(configItem):
                            return FAIL
        return OK

    def cmd(self, tokens, noFlag, slash):
        start = time.time()
        if slash: pass # pychecker
        if noFlag:
            return FAIL, []
        if tokens[0].startswith("pac"):
            tokens = tokens[1:]
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        hostName = tokens[1]
        if self.bomHostField.match(tokens, 1) != (COMPLETE, 1):
            preferredNames = self.bomHostField.preferredNames(tokens, 1)
            if hostName in preferredNames:
                self.hostName = hostName
            else:
                return FAIL, ["Invalid host: "+hostName]
        else:
            self.hostName = self.bomHostField.preferredNames(["command", hostName], 1)[0]
        if tokens[-1] == '':
            tokens = tokens[:-1]
        if self.action== EXECUTE:
            packageNames = self.packageField.possiblePackageNames(hostName, tokens[2])
            if len(packageNames) != 1:
                return FAIL, ["Invalid package: "+tokens[2]]
            packageName = packageNames[0]
            try:
                r = mode.getBomConnection(self.hostName, slash.fpOut)
            except EnableRequiredException:
                return FAIL, ["Must be in enable mode to contact this system."]
            except HostNotEnabledException:
                return FAIL, ["Host not enabled for this user."]
            except IncompleteConfigurationException, ice:
                msg =  ["Host configuration is not complete"]
                msg += ["%s" % ice]
                return FAIL, msg
            status, output = self.processObject(r, packageName, tokens)
        else:
            packageNames = self.packageList.workingNames(tokens, 2)
            if not packageNames:
                return FAIL, "No package names match %s. Check system status" % tokens[2]
            try:
                r = mode.getBomConnection(self.hostName, slash.fpOut)
            except EnableRequiredException:
                return FAIL, ["Must be in enable mode to contact this system."]
            except HostNotEnabledException:
                return FAIL, ["Host not enabled for this user."]
            except IncompleteConfigurationException, ice:
                msg =  ["Host configuration is not complete"]
                msg += ["%s" % ice]
                return FAIL, msg
            if self.requireDecryption and self.checkEncryption(self.hostName, packageNames) == FAIL:
                return FAIL, ["This package requires sensitive data."]
            status, output = self.processObject(r, packageNames, tokens)
        if status == FAIL:
            if not output:
                output = ["Command failed."]
            return FAIL, output
        else:
            if not output:
                output = ['', 'Command took %4.2f seconds' % (time.time() - start)]
            return OK, output

class Install(PackageCommand):
    def __init__(self):
        PackageCommand.__init__(self, "install")
        self.helpText = "install\tinstall a package"
        self.packageField = PackageField.InstallablePackageField()
        self.packageList = LongList.LongList(self.packageField, unique=True)
        self.bomHostField.children = [self.packageList]
        self.action = INSTALL
        self.logCommand = True
        self.requireDecryption = True
        self.removeVersion = False

class Configure(PackageCommand):
    def __init__(self):
        PackageCommand.__init__(self, "configure")
        self.helpText = "configure\tconfigure a package"
        self.packageField = PackageField.InstalledPackageField()
        self.packageList = LongList.LongList(self.packageField, unique=True)
        self.bomHostField.children = [self.packageList]
        self.action = CONFIGURE
        self.logCommand = True
        self.requireDecryption = True

class Verify(PackageCommand):
    def __init__(self):
        PackageCommand.__init__(self, "verify")
        self.helpText = "verify\tverify a package"
        self.packageField = PackageField.InstalledPackageField()
        self.packageList = LongList.LongList(self.packageField, unique=True)
        self.bomHostField.children = [self.packageList]
        self.action = VERIFY

class Uninstall(PackageCommand):
    def __init__(self):
        PackageCommand.__init__(self, "uninstall")
        self.helpText = "uninstall\tuninstall a package"
        self.packageField = PackageField.InstalledPackageField()
        self.packageList = LongList.LongList(self.packageField, unique=True)
        self.bomHostField.children = [self.packageList]
        self.action = UNINSTALL
        self.logCommand = True

class Purge(PackageCommand):
    def __init__(self):
        PackageCommand.__init__(self, "purge")
        self.helpText = "purge\tremove a package from a client's list"
        self.packageField = PackageField.PurgablePackageField()
        self.packageList = LongList.LongList(self.packageField, unique=True)
        self.bomHostField.children = [self.packageList]
        self.action = PURGE

class Fix(PackageCommand):
    def __init__(self):
        PackageCommand.__init__(self, "fix")
        self.helpText = "fix\tfix a broken package"
        self.packageField = PackageField.FixablePackageField()
        self.packageList = LongList.LongList(self.packageField, unique=True)
        self.bomHostField.children = [self.packageList]
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
        self.logCommand = True
        self.requireDecryption = True
        self.removeVersion = False

    def processObject(self, r, packageName, tokens):
        scriptNames = self.scriptField.possibleScriptNames(packageName, tokens[3])
        if len(scriptNames) == 0:
            return FAIL, ["Invalid scriptName"]
        elif len(scriptNames) > 1:
            if tokens[3] in scriptNames:
                scriptNames = [tokens[3]]
            else:
                return FAIL, ["Ambiguous scriptName"]
        if self.checkEncryption(self.hostName, packageName) == FAIL:
            return FAIL, ["This package requires sensitive data."]
        try:
            status, output = r.process(self.action, [packageName], scriptNames[0], mode.debug)
        except ClientConfigurationException:
            return FAIL, []
        return status, output

if __name__ == "__main__":
    pass

