#!/usr/bin/python

import PinshCmd
import BomHostField
import os, re, glob
import syck
import yaml
from commonUtil import *
from Client import Client
from bombardier.miniUtility import getInstalled, stripVersion, getInstalledUninstalledTimes

def getProgressData(hostName):
    statusYml = mode.serverHome + "/status/%s.yml"%(hostName)
    if not os.path.isfile(statusYml):
        print "\n\n %% Cannot retrieve status (NO FILE: %s)" %statusYml
        return None
    yml = syck.load( open(statusYml).read() ) 
    if yml == None:
        return {}
    progressData = yml.get("install-progress")
    return progressData

def getNamesFromProgress(hostName, stripped=False):
    progressData = getProgressData(hostName)
    if progressData == None:
         (installedPackageNames, brokenPackageNames) = ([],[])
    else:

        pkgInfo = getInstalledUninstalledTimes(progressData)
        unstrippedInstPkgs    = [packageName[0] for packageName in pkgInfo["installed"]]
        unstrippedBrokenPkgs  = [packageName[0] for packageName in pkgInfo["brokenInstalled"]]
        unstrippedBrokenPkgs += [packageName[0] for packageName in pkgInfo["brokenUninstalled"]]

        if stripped:
            installedPackageNames = [ stripVersion(x) for x in unstrippedInstPkgs]
            brokenPackageNames    = [ stripVersion(x) for x in unstrippedBrokenPkgs]
        else:
            installedPackageNames = unstrippedInstPkgs
            brokenPackageNames    = unstrippedBrokenPkgs
    return (set(installedPackageNames), set(brokenPackageNames))

def getInstallablePackageNames(hostName, packageName, stripped=True):
    possibleMatches = set([])
    for i in getPackageNamesFromBom(hostName):
        if i.lower().startswith( packageName.lower() ):
            possibleMatches.add( i )
    installedPackageNames  =  getInstalledPackageNames(hostName, stripped)
    brokenPackageNames = getBrokenPackageNames(hostName, stripped)
    return list(possibleMatches - installedPackageNames - brokenPackageNames)

def getInstalledPackageNames(hostName, stripped=False):
    return getNamesFromProgress(hostName, stripped)[0]

def getBrokenPackageNames(hostName, stripped=False):
    return getNamesFromProgress(hostName, stripped)[1]

def getPackageNamesFromBom(hostName):
    client = Client(hostName, mode.password, mode.serverHome)
    status = client.get()
    if status == FAIL:
        print " %% Bad config file for %s." % hostName
        return []
    return set(client.data.get("packages", []))

class PackageField(PinshCmd.PinshCmd):
    def __init__(self, name = "packageName"):
        PinshCmd.PinshCmd.__init__(self, name)
        self.helpText = "<packageName>\tthe name of a bombardier package"
        self.level = 99
        self.bomHostField = BomHostField.BomHostField()
        self.cmdOwner = 0

    def possiblePackageNames(self, hostName, packageName):
        pyChucker(hostName, packageName)
        print "VIRTUAL"
        return []

    def preferredNames(self, tokens, index):
        hostNames = self.bomHostField.preferredNames(tokens, index-1)
        if tokens[index-1] in hostNames:
            hostNames = [tokens[index-1]]
        if len(hostNames) != 1:
            return []
        hostName = hostNames[0]
        try:
            possibleMatches = self.possiblePackageNames(hostName, tokens[index])
        except AttributeError:
            print "%% Invalid status data. Perform a package status command."
            return []
        if possibleMatches:
            return possibleMatches
        return []

    def match(self, tokens, index):
        possibleMatches = self.preferredNames(tokens, index)
        if len(possibleMatches) == 0:
            return INCOMPLETE, 1
        elif len(possibleMatches) == 1:
            return COMPLETE, 1
        else:
            return PARTIAL, 1

class BasicPackageField(PackageField):
    def __init__(self, name = "BasicPackageField"):
        PackageField.__init__(self, name)

    def possiblePackageNames(self, hostName, packageName):
        pyChucker(hostName)
        r = re.compile('([\w\-]+)\-\d+')
        files = glob.glob(mode.serverHome+"/deploy/packages/*.spkg")
        clippedFiles = [ f.split('.spkg')[0].split('/')[-1] for f in files ]
        pkgNames = [ r.findall(c)[0] for c in clippedFiles ]
        filteredNames = [ pkgName for pkgName in pkgNames if pkgName.lower().startswith(packageName.lower()) ]
        uniquePkgNames = list(set(filteredNames))
        return uniquePkgNames

    def preferredNames(self, tokens, index):
        possibleMatches = self.possiblePackageNames('', tokens[index])
        if possibleMatches:
            return possibleMatches
        return [] 

class InstallablePackageField(PackageField):
    def __init__(self, name = "installablePackageField"):
        PackageField.__init__(self, name)

    def possiblePackageNames(self, hostName, packageName):
        client = Client(hostName, mode.password, mode.serverHome)
        status = client.get()
        if status == FAIL:
            print "Bad config file for %s." % hostName
            return []
        return getInstallablePackageNames(hostName, packageName, stripped=True)

class InstalledPackageField(PackageField):
    def __init__(self, name = "installedPackageField"):
        PackageField.__init__(self, name)

    def possiblePackageNames(self, hostName, packageName):
        possibleMatches = []
        for i in getInstalledPackageNames(hostName, stripped=True):
            if i.lower().startswith( packageName.lower() ):
                possibleMatches.append( i )
        return possibleMatches

class PurgablePackageField(PackageField):
    def __init__(self, name = "purgablePackageField"):
        PackageField.__init__(self, name)

    def possiblePackageNames(self, hostName, packageName):
        possibleMatches = []
        packages = getInstalledPackageNames(hostName, False).union( getBrokenPackageNames(hostName))
        for i in packages:
            if i.lower().startswith( packageName.lower() ):
                possibleMatches.append( i )
        return possibleMatches

class FixablePackageField(PackageField):
    def __init__(self, name = "fixablePackageField"):
        PackageField.__init__(self, name)

    def possiblePackageNames(self, hostName, packageName):
        possibleMatches = []
        packages = getBrokenPackageNames(hostName)
        packages = packages.union(set(getInstallablePackageNames(hostName, packageName, stripped=True)))
        for i in packages:
            if i.lower().startswith( packageName.lower() ):
                possibleMatches.append( i )
        return possibleMatches

if __name__ == "__main__":
    from libTest import startTest, runTest, endTest
    print os.getcwd()
    pField = InstallablePackageField()
    puField = PurgablePackageField()
    status = OK
    startTest()
    status = runTest(pField.match, [["bigdb", ""], 0], (INCOMPLETE, 1), status)
    status = runTest(pField.match, [["lilap", "foo"], 1], (INCOMPLETE, 1), status)
    status = runTest(pField.match, [["lilap", "CgApache"], 1], (INCOMPLETE, 1), status)
    status = runTest(pField.match, [["foo"], 0], (INCOMPLETE, 1), status)
    status = runTest(puField.preferredNames,  [["testdb", "Sql"], 1], ["SqlBackup-8"], status)
    endTest(status)

