#!/usr/bin/python

import PinshCmd
import BomHostField
import os
import yaml
from commonUtil import *
from Client import SERVER_PATH, Client
from bombardier.miniUtility import getInstalled, stripVersionFromKeys

def getProgressData(hostName):
    statusYml = "%s/status/%s.yml"%(SERVER_PATH, hostName)
    if not os.path.isfile(statusYml):
        print "NO FILE: %s" %statusYml
        return None
    yml = yaml.load( open(statusYml).read() ) 
    progressData = yml["install-progress"]
    return progressData

def getNamesFromProgress(hostName, stripped=False):
    progressData = getProgressData(hostName)
    if progressData == None:
         (installedPackageNames, brokenPackageNames) = ([],[])
    else:
        if stripped:
            progressData = stripVersionFromKeys(progressData)
        installedPackageNames, brokenPackageNames = getInstalled(progressData)
    return (set(installedPackageNames), set(brokenPackageNames))

def getInstalledPackageNames(hostName, stripped=False):
    return getNamesFromProgress(hostName, stripped)[0]  

def getBrokenPackageNames(hostName, stripped=False):
    return getNamesFromProgress(hostName, stripped)[1]  

def getPackageNamesFromBom(hostName):
    client = Client(hostName, mode.password)
    status = client.get()
    if status == FAIL:
        print "Bad config file for %s." % hostName
        return []
    return set(client.data.get("packages", []))

class PackageField(PinshCmd.PinshCmd):
    def __init__(self, name = "packageName"):
        PinshCmd.PinshCmd.__init__(self, name)
        self.helpText = "<packageName>\tthe name of a bombardier package"
        self.level = 99
        self.cmdOwner = 0

    def possiblePackageNames(self, hostName, packageName):
        print "VIRTUAL", hostName, packageName

    def name(self, tokens):
        hostName = tokens[1]
        possibleMatches = self.possiblePackageNames(hostName, tokens[2])
        if possibleMatches:
            return possibleMatches
        return ''

    def match(self, tokens, index):
        if tokens[index] == '':
            return NO_MATCH, 1
        hostName = tokens[index-1]
        possibleMatches = self.possiblePackageNames(hostName, tokens[index])
        if len(possibleMatches) == 0:
            return INCOMPLETE, 1
        elif len(possibleMatches) == 1:
            return COMPLETE, 1
        else:
            return PARTIAL, 1

class InstallablePackageField(PackageField):
    def __init__(self, name = "installablePackageField"):
        PackageField.__init__(self, name)

    def possiblePackageNames(self, hostName, packageName):
        client = Client(hostName, mode.password)
        status = client.get()
        if status == FAIL:
            print "Bad config file for %s." % hostName
            return []
        possibleMatches = set([])
        for i in getPackageNamesFromBom(hostName):
            if i.lower().startswith( packageName.lower() ):
                possibleMatches.add( i )
        installedPackageNames  =  getInstalledPackageNames(hostName, stripped=True)
        brokenPackageNames = getBrokenPackageNames(hostName, stripped=True)
        return list(possibleMatches - installedPackageNames - brokenPackageNames)

class InstalledPackageField(PackageField):
    def __init__(self, name = "installedPackageField"):
        PackageField.__init__(self, name)

    def possiblePackageNames(self, hostName, packageName):
        possibleMatches = []
        for i in getInstalledPackageNames(hostName):
            if i.lower().startswith( packageName.lower() ):
                possibleMatches.append( i )
        return possibleMatches

class PurgablePackageField(PackageField):
    def __init__(self, name = "purgablePackageField"):
        PackageField.__init__(self, name)

    def possiblePackageNames(self, hostName, packageName):
        possibleMatches = []
        packages = getInstalledPackageNames(hostName).union( getBrokenPackageNames(hostName) )
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
        for i in packages:
            if i.lower().startswith( packageName.lower() ):
                possibleMatches.append( i )
        return possibleMatches

if __name__ == "__main__":
    from libTest import *
    print os.getcwd()
    pField = PackageField()
    status = OK
    startTest()
    runTest(pField.match, [["bigdb"]], (PARTIAL, 2), status)
    runTest(pField.match, [["lilap", "foo"]], (NO_MATCH, 1), status)
    runTest(pField.match, [["lilap", "CgApache"]], (COMPLETE, 2), status)
    runTest(pField.match, [[""]], (NO_MATCH, 1), status)
    endTest(status)
