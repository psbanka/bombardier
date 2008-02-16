#!/usr/bin/python

import PinshCmd
import BomHostField
import os, re, glob
import yaml
from commonUtil import *
from Client import Client
from bombardier.miniUtility import getInstalled, stripVersionFromKeys

def getProgressData(hostName):
    statusYml = "status/%s.yml"%(hostName)
    if not os.path.isfile(statusYml):
        print "\n\n %% Cannot retrieve status (NO FILE: %s)" %statusYml
        return None
    yml = yaml.load( open(statusYml).read() ) 
    progressData = yml.get("install-progress")
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
        print "VIRTUAL"
        return []

    def name(self, tokens, index):
        hostNames = self.bomHostField.name(tokens, index-1)
        if len(hostNames) != 1:
            return ''
        hostName = hostNames[0]
        try:
            possibleMatches = self.possiblePackageNames(hostName, tokens[index])
        except AttributeError:
            print "%% Invalid status data. Perform a package status command."
            return ''
        if possibleMatches:
            return possibleMatches
        return ''

    def match(self, tokens, index):
        possibleMatches = self.name(tokens, index)
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
        r = re.compile('([\w\-]+)\-\d+')
        files = glob.glob("deploy/packages/*.spkg")
        clippedFiles = [ x.split('.spkg')[0].split('/')[-1] for x in files ]
        pkgNames = [ r.findall(x)[0] for x in clippedFiles ]
        filteredNames = [ pkgName for pkgName in pkgNames if pkgName.lower().startswith(packageName.lower()) ]
        uniquePkgNames = list(set(filteredNames))
        return uniquePkgNames

    def name(self, tokens, index):
        possibleMatches = self.possiblePackageNames('', tokens[index])
        if possibleMatches:
            return possibleMatches
        return ''

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
    pField = InstallablePackageField()
    puField = PurgablePackageField()
    status = OK
    startTest()
    status = runTest(pField.match, [["bigdb", ""], 0], (INCOMPLETE, 1), status)
    status = runTest(pField.match, [["lilap", "foo"], 1], (INCOMPLETE, 1), status)
    status = runTest(pField.match, [["lilap", "CgApache"], 1], (INCOMPLETE, 1), status)
    status = runTest(pField.match, [["foo"], 0], (INCOMPLETE, 1), status)
    status = runTest(puField.name,  [["testdb", "Sql"], 1], ["SqlBackup-8"], status)
    endTest(status)
