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

class PackageField(PinshCmd.PinshCmd):
    def __init__(self, name = "packageName"):
        PinshCmd.PinshCmd.__init__(self, name)
        self.bomHostField = BomHostField.BomHostField()
        self.helpText = "<packageName>\tthe name of a bombardier package"
        self.min = min
        self.max = max
        self.level = 99
        self.cmdOwner = 0

    def possiblePackageNames(self, hostName, packageName):
        print "VIRTUAL", hostName, packageName

    def name(self, tokens):
        if len(tokens) == 2:
            return self.bomHostField.name( tokens )
        hostName = tokens[1]
        possibleMatches = self.possiblePackageNames(hostName, tokens[2])
        if possibleMatches:
            return possibleMatches
        return ''

    def match(self, tokens):
        if tokens[0] == '':
            return NO_MATCH, 1
        hostName = tokens[0]
        if len(tokens) == 1:
            if self.bomHostField.match([hostName])[0] == NO_MATCH:
                return NO_MATCH, 1
            return PARTIAL, 2
        if self.bomHostField.match([hostName])[0] != COMPLETE:
            return NO_MATCH, 1
        possibleMatches = self.possiblePackageNames(hostName, tokens[1])
        if len(possibleMatches) == 0:
            return INCOMPLETE, 2
        elif len(possibleMatches) == 1:
            return COMPLETE, 2
        else:
            return PARTIAL, 2

class InstallPackageField(PackageField):
    def __init__(self, name = "installPackageName"):
        PackageField.__init__(self, name)

    def possiblePackageNames(self, hostName, packageName):
        client = Client(hostName, '')
        status = client.get()
        if status == FAIL:
            print "Bad config file for %s." % hostName
            return []
        possibleMatches = []
        for i in client.data.get("packages", []):
            if i.lower().startswith( packageName.lower() ):
                possibleMatches.append( i )
        progressData = getProgressData(hostName)
        if progressData == None:
            return possibleMatches
        progressData = stripVersionFromKeys(progressData)
        installedPackageNames =  getInstalled(progressData)[0]
        return list(set(possibleMatches) - set(installedPackageNames))

class UninstallPackageField(PackageField):
    def __init__(self, name = "uninstallPackageName"):
        PackageField.__init__(self, name)

    def possiblePackageNames(self, hostName, packageName):
        progressData = getProgressData(hostName)
        installedPackageNames =  getInstalled(progressData)[0]
        possibleMatches = []
        for i in installedPackageNames:
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
