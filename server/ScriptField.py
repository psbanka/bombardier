#!/usr/bin/python

import PinshCmd, PackageField, BomHostField
import yaml, syck
from commonUtil import *

class ScriptField(PinshCmd.PinshCmd):
    def __init__(self, name = "packageName"):
        PinshCmd.PinshCmd.__init__(self, name)
        self.helpText = "<scriptName>\tthe name of a bombardier maintenance script"
        self.installedPackageField = PackageField.InstalledPackageField()
        self.bomHostField = BomHostField.BomHostField()
        self.level = 99
        self.cmdOwner = 0

    def possibleScriptNames(self, packageName, scriptName):
        #packageName = '-'.join(packageName.split('-')[:-1])
        data = syck.load(open("%s/deploy/packages/packages.yml" %mode.serverHome).read())
        scriptNames = data[packageName].get("executables", [])
        possibleCompletions = []
        for i in scriptNames:
            if i.lower().startswith(scriptName.lower()):
                possibleCompletions.append(i)
        return possibleCompletions

    def preferredNames(self, tokens, index):
        hostNames = self.bomHostField.preferredNames(tokens, index-2)
        if len(hostNames) != 1:
            return ''
        hostName = hostNames[0]
        partialPackageName = tokens[index-1]
        packageNames = self.installedPackageField.possiblePackageNames(hostName, partialPackageName)
        if len(packageNames) != 1:
            return ''
        packageName = packageNames[0]
        if len(tokens) <= index:
            return ''
        partialScriptName  = tokens[index]
        possibleMatches = self.possibleScriptNames(packageName, partialScriptName)
        if possibleMatches:
            return possibleMatches
        return ''

    def match(self, tokens, index):
        possibleMatches = self.preferredNames(tokens, index)
        if not possibleMatches:
            return NO_MATCH, 1
        if len(possibleMatches) > 1:
            return PARTIAL, 1
        return COMPLETE, 1

if __name__ == "__main__":
    from libTest import startTest, runTest, endTest
    scriptField = ScriptField()
    status = OK
    startTest()
    status = runTest(scriptField.preferredNames, [["bigap", "Conn", "conn"], 2], ["connectTest"], status)
    status = runTest(scriptField.preferredNames, [["bigs", "con", "co"], 2], ["connectTest"], status)
    status = runTest(scriptField.preferredNames, [["virtap", "a", "a"], 2], "", status)
    status = runTest(scriptField.preferredNames, [["foo", "Conn", 'conn'], 2], '', status)
    endTest(status)

