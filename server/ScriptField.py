#!/usr/bin/python

import PinshCmd, PackageField, BomHostField
import yaml
from commonUtil import *

def getSubsection(name):
    d = open("deploy/packages/packages.yml").readlines()
    output = []
    collect = False
    for line in d:
        if collect:
            output.append(line)
            if line[0] != ' ':
                return yaml.load('\n'.join(output))
        if line[0] == ' ':
            continue
        if line == '%s:\n' % name:
            output.append(line)
            collect = True
    return {}

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
        data = getSubsection(packageName)
        scriptNames = data[packageName].get("executables", [])
        possibleCompletions = []
        for i in scriptNames:
            if i.lower().startswith(scriptName.lower()):
                possibleCompletions.append(i)
        return possibleCompletions

    def name(self, tokens, index):
        hostNames = self.bomHostField.name(tokens, index-2)
        if len(hostNames) != 1:
            return ''
        hostName = hostNames[0]
        partialPackageName = tokens[index-1]
        packageNames = self.installedPackageField.possiblePackageNames(hostName, partialPackageName)
        if len(packageNames) != 1:
            return ''
        packageName = packageNames[0]
        partialScriptName  = tokens[index]
        possibleMatches = self.possibleScriptNames(packageName, partialScriptName)
        if possibleMatches:
            return possibleMatches
        return ''

    def match(self, tokens, index):
        possibleMatches = self.name(tokens, index)
        if not possibleMatches:
            return NO_MATCH, 1
        if len(possibleMatches) > 1:
            return PARTIAL, 1
        return COMPLETE, 1

if __name__ == "__main__":
    from libTest import *
    scriptField = ScriptField()
    status = OK
    startTest()
    status = runTest(scriptField.name, [["bigap", "Conn", "conn"], 2], ["connectTest"], status)
    status = runTest(scriptField.name, [["bigs", "con", "co"], 2], ["connectTest"], status)
    status = runTest(scriptField.name, [["virtap", "a", "a"], 2], "", status)
    status = runTest(scriptField.name, [["foo", "Conn", 'conn'], 2], '', status)
    endTest(status)

