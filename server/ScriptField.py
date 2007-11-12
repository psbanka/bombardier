#!/usr/bin/python

import PinshCmd
import yaml
from commonUtil import *
from Client import SERVER_PATH

def getSubsection(name):
    d = open("%s/deploy/packages/packages.yml" % SERVER_PATH).readlines()
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
        self.level = 99
        self.cmdOwner = 0

    def possibleScriptNames(self, packageName, scriptName):
        packageName = '-'.join(packageName.split('-')[:-1])
        data = getSubsection(packageName)
        scriptNames = data[packageName].get("executables", [])
        possibleCompletions = []
        for i in scriptNames:
            if i.lower().startswith(scriptName.lower()):
                possibleCompletions.append(i)
        return possibleCompletions

    def name(self, tokens):
        packageName = tokens[2]
        scriptName  = tokens[3]
        possibleMatches = self.possibleScriptNames(packageName, scriptName)
        if possibleMatches:
            return possibleMatches
        return ''

    def match(self, tokens, index):
        if tokens[index] == '':
            return NO_MATCH, 1
        packageName = tokens[index-1]
        scriptName = tokens[index]
        possibleMatches = self.possibleScriptNames(packageName, scriptName)
        if len(possibleMatches) == 0:
            return INCOMPLETE, 1
        elif len(possibleMatches) == 1:
            return COMPLETE, 1
        else:
            return PARTIAL, 1

