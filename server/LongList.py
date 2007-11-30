#!/usr/bin/python

import sys,re

import PinshCmd
from commonUtil import *

class LongList(PinshCmd.PinshCmd):
    def __init__(self, obj, unique=False):
        PinshCmd.PinshCmd.__init__(self, "NONE")
        self.item = obj
        self.helpText = self.item.helpText
        self.myName = "LIST OF "+self.item.myName
        self.level = 99
        self.unique = unique
        self.cmdOwner = 0

    def name(self, tokens, index):
        #print "\nN>",tokens, index
        output = []
        previousTokens = tokens[0:index]
        for i in range(index, len(tokens)):
            newTokens = previousTokens + [tokens[i]]
            tokenNames = self.item.name(newTokens, index)
            if len(tokenNames) == 1:
                if tokenNames[0] in output and self.unique:
                    break
                output.append(tokenNames[0])
            if len(tokenNames) == 0:
                break
            if len(tokenNames) > 1:
                return tokenNames
        if not output:
            return ''
        returnValue = output[-1]
        if returnValue:
            return [output[-1]]
        else:
            return ''

    def fullName(self, tokens, index):
        #print "\nF>",tokens, index
        output = []
        previousTokens = tokens[0:index]
        for i in range(index, len(tokens)):
            newTokens = previousTokens + [tokens[i]]
            tokenNames = self.item.name(newTokens, index)
            if len(tokenNames) == 1:
                if tokenNames[0] in output and self.unique:
                    break
                output.append(tokenNames[0])
            if len(tokenNames) == 0:
                break
            if len(tokenNames) > 1:
                base = ' '.join(output)
                return [ "%s %s" % (base, x) for x in tokenNames]
        if not output:
            return ''
        returnValue = ' '.join(output)
        if returnValue:
            return [' '.join(output)]
        else:
            return ''

    def match(self, tokens, index):
        #print "\nM>",tokens, index
        names = self.fullName(tokens, index)
        if names == '':
            return NO_MATCH, 1
        lengthOfMatch = len(names[0].split(' '))
        if len(names) == 1:
            if names[0].split()[-1] != tokens[-1]:
                return PARTIAL, lengthOfMatch
            return COMPLETE, lengthOfMatch
        return PARTIAL, lengthOfMatch

if __name__ == "__main__":
    from libTest import *
    import Integer, sys
    i = Integer.Integer(1, 100)
    ll = LongList(i)
    status = OK
    startTest()
    status = runTest(ll.name, [["1"], 0], ["1"], status)
    status = runTest(ll.name, [["1", "1", "1"], 0], ["1"], status)
    status = runTest(ll.name, [["1", "2", "3"], 0], ["3"], status)
    status = runTest(ll.name, [["1", "2", "a"], 0], ["2"], status)
    status = runTest(ll.name, [["1", "a", "2"], 0], ["1"], status)
    status = runTest(ll.name, [[""], 0], "", status)
    status = runTest(ll.match, [[""], 0], (NO_MATCH, 1), status)
    status = runTest(ll.match, [["1", "2", "3"], 0], (COMPLETE, 3), status)
    import PackageField
    pf = PackageField.PurgablePackageField()
    ll2 = LongList(pf, unique=True)
    status = runTest(ll2.name, [["testdb", "Sql"], 1], ["SqlBackup-8"], status)
    output = ['casDB-patch-R3O-9', 'uhrDB-patch-R3AB_7-4', 'mcsDB-patch-R2S-1', 'casDB-patch-R3Q-5', 'rmsDB-install-12', 'uhrDB-patch-R3AB_9-5', 'casDB-patch-R3X-11', 'rmsDB-patch-R3I-8', 'LogReport-193-3', 'uhrDB-patch-R3AB_6-7', 'uhrDB-patch-R3K-8', 'CgAuthorization-1', 'rmsDB-patch-R3K-8', 'HostAuthorization-24', 'uhrDB-patch-R3S-8', 'uhrDB-patch-R3X-6', 'rmsDB-patch-R3V-10', 'uhrDB-install-11', 'mcsDB-patch-R2AD-6', 'mcsDB-patch-R2U-1', 'uhrDB-patch-R3N-7', 'mcsDB-patch-R2Z-2', 'mcsDB-patch-R2X-3', 'mcsDB-patch-R2Q2-1', 'mcsDB-patch-R2AA-4', 'rmsDB-patch-R3S-5', 'casDB-patch-R3L-8', 'uhrDB-patch-R3AB-7', 'TestPackage-2', 'casDB-patch-R3U-2', 'casDB-patch-R3M-9', 'casDB-install-14', 'uhrDB-patch-R3Z-5', 'rmsDB-patch-R3T-8', 'rmsDB-patch-R3U-3', 'uhrDB-patch-R3AB_5-6', 'DbAuthorization-1', 'rmsDB-patch-R3Y_1-8', 'uhrDB-patch-R3Q-7', 'uhrDB-patch-R3H-8', 'uhrDB-patch-R3J-7', 'mcsDB-patch-R2Q-2', 'CgDbSettings-1', 'casDB-patch-R3Y_1-9', 'rmsDB-patch-R3L-8', 'rmsDB-patch-R3N-9', 'mcsDB-patch-R2T-4', 'uhrDB-patch-R3U-10', 'SqlBackup-8', 'rmsDB-patch-R3Y_5-6', 'casDB-patch-R3J-9', 'mcsDB-install-1', 'uhrDB-patch-R3AB_13-4']
    output2 = [ x for x in output if x.startswith("uhrDB") ]
    status = runTest(ll2.name, [["testdb", "Sql", ""], 1], output, status)
    status = runTest(ll2.match, [["testdb", "Sql"], 1], (PARTIAL, 1), status)
    status = runTest(ll2.match, [["testdb", "Sql", "Hos"], 1], (PARTIAL, 2), status)
    status = runTest(ll2.match, [["testdb", "Sql", "Sql"], 1], (PARTIAL, 1), status)
    status = runTest(ll2.name, [["purge", "testdb", "HostAuthorization-24", "Log"], 2], ["LogReport-193-3"], status)
    status = runTest(ll2.match, [['purge', 'testdb', 'HostAuthorization-24', 'Log'], 2], (PARTIAL, 2), status)
    status = runTest(ll2.name, [['purge', 'testdb', 'uhrDB-patch-R3AB_7', 'uhrDB'], 2], output2, status)
    status = runTest(ll2.fullName, [['purge', 'testdb', 'uhrDB-patch-R3AB_7', 'uhrDB-patch-R3N'], 2], ["uhrDB-patch-R3AB_7-4 uhrDB-patch-R3N-7"], status)
    status = runTest(ll2.match, [['purge', 'testdb', 'uhrDB-patch-R3AB_7', 'uhrDB'], 2], (PARTIAL, 2), status)
    endTest(status)
