#!/usr/bin/python

import sys, os
from commands import getstatusoutput as gso
import Expression, FileNameField, MultipleChoice
import PinshCmd
from commonUtil import *

def statusAndFile(input, output, directory):
    for line in input.split('\n'):
        if len(line.split()) != 2:
            continue
        stat, fileName = line.split()
        name = fileName.split(os.path.sep)[-1].split('.yml')[0]
        output[directory].append('%s\t%s' % (stat, name))
    return output

def logParser(input, output, directory):
    newItem = ''
    for line in input.split('\n'):
        if line.startswith('-'):
            newItem = ''
            continue
        elif line.strip() == '':
            continue
        elif line.startswith('r'):
            info = [ x.strip() for x in line.split('|') ]
            rev = info[0]
            date = info[2]
            newItem = "%s | %s" % (rev, date)
        else:
            output[directory].append(" %s | %s" % (newItem, line))
    return output

class Cmdb(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "cmdb")
        self.helpText = "cmdb\toperate on the Configuration Management Database"
        self.status = PinshCmd.PinshCmd("status","status\tdisplay the status of the cmdb")
        self.commit = PinshCmd.PinshCmd("commit","commit\tcommit current changes")
        self.revert = PinshCmd.PinshCmd("revert","revert\trevert all changes")
        self.diff   = PinshCmd.PinshCmd("diff","diff\tdisplay the changes that have been made")
        self.add    = PinshCmd.PinshCmd("add","add\tadd a file to the configuration db")
        self.log    = PinshCmd.PinshCmd("log","log\treview the changes that have been made")
        self.comment = Expression.Expression()

        self.fileNameField = FileNameField.FileNameField(mode.serverHome)
        self.diff.children = [self.fileNameField]
        self.add.children = [self.fileNameField]

        self.children = [self.status, self.commit, self.revert, self.diff, self.add, self.log]
        self.commit.children = [self.comment]
        self.cmdOwner = 1
        #self.logCommand = True
        self.svnCmd = mode.config.get("svnPath")
        if not self.svnCmd:
            self.svnCmd = "svn"

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        if noFlag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        if tokens[1].startswith("s"):
            return self.getStatus()
        elif tokens[1].startswith('a'):
            return self.addFile(tokens)
        elif tokens[1].startswith('l'):
            return self.getLog()
        elif tokens[1].startswith('d'):
            return self.getDiff(tokens)
        elif tokens[1].startswith('c'):
            return self.commitChanges(tokens)
        elif tokens[1].startswith('r'):
            return self.revertChanges()

    def runSvn(self, tokens, action):
        if len(tokens) < 3:
            return FAIL, ["Incomplete command"]
        fileName = os.path.join(mode.serverHome, tokens[2])
        if not os.path.isfile(fileName):
            return FAIL, ["Unknown file: %s" % fileName]
        cmd = "%s %s %s" % (self.svnCmd, action, fileName)
        status, output = gso(cmd)
        if status != OK:
            errmsg = ["Repository is corrupt. Please examine it manually"]
            errmsg += output.split('\n')
            return FAIL, errmsg
        return OK, output.split('\n')


    def runSvnMult(self, action, argString='', parseFunction=statusAndFile):
        directories = ["include", "bom", "client"]
        cmdOutput = {}
        for directory in directories:
            cmdOutput[directory] = []
            directoryPath = os.path.join(mode.serverHome, directory)
            cmd = "%s %s %s %s" % (self.svnCmd, action, directoryPath, argString)
            status, output = gso(cmd)
            if status != OK:
                errmsg = ["Repository is corrupt. Please examine it manually"]
                errmsg += [cmd]
                errmsg += output.split('\n')
                return FAIL, errmsg
            cmdOutput = parseFunction(output, cmdOutput, directory)
        return OK, cmdOutput

    def commitChanges(self, tokens):
        comment = ''
        if len(tokens) > 2:
            comment = tokens[-1]
        if not comment:
            comment = raw_input("Please describe this change: ")
        comment = comment.replace("'", '').replace('"', '')
        return self.runSvnMult("commit", "-m '%s'" % comment)

    def addFile(self, tokens):
        return self.runSvn(tokens, "add")

    def getDiff(self, tokens):
        return self.runSvn(tokens, "diff")

    def revertChanges(self):
        if libUi.askYesNo("Revert all changes made to the CMDB", NO) == YES:
            return self.runSvnMult("revert", '-R')
        else:
            return FAIL, ["Command aborted"]

    def getLog(self):
        return self.runSvnMult("log", parseFunction=logParser)

    def getStatus(self):
        return self.runSvnMult("status")
