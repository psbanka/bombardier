#!/usr/bin/python

import sys, time

import PinshCmd, ScpHostField
from commonUtil import *

class Rsync(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "rsync")
        self.helpText = "rsync\trsync between hosts"
        self.scpHostField = ScpHostField.ScpHostField()
        scpHostField2 = ScpHostField.ScpHostField()
        self.children = [self.scpHostField]
        self.scpHostField.children = [scpHostField2]
        self.cmdOwner = 1
        self.logCommand = True

    def getCleanPath(self, path):
        possibleNames = self.scpHostField.name([path], 0)
        if len(possibleNames) < 1:
            return '', NO_MATCH
        elif len(possibleNames) > 1:
            if path in possibleNames:
                return path, COMPLETE
            else:
                return '', PARTIAL
        else:
            if path in possibleNames:
                return path, COMPLETE
            return possibleNames[0], COMPLETE

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        if noFlag:
            return FAIL, []
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]

        source, matchType = self.getCleanPath(tokens[1])
        if matchType == PARTIAL:
            return FAIL, ["Source path is ambiguous."]
        elif matchType == NO_MATCH:
            return FAIL, ["Source path not found."]

        dest, matchType = self.getCleanPath(tokens[2])
        if matchType == PARTIAL:
            return FAIL, ["Destination path is ambiguous."]
        elif matchType == NO_MATCH:
            return FAIL, ["Destination path not found."]

        sourceHostNames = self.scpHostField.getPossibleHostNames(source)
        destHostNames   = self.scpHostField.getPossibleHostNames(dest)
        localPath       = ''
        remotePath      = ''
        direction       = "PUSH"
        status          = OK

        now = time.time()
        if sourceHostNames:
            s = mode.getBomConnection(sourceHostNames[0])
            direction  = "PULL"
            remotePath = source.split(':')[-1]
            deleteFlag = False
            if destHostNames:
                if source.endswith('/'):
                    localDir  = source.split(':')[0]+'-'+dest.split(':')[1][:-1].replace('/','-')
                else:
                    localDir  = source.split(':')[0]+'-'+dest.split(':')[1].replace('/','-')
                localPath = mode.config["tmpPath"]+'/'+localDir
                deleteFlag = True
            else:
                localPath = dest
            status = s.rsync(localPath, remotePath, direction, deleteFlag = deleteFlag)

        if destHostNames:
            d = mode.getBomConnection(destHostNames[0])
            direction  = "PUSH"
            remotePath = dest.split(':')[-1]
            if not localPath:
                if status == OK:
                    localPath = source
                else:
                    return FAIL, ["Not pushing to remote system because pull failed"]
            if deleteFlag:
                localPath += '/*'
            status = d.rsync(localPath, remotePath, direction)
        elapsed = time.time() - now

        return status, ["Rsync successful (%3.2f seconds elapsed)." % elapsed]
