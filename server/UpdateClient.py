#!/usr/bin/python

import sys
from update import UpdateRemoteClient
import PinshCmd, BomHostField
from commonUtil import *

class UpdateClient(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "updateClient")
        self.helpText = "updateClient\tupdate bombardier on a remote system"
        self.bomHostField = BomHostField.BomHostField()
        self.children = [self.bomHostField]
        self.level = 0
        self.cmdOwner = 1
        self.logCommand = True

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        if noFlag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        hostNames = self.bomHostField.name(tokens, 1)
        if len(hostNames) == 0:
            return FAIL, ["Invalid host %s" % tokens[1]]
        if len(hostNames) > 1:
            return FAIL, ["Ambiguous host %s" % tokens[1]]
        hostName = hostNames[0]
        r = UpdateRemoteClient(hostName, mode.password, mode.dataPath, slash.fpOut, mode.packageData, mode.config.get("svnPath"))
        r.update()
        status = OK
        if status == FAIL:
            return FAIL, ["Host is screwed up"]
        else:
            return OK, ['']
