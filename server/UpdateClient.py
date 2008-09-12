#!/usr/bin/python

import sys
from update import UpdateRemoteClient
import PinshCmd, BomHostField
from commonUtil import *

class UpdateClient(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "update")
        self.helpText = "update\tupdate the bombardier software on a remote system"
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
        hostName = tokens[1]
        #r = UpdateRemoteClient(hostName, mode.password, mode.dataPath, slash.fpOut, mode.config.get("svnPath"))
        r = UpdateRemoteClient(hostName, mode, slash.fpOut)
        r.update()
        status = OK
        if status == FAIL:
            return FAIL, ["Host is screwed up"]
        else:
            return OK, ['']
