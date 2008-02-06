#!/usr/bin/python

import PinshCmd, BomCmd
from commonUtil import *

class Package(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "package")
        self.childrenDict = { "status": BomCmd.Status(), "uninstall":BomCmd.Uninstall(), 
            "verify": BomCmd.Verify(), "configure": BomCmd.Configure(), "install":BomCmd.Install(), 
            "execute": BomCmd.Execute(), "purge": BomCmd.Purge(), "fix": BomCmd.Fix(), 
            "reconcile": BomCmd.Reconcile()}
        self.children = [ self.childrenDict[key] for key in self.childrenDict ]
        self.helpText = "package\tperform a command that operates on another server through a package"
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        command = tokens[1]
        return self.childrenDict[command].cmd(tokens[1:], noFlag, slash)
