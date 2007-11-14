#!/usr/bin/python

import sys
import os

import PinshCmd
from commonUtil import *

DEBUG = 0

os.environ["no_bom_sh"] = "1"
#BASH = "bashem"
BASH = "/bin/bash"
BASHRC = "/etc/nobomshrc" #^ FIXME!

class Expert(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "expert")
        self.helpText = "expert\tchange over to expert mode"
        self.level = 0
        self.auth = ADMIN
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return OK, []
        #os.system(BASH+' --init-file '+BASHRC)
        #os.chdir('/')
        os.system(BASH)

if __name__ == "__main__":
    from libTest import *
    status = startTest()
    expert = Expert()
    print ">>>>>>>>> TYPE C-d"
    status = testMe(expert, "expert", OK, "", status)
    endTest(status)

