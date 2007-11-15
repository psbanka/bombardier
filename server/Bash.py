#!/usr/bin/python

import sys
import os

import PinshCmd
from commonUtil import *

DEBUG = 0

os.environ["no_bom_sh"] = "1"
BASH = "/bin/bash"

class Bash(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "bash")
        self.helpText = "bash\trun an interactive bash shell"
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return OK, []
        os.system(BASH)

if __name__ == "__main__":
    pass
