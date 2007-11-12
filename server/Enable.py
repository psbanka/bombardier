#!/usr/bin/python

import sys, getpass
import Client
import PinshCmd, Mode
from commonUtil import *

class Enable(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "enable", "enable\tmake all priveleged commands available")
        self.level = Mode.ENABLE
        self.auth = USER
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            if mode.password != '':
                mode.password = ''
                mode.auth = USER
                mode.popPrompt()
                return OK, []
            return FAIL, []
        if mode.password != '':
            return FAIL, ["Already in enable mode"]
        passwd = getpass.getpass("password: ")
        client = Client.Client("test", passwd)
        client.get()
        try:
            client.decryptConfig()
        except:
            return FAIL, ["Invalid password"]
        mode.password = passwd    
        mode.auth = ADMIN
        mode.pushPrompt(slash, "#", Mode.ENABLE)
        return OK, []

if __name__ == "__main__":
    from libTest import *
    status = startTest()
    enable = Enable()
    status = testMe(enable, "enable", OK, '', status)
    endTest(status)
