#!/usr/bin/python

import sys, getpass, os
import libCipher, libUi
import PinshCmd, Mode, BomHostField, LongList
from commonUtil import *

class Enable(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "enable", "enable\tmake all priveleged commands available")
        self.level = Mode.ENABLE
        self.bomHostField = BomHostField.BomHostField()
        self.hostList = LongList.LongList(self.bomHostField, unique=True)
        self.children = [self.hostList]
        self.auth = USER
        self.cmdOwner = 1

    def enableUser(self, noFlag, slash):
        if noFlag:
            return self.exit(slash)
        if not os.path.isfile(PASSWORD_FILE):
            output = ['User Denied: "%s" doesn\'t exist' % (PASSWORD_FILE)]
            output.append("Use the 'set password' command to authorize.")
            return FAIL, output
        if mode.auth == ADMIN:
            return FAIL, ["Already in enable mode"]
        cipherPass = open(PASSWORD_FILE).read().strip()
        mode.myPassword = libUi.pwdInput("password: ")
        if 1 == 1:
        #try:
            padPassword = libCipher.pad(mode.myPassword)
            mode.password = libCipher.decryptString(cipherPass, padPassword)
        else:
        #except:
            mode.password = ''
            mode.myPassword = ''
            return FAIL, ["Invalid password"]
        mode.auth = ADMIN
        mode.pushPrompt(slash, "#", Mode.ENABLE)
        mode.exitMethods.append(self.exit)
        return OK, []

    def exit(self, slash):
        mode.password = ''
        mode.auth = USER
        mode.popPrompt()
        if mode.newClasses:
            extraClasses = mode.newClasses[-1]
            for i in range(0,extraClasses):
                slash.children.pop()
        return OK, []

    def cmd(self, tokens, noFlag, slash):
        if len(tokens) == 1 or tokens[1] == '':
            return self.enableUser(noFlag, slash)
        if mode.auth != ADMIN:
            return FAIL, ["You must be in enable mode to use this feature."]
        pubKeyFile = "%s/.ssh/id_dsa.pub" % os.environ["HOME"]
        if not os.path.isfile(pubKeyFile):
            return FAIL, ['You must generate a DSA ssh key with "ssh-keygen -t dsa"']
        try:
            hostNames = self.hostList.name(tokens, 1)[0]
        except:
            return FAIL, ["Invalid host name: %s" % tokens[2]]
        overallStatus = OK
        overallOutput = []
        for hostName in hostNames.split():
            r = mode.getBomConnection(hostName)
            r.password = mode.password
            r.sharedKey = False
            remoteKeyFile = "%s_id_dsa.pub" % os.environ["USER"]
            r.scp(pubKeyFile, ".ssh/%s" % remoteKeyFile)
            status, output = r.runCmd("cd ~/.ssh && cat %s >> authorized_keys2" % remoteKeyFile)
            print output
            r.password = ''
            r.sharedKey = True
            if status == OK:
                overallOutput.append("Added key to %s" % hostName)
            else:
                overallStatus = FAIL
                overallOutput.append("Unable to add key to %s." % hostName)
        return overallStatus, overallOutput
