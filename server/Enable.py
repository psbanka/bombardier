#!/usr/bin/python

import sys, os
import libCipher, libUi
import PinshCmd, Mode, BomHostField, LongList
from RemoteClient import ClientUnavailableException
from commonUtil import *

def performEnable(slash):
    cipherPass = mode.config.get("password")
    mode.password = libUi.pwdInput("password: ")
    try:
        padPassword = libCipher.pad(mode.password)
        mode.password = libCipher.decryptString(cipherPass, padPassword)
    except:
        mode.password = ''
        return FAIL, ["Invalid password"]
    mode.auth = ADMIN
    mode.pushPrompt(slash, "#", Mode.ENABLE)
    return OK, []

class Enable(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "enable", "enable\tmake all priveleged commands available")
        self.level = Mode.ENABLE
        self.bomHostField = BomHostField.BomHostField(enabled = False)
        self.hostList = LongList.LongList(self.bomHostField, unique=True)
        self.children = [self.hostList]
        self.auth = USER
        self.cmdOwner = 1

    def enableUser(self, noFlag, slash):
        if noFlag:
            return self.exit(slash)
        if not mode.config.get("password"):
            output = [ "User denied: Use the 'set password' command to authorize." ]
            return FAIL, output
        if mode.auth == ADMIN:
            return FAIL, ["Already in enable mode"]
        return performEnable(slash)

    def exit(self, slash):
        mode.password = ''
        mode.auth = USER
        mode.popPrompt()
        if mode.newClasses:
            extraClasses = mode.newClasses[-1]
            while extraClasses > 0:
                slash.children.pop()
                extraClasses -= 1
        return OK, []

    def cmd(self, tokens, noFlag, slash):
        sshDir = "%s/.ssh" % os.environ["HOME"]
        if not os.path.isdir(sshDir):
            os.makedirs(sshDir)
            os.system("chmod 700 %s" % sshDir)
        if len(tokens) == 1 or tokens[1] == '':
            return self.enableUser(noFlag, slash)
        if mode.auth != ADMIN:
            return FAIL, ["You must be in enable mode to use this feature."]
        pubKeyFile = "%s/.ssh/id_dsa.pub" % os.environ["HOME"]
        if not os.path.isfile(pubKeyFile):
            return FAIL, ['You must generate a DSA ssh key with "ssh-keygen -t dsa"']
        try:
            hostNames = self.hostList.preferredNames(tokens, 1)
        except:
            return FAIL, ["Invalid host name: %s" % tokens[2]]
        overallStatus = OK
        overallOutput = ['\n']
        for hostName in hostNames.split():
            r = mode.getBomConnection(hostName, ignoreConfig=True)
            r.password = mode.password
            r.sharedKey = False
            remoteKeyFile = "%s_id_dsa.pub" % os.environ["USER"]
            try:
                r.scp(pubKeyFile, ".ssh/%s" % remoteKeyFile)
            except ClientUnavailableException:
                return FAIL, ['\n', "Client %s (IP: %s) will not accept an SCP connection." % (hostName, r.ipAddress)]
            status, output = r.runCmd("cd ~/.ssh && cat %s >> authorized_keys2" % remoteKeyFile)
            r.password = ''
            r.sharedKey = True
            if status == OK:
                mode.addPersonalConfigList("enabledSystems", hostName)
                mode.enabledSystems.append(hostName)
                overallOutput.append("Added key to %s" % hostName)
            else:
                overallStatus = FAIL
                overallOutput.append("Unable to add key to %s." % hostName)
        return overallStatus, overallOutput
