#!/usr/bin/python

import sys, os

import PinshCmd, BomHostField, Client, Expression
from bombardier_core.libCipher import DecryptionException
from commonUtil import *

SSH = "/usr/bin/ssh"

class Ssh(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "ssh")
        self.helpText = "ssh\tssh to another host"
        self.bomHostField = BomHostField.BomHostField()
        self.expression = Expression.Expression("ssh command")
        self.children = [self.bomHostField]
        self.bomHostField.children = [self.expression]
        self.cmdOwner = 1
        self.logCommand = True

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        if noFlag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        hostName = tokens[1]

        #r = mode.getBomConnection(hostName)
        #if len(tokens) > 2:
            #command = '%s'%(' '.join(tokens[2:]))
        #else:
            #command = ''
        #status = r.interact(command)
        #return status, []

        client = Client.Client(hostName, mode.password, mode.serverHome)
        try:
            client.get()
        except IOError:
            return FAIL, ["No such client '%s'" % hostName]
        if mode.password:
            try:
                client.decryptConfig()
            except DecryptionException, de:
                msg  = ["Cannot decrypt configuration data."]
                msg += [de.reason]
                return FAIL, msg
        username = client.data.get("defaultUser")
        address  = client.data.get("ipAddress")
        if not username or not address:
            if client.data.get("enc_defaultUser") or \
                client.data.get("enc_ipAddress"):
                return FAIL, ["Enable mode is required to connect to this system"]
            else:
                msg  = ["ipAddress and defaultUser are required in the system"]
                msg += ["configuration in order to access this host."]
                return FAIL, msg
            return FAIL, ["Cannot log in"]
        if len(tokens) > 2:
            command = '"%s"'%(' '.join(tokens[2:]))
        else:
            command = ''
        if username and address:
            os.system('%s %s@%s %s'%(SSH,username,address,command))
        return OK, []
