#!/usr/bin/python

import sys
import yaml
import Client
import PinshCmd, BomHostField, ConfigField
from commonUtil import *

class Server(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "server")
        self.helpText = "server\tshows the configuration for a server"
        self.bomHostField = BomHostField.BomHostField()
        self.children = [self.bomHostField]
        self.configField = ConfigField.ConfigField()
        self.bomHostField.children = [self.configField]
        #self.auth = ADMIN
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return FAIL, []
        if len(tokens) < 3:
            return FAIL, ["Incomplete command"]
        hostName = tokens[2]
        if self.bomHostField.match(tokens, 2) != (COMPLETE, 1):
            return FAIL, ["No server %s" % hostName]
        else:
            hostName = self.bomHostField.name(tokens, 2)[0]
        #config = [x.strip() for x in open("deploy/client/%s.yml" % hostName).readlines()]
        client = Client.Client(hostName, '')
        client.get()
        if len(tokens) == 4:
            configString = tokens[3]
            configName = self.configField.name([hostName, configString], 1)
            if len(configName) == 0:
                return FAIL, ["Unknown configuration option"]
            if len(configName) > 1:
                return FAIL, ["Incomplete configuration option"]
            currentDict = client.data
            for configValue in configName[0].split('.'):
                currentDict = currentDict.get(configValue)
        else:
            currentDict = client.data
        return OK, yaml.dump(currentDict, default_flow_style=False).split('\n')

class Show(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "show")
        self.helpText = "show\tdisplay components of the system"
        server = Server()
        self.children = [server]
        self.level = 0
        self.cmdOwner = 1

