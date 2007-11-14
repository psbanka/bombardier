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
        self.configField = ConfigField.ConfigField()
        self.children = [self.configField]
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return FAIL, []
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        currentDict = self.configField.getConfigData(tokens, 2)
        return OK, yaml.dump(currentDict, default_flow_style=False).split('\n')

class Show(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "show")
        self.helpText = "show\tdisplay components of the system"
        server = Server()
        self.children = [server]
        self.level = 0
        self.cmdOwner = 1

