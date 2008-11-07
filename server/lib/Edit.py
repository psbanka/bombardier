#!/usr/bin/python

import sys, os, yaml

import PinshCmd, ConfigField
from commonUtil import *


class Edit(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "edit")
        self.helpText = "edit\tedit a configuration file"
        self.level = 0
        self.cmdOwner = 1

        self.client = PinshCmd.PinshCmd("client","client\tchange the configuration of one client")
        self.bom = PinshCmd.PinshCmd("bom","bom\tchange a bill of materials")
        self.include = PinshCmd.PinshCmd("include","include\tchange an include file")
        self.children = [self.client, self.bom, self.include]

        # CLIENT
        self.clientConfigField = ConfigField.ConfigField(dataType=ConfigField.CLIENT, strict=False)
        self.client.children = [self.clientConfigField]

        # INCLUDE
        self.includeConfigField = ConfigField.ConfigField(dataType=ConfigField.INCLUDE, strict=False)
        self.include.children += [self.includeConfigField]

        # BOM
        self.bomConfigField = ConfigField.ConfigField(dataType=ConfigField.BOM, strict=False)
        self.bom.children += [self.bomConfigField]

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        if noFlag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        editor = mode.editor
        clientFile = False

        if len(tokens) > 1:
            if tokens[1].lower().startswith('c'):
                filePath = os.path.join(mode.serverHome, "client")
                clientFile = True
            elif tokens[1].lower().startswith('i'):
                filePath = os.path.join(mode.serverHome, "include")
            elif tokens[1].lower().startswith('b'):
                filePath = os.path.join(mode.serverHome, "bom")
            if len(tokens) > 2:
                filePath = os.path.join(filePath, tokens[2].split('.')[0])
                filePath += '.yml'
        else:
            filePath = mode.serverHome

        output = []
        if not os.path.isfile(filePath):
            if clientFile:
                minimumConfig = {"ipAddress": "127.0.0.1",
                                 "platform": mode.global_config.get("defaultPlatform", "linux"),
                                 "defaultUser": mode.global_config.get("defaultUser", "root")}
                open(filePath, 'w').write(yaml.dump(minimumConfig, default_flow_style=False))
            output.append("New file: %s" % filePath)
        os.system("%s %s" % (editor, filePath))
        return OK, output
