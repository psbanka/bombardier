#!/usr/bin/python

import sys, readline
import yaml
import Client
import PinshCmd, ConfigField, Integer
from commonUtil import *

class ShowCommand(PinshCmd.PinshCmd):
    def __init__(self, name, helpText):
        PinshCmd.PinshCmd.__init__(self, name, helpText)
        self.configField = None
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return FAIL, []
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        currentDict = self.configField.getSpecificData(tokens, 2)
        return OK, yaml.dump(currentDict, default_flow_style=False).split('\n')

class Merged(ShowCommand):
    def __init__(self):
        ShowCommand.__init__(self, "merged", "merged\tdisplay a merged configuration")
        self.configField = ConfigField.ConfigField()
        self.children = [self.configField]

class Client(ShowCommand):
    def __init__(self):
        ShowCommand.__init__(self, "client", "client\tshow the configuration for one client")
        self.configField = ConfigField.ConfigField(dataType=ConfigField.CLIENT)
        self.children = [self.configField]

class Include(ShowCommand):
    def __init__(self):
        ShowCommand.__init__(self, "include", "include\tshow a shared include file")
        self.configField = ConfigField.ConfigField(dataType=ConfigField.INCLUDE)
        self.children = [self.configField]

class Bom(ShowCommand):
    def __init__(self):
        ShowCommand.__init__(self, "bom", "bom\tshow a bill of materials")
        self.configField = ConfigField.ConfigField(dataType=ConfigField.BOM)
        self.children = [self.configField]

class History(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "history")
        self.helpText = "history\tdisplay the history of commands"
        self.integer  = Integer.Integer(min=1, max=1000)
        self.children = [self.integer]
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if len(tokens) == 2 or tokens[-1].strip()=='':
            number = 20
        else:
            try:
                number = int(tokens[2])
            except:
                return FAIL, ["%s is not a number." % tokens[2]]
        hlen = readline.get_current_history_length()
        if hlen < number:
            number = hlen
        output = []
        for i in range(hlen, hlen-number, -1):
            output.append("%4d\t%s" % (i, readline.get_history_item(i)))
        return OK, output

class Show(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "show")
        self.helpText = "show\tdisplay components of the system"
        history = History()
        merged = Merged()
        client = Client()
        include = Include()
        bom = Bom()
        self.children = [merged, client, include, bom, history]
        self.level = 0
        self.cmdOwner = 1

