#!/usr/bin/python

import sys
import PinshCmd, Mode, Variable, List, ConfigField
from commonUtil import *

class For(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "for")
        self.helpText = "for\tloop over a list of values"
        self.variable = Variable.Variable()
        self.children = [self.variable]
        inCmd = PinshCmd.PinshCmd('in')
        self.variable.children = [inCmd]
        self.varList = List.List()
        self.configField = ConfigField.ConfigField()
        inCmd.children = [self.configField, self.varList]
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        if noFlag:
            return FAIL, ['Type "exit" to leave for loop.']
        if len(tokens) < 4:
            return FAIL, ["Incomplete command"]
        if mode.state[-1] == Mode.ENABLE:
            newMode = Mode.F0
        else:
            newMode = mode.state[-1] + 1
            if newMode > Mode.F2:
                return FAIL, ["Too many layers of nesting"]

        variableNames = self.variable.name(tokens, 1)
        if len(variableNames) != 1:
            return FAIL, ["Invalid variable name. Must contain alphanumeric and underscore characters only"]
        variableName = variableNames[0]
        
        status, length = self.varList.match(tokens, 3)
        if status == PARTIAL:
            return FAIL, ["Incomplete list. Need to end it with a ']'"]
        if status == COMPLETE:
            values = tokens[4:-1]
        else:
            configFieldNames = self.configField.name(tokens, 3)
            if len(configFieldNames) == 0:
                return FAIL, ["%s is not a valid config entry." % tokens[-1]]
            values = self.configField.getConfigData(tokens, 3)
            if not values:
                return FAIL, ["%s is not iterable." % tokens[-1]]

        mode.variables[newMode] = (variableName, values)
        mode.commandBuffer[newMode] = []
        mode.pushPrompt(slash, "-for %s#" % variableName, newMode, 0)
        return OK, []

