#!/usr/bin/python

import sys

import PinshCmd, Mode, libUi, ConfigField, Expression
from commonUtil import *

class Set(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "set", "set\tset a configuration value")
        # TOP LEVEL
        self.client = PinshCmd.PinshCmd("client","client\tchange the configuration of one client")
        self.bom = PinshCmd.PinshCmd("bom","bom\tchange a bill of materials")
        self.include = PinshCmd.PinshCmd("include","include\tchange an include file")
        self.children = [self.client, self.bom, self.include]

        # CLIENT
        self.clientConfigField = ConfigField.ConfigField(dataType=ConfigField.CLIENT)
        self.client.children = [self.clientConfigField]
        expression = Expression.Expression()
        self.clientConfigField.children = [expression]

        # INCLUDE
        self.includeConfigField = ConfigField.ConfigField(dataType=ConfigField.INCLUDE)
        self.include.children += [self.includeConfigField]
        self.includeConfigField.children = [expression]

        # BOM
        self.bomConfigField = ConfigField.ConfigField(dataType=ConfigField.BOM)
        self.bom.children += [self.bomConfigField]
        self.bomConfigField.children = [expression]

        self.cmdOwner = 1

    def getFieldObject(self, tokens):
        configType = tokens[1].lower()
        if configType.startswith('c'):
            fieldObject = self.clientConfigField
        elif configType.startswith('i'):
            fieldObject = self.includeConfigField
        elif configType.startswith('b'):
            fieldObject = self.bomConfigField
        else:
            fieldObject = None
        return fieldObject

    def getNewValue(self, noFlag, tokens):
        if len(tokens) >= 4:
            newValue = ' '.join(tokens[3:])
        else:
            if noFlag:
                newValue = ''
            else:
                newValue = libUi.pwdInput("Enter configuration value: ")                
        return newValue

    def modifyList(self, noFlag, tokens, currentValue, newValue, fieldObject):
        if noFlag:
            if newValue in currentValue:
                currentValue.remove(newValue)
                fieldObject.setValue(tokens, 2, currentValue)
                return OK, ["%s removed from list" % newValue]
            try:
                if int(newValue) in currentValue:
                    currentValue.remove(int(newValue))
                    fieldObject.setValue(tokens, 2, currentValue)
                    return OK, ["%s removed from list" % newValue]
            except:
                pass
            return FAIL, ["%s is not in the current list of values." % newValue]
        currentValue.append(newValue)
        fieldObject.setValue(tokens, 2, currentValue)
        return OK, ["%s appended to list" % newValue]

    def modifyScalar(self, noFlag, tokens, newValue, fieldObject):
        if noFlag:
            fieldObject.setValue(tokens, 2, '')
            return OK, ["Value removed."]
        fieldObject.setValue(tokens, 2, newValue)
        return OK, []

    def cmd(self, tokens, noFlag, slash):
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        fieldObject = self.getFieldObject(tokens)
        if not fieldObject:
            return FAIL, ["Unknown command: %s" % configType]
        currentValue = fieldObject.getSpecificData(tokens, 2)
        newValue = self.getNewValue(noFlag, tokens)
        if type(currentValue) == type(['list']):
            return self.modifyList(noFlag, tokens, currentValue, newValue, fieldObject)
        elif type(currentValue) in [type('string'), type(1), type(True)]:
            return self.modifyScalar(noFlag, tokens, newValue, fieldObject)
