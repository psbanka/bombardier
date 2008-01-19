#!/usr/bin/python

import os, sys

import PinshCmd, Mode, libUi, ConfigField, Expression
from commonUtil import *

import Client, libCipher
def setPassword(slash):
    if not os.path.isfile(PASSWORD_FILE):
        masterPass = libUi.pwdInput("Enter master password to authorize this user: ")
        client = Client.Client("test", masterPass)
        client.downloadClient()
        try:
            status = client.decryptConfig()
        except:
            return FAIL, ['Incorrect master password']
        mode.password = masterPass
        print "% Password correct, continue"
    else:
        if mode.auth != ADMIN:
            return FAIL, ["Must be done from enable mode"]
    bomDir = '/'.join(PASSWORD_FILE.split('/')[:-1])
    #bomDir = PASSWORD_FILE.rpartition('/')[0]
    if not os.path.isdir(bomDir):
        os.makedirs(bomDir)
    testPass1 = libUi.pwdInput("new password: ")
    if testPass1 == '':
        return FAIL, ["Aborted"]
    testPass2 = libUi.pwdInput("new password (confirm): ")
    if testPass1 != testPass2:
        return FAIL, ["Passwords do not match"]
    mode.myPassword = testPass1
    try:
        cipherMasterPass = libCipher.encrypt(mode.password, mode.myPassword)
        open(PASSWORD_FILE, 'w').write(cipherMasterPass)
    except:
        mode.password = ''
        mode.myPassword = ''
        return FAIL, ["Could not encrypt password"]
    if mode.auth != ADMIN:
        mode.auth = ADMIN
        mode.pushPrompt(slash, "#", Mode.ENABLE)
    return OK, ["Password set"]


class UnsupportedTypeException(Exception):
    def __init__(self, destObject):
        Exception.__init__(self)
        self.badType = type(destObject)
    def __str__(self):
        return "Cannot support conversion to type %s." % (self.badType)
    def __repr__(self):
        return "Lock file %s has been locked for too long (%s)" % (self.filename, `self.elapsedMin`)

def makeSameType(currentValue, newValue):
    if len(currentValue) == 0:
        return newValue
    if type(currentValue[0]) == type(newValue):
        return newValue
    if type(currentValue[0]) == type(1):
        return int(newValue)
    raise UnsupportedTypeException(currentValue[0])
    
class Set(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "set", "set\tset a configuration value")
        # TOP LEVEL
        self.client = PinshCmd.PinshCmd("client","client\tchange the configuration of one client")
        self.bom = PinshCmd.PinshCmd("bom","bom\tchange a bill of materials")
        self.include = PinshCmd.PinshCmd("include","include\tchange an include file")
        self.password = PinshCmd.PinshCmd("password","password\tset your password")
        self.children = [self.client, self.bom, self.include, self.password]

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
        newValue = makeSameType(currentValue, newValue)
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
        if tokens[1].lower().startswith('p'):
            return setPassword(slash)
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
