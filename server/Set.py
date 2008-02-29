#!/usr/bin/python

import sys

import PinshCmd, Mode, libUi, ConfigField, Expression, JobNameField, Integer, MultipleChoice
import SecureCommSocket
from commonUtil import *

import Client, libCipher
def setPassword(slash):
    if not 'password' in mode.config: 
        masterPass = libUi.pwdInput("Enter master password to authorize this user: ")
        client = Client.Client("test", masterPass, mode.dataPath)
        client.downloadClient()
        try:
            client.decryptConfig()
        except:
            return FAIL, ['Incorrect master password']
        mode.password = masterPass
        print "% Password correct, continue"
    else:
        if mode.auth != ADMIN:
            return FAIL, ["Must be done from enable mode"]
    testPass1 = libUi.pwdInput("new password: ")
    if testPass1 == '':
        return FAIL, ["Aborted"]
    testPass2 = libUi.pwdInput("new password (confirm): ")
    if testPass1 != testPass2:
        return FAIL, ["Passwords do not match"]
    mode.myPassword = testPass1
    try:
        cipherMasterPass = libCipher.encrypt(mode.password, mode.myPassword)
        mode.writeConfig("password", cipherMasterPass)
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
        return "Cannot support conversion to type %s." % (self.badType)

def makeSameType(currentValue, newValue):
    if len(currentValue) == 0:
        return newValue
    if type(currentValue[0]) == type(newValue):
        return newValue
    if type(currentValue[0]) == type(1):
        return int(newValue)
    raise UnsupportedTypeException(currentValue[0])

def delJob(jobName):
    c = SecureCommSocket.SecureClient(TB_CTRL_PORT, mode.password)
    try:
        print "  Removing job..."
        jobDictStart = c.sendSecure(TB_SHOW, [])
    except ConnectionRefusedException:
        return FAIL, ["Job server is not running. Use 'scheduler start' to start it."]
    if jobName in jobDictStart[0]:
        c.sendSecure(TB_DEL, [jobName])
    else:
        return FAIL, ["Job %s does not exist in the job list" % jobName]
    jobDictEnd = c.sendSecure(TB_SHOW, [])
    if jobName in jobDictEnd[0]:
        return FAIL, ["Could not remove job %s" % jobName]
    return OK, ["Job removed"]

def setJob(tokens, noFlag):
    if not tokens[2]:
        return FAIL, ["Need a name for the job"]
    jobName = tokens[2]
    if noFlag:
        return delJob(jobName)
    if not tokens[3]:
        return FAIL, ["Need a frequency with which to run the job"]
    freq = int(tokens[3])
    multiplier = 60
    if len(tokens) > 4:
        if tokens[4].startswith('h'):
            multiplier = multiplier * 60
        elif tokens[4].startswith('s'):
            multiplier = 1
        elif tokens[4].startswith('d'):
            multiplier = multiplier * 60 * 24
        elif tokens[4].startswith('w'):
            multiplier = multiplier * 60 * 24 * 7
    bomshCmd = raw_input("Enter the command to schedule> ")
    if bomshCmd == '':
        return FAIL, ["Abort"]
    c = SecureCommSocket.SecureClient(TB_CTRL_PORT, mode.password)
    try:
        c.sendSecure(TB_ADD, [bomshCmd, freq * multiplier, jobName, mode.username])
    except ConnectionRefusedException:
        return FAIL, ["Job server is not running. Use 'scheduler start' to start it."]
    return OK, ["Job submitted"]
    
    
class Set(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "set", "set\tset a configuration value")
        # TOP LEVEL
        self.client = PinshCmd.PinshCmd("client","client\tchange the configuration of one client")
        self.bom = PinshCmd.PinshCmd("bom","bom\tchange a bill of materials")
        self.include = PinshCmd.PinshCmd("include","include\tchange an include file")
        self.password = PinshCmd.PinshCmd("password","password\tset your password")
        self.job = PinshCmd.PinshCmd("job", "job\nset up a new batch job")
        self.job.auth = ADMIN
        self.children = [self.client, self.bom, self.include, self.password, self.job]

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

        # JOB
        self.jobNameField = JobNameField.JobNameField()
        freq = Integer.Integer(min = 1, max = 1000, name = "number of minutes, hours, or days between each time the job is run")
        timespan = MultipleChoice.MultipleChoice(choices = ["seconds", "minute", "hour", "day", "week"], 
        helpText = ["measure by seconds", "measure by minutes (default)", "measure by hours", "measure by days", "measure by weeks"])
        
        self.job.children = [self.jobNameField]
        self.jobNameField.children = [freq]
        freq.children = [timespan]

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
                status, output = fieldObject.setValue(tokens, 2, currentValue)
                return status, output + ["%s removed from list" % newValue]
            try:
                if int(newValue) in currentValue:
                    currentValue.remove(int(newValue))
                    status, output = fieldObject.setValue(tokens, 2, currentValue)
                    return status, output + ["%s removed from list" % newValue]
            except:
                pass
            return FAIL, ["%s is not in the current list of values." % newValue]
        newValue = makeSameType(currentValue, newValue)
        currentValue.append(newValue)
        try:
            status, output = fieldObject.setValue(tokens, 2, currentValue)
            return status, output + ["%s appended to list" % newValue]
        except:
            return FAIL, ["Unable to set. Check your configuration path."]

    def modifyScalar(self, noFlag, tokens, newValue, fieldObject):
        if noFlag:
            fieldObject.setValue(tokens, 2, '')
            return OK, ["Value removed."]
        status, output = fieldObject.setValue(tokens, 2, newValue)
        return status, output + ["Value set."]

    def cmd(self, tokens, noFlag, slash):
        if tokens[1].lower().startswith('j'):
            return setJob(tokens, noFlag)
        if tokens[1].lower().startswith('p'):
            return setPassword(slash)
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        fieldObject = self.getFieldObject(tokens)
        if not fieldObject:
            return FAIL, ["Unknown command: %s" % tokens[1]]
        currentValue = fieldObject.getSpecificData(tokens, 2)
        newValue = self.getNewValue(noFlag, tokens)
        if type(currentValue) == type(['list']):
            return self.modifyList(noFlag, tokens, currentValue, newValue, fieldObject)
        elif type(currentValue) in [type('string'), type(1), type(True)]:
            if currentValue == newValue:
                return FAIL, ["New value and old value are identical."] 
            return self.modifyScalar(noFlag, tokens, newValue, fieldObject)
