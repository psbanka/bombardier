#!/usr/bin/python

# FIXME: This module is in horrible need of refactoring

import sys, os, time, glob, md5, random
import PinshCmd, Mode, libUi, ConfigField, Expression, JobNameField, Integer, MultipleChoice, Variable
import SecureCommSocket
from commonUtil import *
import Client, libCipher
import yaml, syck

def cleanup():
    directories = ["include", "client"]
    for directory in directories:
        fullGlobPath = os.path.join(mode.serverHome, directory, "*.yml.new")
        files = glob.glob(fullGlobPath)
        for fileName in files:
            os.unlink(fileName)

def reEncryptFiles(oldPassword, newPassword):
    print "Re-encrypting files...",
    directories = ["include", "client"]
    renameFiles = []
    for directory in directories:
        fullGlobPath = os.path.join(mode.serverHome, directory, "*.yml")
        files = glob.glob(fullGlobPath)
        for fileName in files:
            baseName = fileName.split(os.path.sep)[-1]
            newFileName = baseName + ".new"
            sys.stdout.write('.')
            sys.stdout.flush()
            try:
                oldData = syck.load(open(fileName).read())
            except syck.error, e:
                errStr = e[0]
                msg  = ["Could not parse file under configuration control"]
                msg += ["File name: %s" % fileName]
                msg += ["Error: %s" % (errStr)]
                cleanup()
                return FAIL, msg
            try:
                newData = libCipher.changePass(oldData, oldPassword, newPassword)
            except libCipher.DecryptionException, de:
                msg  = ["Could not decrypt file under configuration control"]
                msg += ["File name: %s" % fileName]
                msg += ["Error: %s" % (de)]
            except libCipher.InvalidData, id:
                msg  = ["Could not perform encryption for file under configuration control"]
                msg += ["File name: %s" % fileName]
                msg += ["Error: %s" % (id)]
                return FAIL, msg
            newFilePath = os.path.join(mode.serverHome, directory, newFileName)
            fh = open(newFilePath, 'w')
            fh.write(yaml.dump(newData, default_flow_style=False))
            renameFiles.append((fileName, newFilePath))
    overallStatus = OK
    errorFiles = []
    for oldName,newName in renameFiles:
        status1 = os.system("mv %s %s" % (newName, oldName))
        status2 = os.system("chgrp %s %s 2> /dev/null" % (mode.defaultGroup, oldName))
        status3 = os.system("chmod 660 %s 2> /dev/null" % (oldName))
        if status1 != OK or status2 != OK or status3 != OK:
            errorFiles.append(oldName)
            overallStatus = FAIL
    if overallStatus != OK:
        return FAIL, ["Files that failed to be updated: %s" % errorFiles]
    return OK, ["Password updated"]

def setPassword(slash, reEncrypt):
    if libUi.askYesNo("Reset master password", NO) != YES:
        return OK, ["No action taken"]
    if mode.auth == ADMIN or 'password' not in mode.global_config:
        oldPassword = mode.password
        # MASTER PASSWORD HAS NEVER BEEN SET
        masterPass1 = libUi.pwdInput("Enter new master password: ")
        if masterPass1 == '':
            return FAIL, ["Aborted"]
        masterPass2 = libUi.pwdInput("Verify new master password: ")
        if masterPass1 != masterPass2:
            return FAIL, ['Passwords do not match']
    else:
        return FAIL, ["Must be done from enable mode"]
    newPassword = masterPass1

    if mode.auth == ADMIN:
        if reEncrypt:
            status, msg = reEncryptFiles(oldPassword, newPassword) 
            if status != OK:
                return FAIL, msg
    if mode.auth != ADMIN:
        mode.auth = ADMIN
        mode.pushPrompt(slash, "#", Mode.ENABLE)
    mode.password = masterPass1
    salt = ''.join(random.sample(libCipher.VALID_CHARS, 20))
    makeMd5 = md5.new()
    makeMd5.update(salt)
    makeMd5.update(mode.password)
    mode.writeConfig("password", (salt, makeMd5.hexdigest()))
    print
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

def sendMessage(command, messageList, successMessage):
    c = SecureCommSocket.SecureClient(TB_CTRL_PORT, mode.password)
    try:
        c.sendSecure(command, messageList)
        return OK, [successMessage]
    except ConnectionRefusedException:
        return FAIL, ["Job server is not running. Use 'scheduler start' to start it."]

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
    if len(tokens) < 4 and noFlag:
        return delJob(jobName)
    if not tokens[3]:
        return FAIL, ["Command incomplete"]
    if tokens[3].startswith('e'):
        if noFlag:
            return sendMessage(TB_DISABLE, [jobName], "Job %s disabled." % jobName)
        else:
            return sendMessage(TB_ENABLE, [jobName], "Job %s enabled." % jobName)

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
    return sendMessage(TB_ADD, [bomshCmd, freq * multiplier, jobName, mode.username], "Job submitted.")
    
def setLock(noFlag, lockName):
    lockPath = "%s/%s" % (mode.global_config["tmpPath"], lockName)
    if noFlag:
        setTime = 0
        if not os.path.isfile(lockPath):
            return FAIL, ["Lock was not set."]
        try:
            setTime = float(open(lockPath).read().strip())
        except ValueError:
            pass
        try:
            os.unlink(lockPath)
        except OSError:
            return FAIL, ["Unable to remove lock file %s" % lockPath]
        if setTime:
            return OK, ["Lock %s removed (%4.1f seconds old)" % (lockName, time.time()-setTime)]
        return OK, ["Lock %s removed." % lockName]

    if os.path.isfile(lockPath):
        setTime = 0
        try:
            setTime = float(open(lockPath).read().strip())
            return FAIL, ["Lock %s is already set (%4.1f seconds old)" % (lockName, time.time()-setTime)]
        except ValueError:
            return FAIL, ["Lock %s is already set." % lockName]
    try:
        open(lockPath, 'w').write(str(time.time()))
    except IOError, e:
        return FAIL, ["Lock file %s cannot be set (%s)" % (lockName, str(e))]
    return OK, ["Lock %s has been set." % lockName]

class Set(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "set", "set\tset a configuration value")
        # TOP LEVEL
        self.client = PinshCmd.PinshCmd("client","client\tchange the configuration of one client")
        self.bom = PinshCmd.PinshCmd("bom","bom\tchange a bill of materials")
        self.include = PinshCmd.PinshCmd("include","include\tchange an include file")
        self.password = PinshCmd.PinshCmd("password","password\tset your password")
        withoutReEncryption= PinshCmd.PinshCmd("without-reencryption","without-reencryption\tDon't re-encrypt data files")
        self.password.children = [withoutReEncryption]
        self.job = PinshCmd.PinshCmd("job", "job\tset up a new batch job")
        self.job.auth = ADMIN
        self.lock = PinshCmd.PinshCmd("lock", "lock\tset a lock")
        self.children = [self.client, self.bom, self.include, self.password, self.job, self.lock]

        expression = Expression.Expression()
        encrypt = PinshCmd.PinshCmd("encrypt","encrypt\tencrypt the confiuration value")
        encrypt.children = [expression]

        # CLIENT
        self.clientConfigField = ConfigField.ConfigField(dataType=ConfigField.CLIENT)
        self.client.children = [self.clientConfigField]
        self.clientConfigField.children = [expression, encrypt]

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
        enabled = PinshCmd.PinshCmd("enabled","enabled\tenable a job")
        freq = Integer.Integer(min = 1, max = 1000, name = "number of minutes, hours, or days between each time the job is run")
        timespan = MultipleChoice.MultipleChoice(choices = ["seconds", "minute", "hour", "day", "week"], 
        helpText = ["measure by seconds", "measure by minutes (default)", "measure by hours", "measure by days", "measure by weeks"])
        self.job.children = [self.jobNameField]
        self.jobNameField.children = [freq, enabled]
        freq.children = [timespan]

        # LOCK
        self.lock.children = [Variable.Variable()]

        self.level = 0
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

    def getNewValue(self, tokens, encrypt, default=''):
        if type(default) != type("string"):
            default = ''
        if encrypt:
            newValue = libUi.pwdInput("Enter value to encrypt: ")
        elif len(tokens) < 4:
            newValue = libUi.getDefault("Enter value", default)
        else:
            newValue = ' '.join(tokens[3:])
        return newValue

    def modifyList(self, tokens, currentValue, newValue, fieldObject, encrypt):
        newValue = makeSameType(currentValue, newValue)
        currentValue.append(newValue)
        try:
            status, output = fieldObject.setValue(tokens, 2, currentValue, encrypt)
            return status, output + ["%s appended to list" % newValue]
        except:
            return FAIL, ["Unable to set. Check your configuration path."]

    def modifyScalar(self, tokens, newValue, fieldObject, encrypt):
        status, output = fieldObject.setValue(tokens, 2, newValue, encrypt)
        return status, output + ["Value set."]

    def setConfigValue(self, tokens, noFlag):
        fieldObject = self.getFieldObject(tokens)
        if noFlag:
            currentValue = fieldObject.getSpecificData(tokens, 2)
            if type(currentValue) == type(['list']):
                item = tokens[3]
                if item in currentValue:
                    currentValue.remove(item)
                    status, output = fieldObject.setValue(tokens, 2, currentValue, False)
                    return status, output + ["%s removed from list" % item]
                try:
                    if int(item) in currentValue:
                        currentValue.remove(int(item))
                        status, output = self.setValue(tokens, 2, currentValue, False)
                        return status, output + ["%s removed from list" % item]
                except:
                    pass
                return FAIL, ["%s is not in the current list of values." % item]
            return fieldObject.removeValue(tokens, 2)
        encrypt = False
        if len(tokens) == 4 and tokens[-1].lower() == 'encrypt':
            encrypt = True
        if not fieldObject:
            return FAIL, ["Unknown configuration item: %s" % tokens[2]]
        currentValue = fieldObject.getSpecificData(tokens, 2)
        newValue = self.getNewValue(tokens, encrypt, currentValue)
        if type(currentValue) == type(['list']):
            return self.modifyList(tokens, currentValue, newValue, fieldObject, encrypt)
        elif type(currentValue) in [type('string'), type(1), type(True)]:
            if currentValue == newValue and not encrypt:
                return FAIL, ["New value and old value are identical."]
            return self.modifyScalar(tokens, newValue, fieldObject, encrypt)

    def cmd(self, tokens, noFlag, slash):
        if tokens[1].lower().startswith('l'):
            if len(tokens) < 2:
                return FAIL, ["Incomplete command; must provide lock name"]
            lockName = tokens[2]
            return setLock(noFlag, lockName)
        if tokens[1].lower().startswith('j'):
            return setJob(tokens, noFlag)
        if tokens[1].lower().startswith('p'):
            if len(tokens) == 3 and tokens[2].lower().startswith('w'):
                return setPassword(slash, reEncrypt=False)
            return setPassword(slash, reEncrypt=True)
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        return self.setConfigValue(tokens, noFlag)


class Lock(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "lock")
        self.helpText = "lock\tset a lock"
        #self.children = [FileNameField.FileNameField(mode.global_config["tmpPath"], crusty=False)]
        self.children = [Variable.Variable()]
        self.level = 0
        self.cmdOwner = 1

