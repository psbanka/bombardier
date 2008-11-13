#!/usr/bin/env python

import sys, re, os, getpass, base64, time
import yaml
import StringIO
import copy
import traceback
from RemoteClient import RemoteClient, ClientUnavailableException
from Client import ClientConfigurationException
from bombardier.miniUtility import stripVersion
from staticData import *
try:
    import syck
except:
    import yaml as syck


TMP_FILE = "tmp.yml"
DOT_LENGTH = 20
PROGRESS = "install-progress"
LOCAL_PACKAGES = "local-packages"

class BombardierRemoteClient(RemoteClient):

    def __init__(self, hostName, configPass, serverHome, termwidth, termcolor,
                 outputHandle=sys.stdout, packageData=None, enabled=True):
        RemoteClient.__init__(self, hostName, serverHome, termwidth, termcolor,
                              outputHandle, configPass, enabled)
        self.statusData    = {}
        self.localFilename = ''
        self.reportInfo    = ''
        self.stateMachine  = []

        self.stateMachine.append([re.compile("\=\=REPORT\=\=:(.*)"), self.getReport])
        self.stateMachine.append([re.compile("\=\=REQUEST-CONFIG\=\="), self.sendClient])
        self.stateMachine.append([re.compile("\=\=REQUEST-PKGINFO\=\=:(.+)"), self.sendPkgInfo])
        self.stateMachine.append([re.compile("\=\=REQUEST-BOM\=\=:(.+)"), self.sendBom])
        self.stateMachine.append([re.compile("\=\=REQUEST-PACKAGE\=\=:(.+):(.+)"), self.oldSendPackage])
        self.stateMachine.append([re.compile("Unable to shred before deleting"), self.noReport])
        self.stateMachine.append([re.compile("Uninstalling package \((\S+)\)"), self.unInstall])
        self.stateMachine.append([re.compile("Beginning installation of \((\S+)\)"), self.install])
        self.stateMachine.append([re.compile("(\S+) result for (\S+) : (\d)"), self.actionResult])
        self.logMatcher = re.compile( "\d+\-\d+\-\d+\s\d+\:\d+\:\d+\,\d+\|([A-Z]+)\|(.+)\|" )
        self.traceMatcher = re.compile( "\|\|\>\>\>(.+)" )
        if self.platform == 'win32':
            self.python  = '/cygdrive/c/Python25/python.exe'
            self.spkgDir = '/cygdrive/c/spkg'
        else:
            self.python  = '/usr/bin/python'
            self.spkgDir = '/opt/spkg'
        if self.info.get("pythonPath"):
            self.python = self.info.get("pythonPath")
        if self.info.get("spkgPath"):
            self.spkgDir = self.info.get("spkgPath")
        self.cmdDebug = None
        self.packageData = {}
        self.pullReport = True

    def setConfigPass(self, configPass):
        self.configPass = configPass

    def freshen(self):
        statusFile = os.path.join(self.serverHome, "status", "%s.yml" % self.hostName)
        self.statusData = ''
        if os.path.isfile(statusFile):
            try:
                self.statusData = syck.load(open(statusFile).read())
            except Exception, e:
                if e[0] == "syntax error":
                    msg = "Syntax error in status information for %s" % self.hostName
                    self.errorOutput(msg)
                    self.statusData = ''
            if type(self.statusData) != type({}) \
                or PROGRESS not in self.statusData \
                or LOCAL_PACKAGES not in self.statusData:
                    msg = "Invalid status data. Ignoring."
                    self.warningOutput(msg)
        self.packageData = syck.load(open(os.path.join(self.serverHome, PACKAGES_FILE)).read())
        return(RemoteClient.freshen(self))

    def actionResult(self, data):
        action, packageName, result = data
        if self.termcolor != NO_COLOR:
            if int(result) == OK:
                colorCode = GOOD_COLOR[self.termcolor]
            else:
                colorCode = WARNING_COLOR[self.termcolor]
            message = "\033%s%s %s: %s\033[m" % (colorCode, action.lower(), RETURN_DICT[int(result)], packageName)
        else:
            message = "%s %s: %s" % (action.lower(), RETURN_DICT[int(result)], packageName)
        self.fromOutput(message)

    def actionStart(self, action, packageName):
        if self.termcolor != NO_COLOR:
            colorCode = STRONG_COLOR[self.termcolor]
            message = "\033%s%s %s %s\033[m" %(colorCode, self.hostName, action, packageName)
        else:
            message = "%s installing %s" %(self.hostName, packageName)
        self.fromOutput(message)

    def install(self, packageName):
        self.actionStart("installing", packageName)

    def unInstall(self, packageName):
        self.actionStart("uninstalling", packageName)

    def noReport(self, data):
        self.pullReport = False

    def sendPackage(self, packageName, destPath):
        filename = os.path.join(self.serverHome, "packages", packageName)
        if not os.path.isfile(filename):
            message = "Client requested a file that is not on this server: %s" % filename
            self.errorOutput(message)
            return OK
        self.scp(filename, destPath, False)

    def oldSendPackage(self, data):
        package, path = data
        filename = os.path.join(self.serverHome, "packages", package)
        self.debugOutput("Sending %s..." % package,"")
        if not os.path.isfile(filename):
            message = "Client requested a file that is not on this server: %s" % filename
            self.errorOutput(message, message)
            self.s.send(`FAIL`)
        self.scp(filename, path)
        self.s.send("OK\n")

    def streamFile(self, filename):
        plainText = open(filename, 'rb').read()
        return self.streamData(plainText)

    def streamData(self, plainText):
        import zlib
        compressed = zlib.compress(plainText)
        encoded    = base64.encodestring(compressed)
        self.s.setecho(False)
        handle = StringIO.StringIO(encoded)
        BLK_SIZE = 77
        lines = 0
        totalLines = len(encoded.split()) / BLK_SIZE
        printFrequency = totalLines / DOT_LENGTH
        if self.debug:
            self.outputHandle.write("==> Sending configuration information:")
            self.outputHandle.flush()
        if printFrequency < 1:
            printFrequency = 1
        while True:
            chunk = handle.read(BLK_SIZE)
            lines += 1
            if chunk == '':
                chunk = ' '*(BLK_SIZE-1)+'\n'
                self.s.send(chunk)
                break
            if len(chunk) < BLK_SIZE:
                pad = ' '*(BLK_SIZE-len(chunk))
                chunk = chunk[:-1] + pad + '\n'
            if lines % printFrequency == 0:
                self.outputHandle.write('.')
                self.outputHandle.flush()
            self.s.send(chunk)
        if self.debug:
            self.outputHandle.write('\n')
            self.outputHandle.flush()

    def sendClient(self, data):
        if data:
            pass
        tmpFilePath = self.serverHome+"/"+TMP_FILE
        open(tmpFilePath, 'w').write(yaml.dump( self.info ))
        self.streamFile(tmpFilePath)
        if os.path.isfile(tmpFilePath):
            os.unlink(tmpFilePath)

    def getReport(self, yamlLine):
        self.reportInfo += yamlLine + "\n"

    def sendPkgInfo(self, packageName):
        thisPackageData = self.packageData.get(packageName) 
        if not thisPackageData:
            message = "Could not find package data for %s." % packageName
            self.errorOutput(message)
            raise ClientConfigurationException(self.hostName)
        self.streamData(yaml.dump(thisPackageData))

    def sendBom(self, data):
        filename = os.path.join(self.serverHome, "bom", "%s.yml" % data)
        if not os.path.isfile(filename):
            message = "Could not find valid bom data for this %s. Exiting." % filename
            self.errorOutput(message)
            raise ClientConfigurationException(self.hostName)
        self.streamFile(filename)

    def processMessage(self, message):
        for state in self.stateMachine:
            match, function = state
            grepInfo = match.findall(message)
            if grepInfo:
                if function:
                    function(grepInfo[0])
                    return True
        return False

    def getReturnCode(self):
        return OK
        self.s.sendline("echo $?")
        self.s.prompt()
        returnCodeStr = str(self.s.before.split()[0].strip())
        try:
            returnCode = int(returnCodeStr)
            return returnCode
        except Exception, e:
            self.debugOutput(str(e))
            self.errorOutput("Command returned non-numeric exit code: ('%s')" % self.s.before)
        return FAIL

    def gso(self, cmd, raiseOnError=True):
        if self.cmdDebug:
            self.debugOutput("* RUNNING: %s" % cmd)
        try:
            self.s.sendline( cmd )
            self.s.prompt()
        except pexpect.EOF:
            if raiseOnError:
                msg = "Error running %s (%s)" % (cmd, output)
                raise ClientUnavailableException(msg)
            else:
                return ""
        output = self.s.before.strip()
        if self.cmdDebug:
            self.debugOutput("* OUTPUT: %s" % output)
        status = self.getReturnCode()
        if status != OK and raiseOnError:
            msg = "Error running %s (%s)" % (cmd, output)
            raise ClientConfigurationException(msg)
        return output

    def runCmd(self, commandString):
        self.reportInfo = ''
        if self.freshen() != OK:
            message = "Unable to connect to %s." % self.hostName
            self.errorOutput(message)
            return FAIL, ''
        returnCode = OK
        self.s.sendline ('cd %s' %self.spkgDir)
        self.s.prompt()
        self.s.sendline(commandString)
        self.s.prompt()
        output = self.s.before
        self.s.setecho(False)
        returnCode = self.getReturnCode()
        return returnCode, output

    def dumpTrace(self):
        traceback = []
        foundIndex = 1
        while foundIndex == 1:
            traceback.append(self.s.match.groups()[0])
            foundIndex = self.s.expect([self.s.PROMPT, self.traceMatcher, self.logMatcher], timeout=6000)
        tString = ''.join(traceback)

        data = re.compile("NoOptionError\: No option \'(\w+)\' in section\: \'(\w+)\'").findall(tString)
        if data:
            message1 = "Invalid client configuration data"
            self.errorOutput(message1)
            if len(data) == 2:
                message2 = "Need option '%s' in section '%s'." % (data[0], data[1])
            else:
                message2 = "Need options: %s" % data
            self.debugOutput(message2, message2)
        data = re.compile("NoSectionError\: No section\: \'(\w+)\'").findall(tString)
        if data:
            message1 = "Invalid client configuration data"
            self.errorOutput(message1)
            message2 = "Need section '%s'." % (data[0])
            self.debugOutput(message2, message2)
        else:
            for line in traceback:
                message = "\033[mCLIENT TRACEBACK: %s" % line
                self.debugOutput(message, message)

    def getPackageNamesFromProgress(self):
        #CANNIBALIZED FROM PackageField.py
        statusYml = os.path.join(self.serverHome, "status", "%s.yml" % self.hostName)
        if not os.path.isfile(statusYml):
            self.debugOutput("Cannot retrieve status (NO FILE: %s)" % statusYml)
            return []
        yml = syck.load( open(statusYml).read() ) 
        if yml == None:
            self.debugOutput("Cannot retrieve status (EMPTY FILE: %s)" % statusYml)
            return []
        return yml

    def getAllPackageNames(self):
        packageNames = set([stripVersion(x) for x in self.getPackageNamesFromProgress().get(PROGRESS, {})])
        packageNames = packageNames.union(set(self.info.get("packages")))
        return list(packageNames)

    def sendAllClientData(self, action):
        sendData = {"configData": self.info, "packageData": {}}
        if action != PURGE:
            packageNames = self.getAllPackageNames()
            for packageName in packageNames:
                thisPackageData = self.packageData.get(packageName) 
                if not thisPackageData:
                    message = "Could not find package data for %s." % packageName
                    self.errorOutput(message)
                    raise ClientConfigurationException(self.hostName)
                sendData["packageData"][packageName] = thisPackageData
        self.streamData(yaml.dump(sendData))

    def getNewestExistingPackages(self):
        output = []
        existingPackages = self.getPackageNamesFromProgress().get(LOCAL_PACKAGES, [])
        basePackageNames = [stripVersion(x) for x in existingPackages]
        basePackageNames = list(set(basePackageNames)) # eliminates dups
        for basePackageName in basePackageNames:
            packageVersions = []
            for ep in existingPackages:
                if ep.startswith(basePackageName):
                    suffix = ep.split(basePackageName)[-1]
                    matches = re.compile('^\-(\d+)').findall(suffix)
                    if len(matches) != 1:
                        #self.errorOutput("Invalid existing package name: %s" % ep)
                        continue
                    packageVersions.append(int(matches[0]))
            packageVersions.sort()
            if packageVersions:
                newestExisting = "%s-%d" % (basePackageName, packageVersions[-1])
                output.append(newestExisting)
        return output

    def uploadNewPackages(self):
        newPackages = copy.deepcopy(self.info.get("packages"))
        newestExistingPackages = self.getNewestExistingPackages()
        destPath = os.path.join(self.spkgDir, self.hostName, "packages")
        for fullPackageName in newestExistingPackages:
            basePackageName = stripVersion(fullPackageName)
            newestPackageData = self.packageData.get(basePackageName, {})
            newestPackageName = newestPackageData.get("install", {}).get("fullName")
            if newestPackageName and newestPackageName != fullPackageName:
                self.debugOutput('', "Need to send package: %s" % newestPackageName)
                self.sendPackage(newestPackageName+".spkg", destPath)
            if basePackageName in newPackages:
                newPackages.remove(basePackageName)
        for basePackageName in newPackages:
            newestPackageData = self.packageData.get(basePackageName, {})
            newestPackageName = newestPackageData.get("install", {}).get("fullName")
            self.sendPackage(newestPackageName+".spkg", destPath)

    def runBc(self, action, packageNames, scriptName, debug):
        self.s.sendline ('cd %s' %self.spkgDir)
        self.s.prompt()
        packageString = ' '.join(packageNames)
        if self.platform == "win32":
            cmd = "cat /proc/registry/HKEY_LOCAL_MACHINE/SOFTWARE/Python/PythonCore/2.5/InstallPath/@"
            pythonHomeWin = self.gso(cmd)
            pythonHomeCyg = self.gso("cygpath $(%s)" %cmd)
            self.getStatusYml()
            cmd = "%spython.exe '%sScripts\\bc.py' %s %s %s %s" % (pythonHomeCyg, pythonHomeWin,
                  ACTION_DICT[action], self.hostName, packageString, scriptName)
        else:
            cmd = "export PYTHON_HOME=$(%s -c 'import sys; print sys.prefix')" %self.python
            gsoOut = self.gso(cmd)
            cmd = '$PYTHON_HOME/bin/python $PYTHON_HOME/bin/bc.py %s %s %s %s' % (ACTION_DICT[action], self.hostName,
                                         packageString, scriptName)
        self.s.sendline(cmd)
        self.sendAllClientData(action)
        foundIndex = 0
        while True:
            foundIndex = self.s.expect([self.s.PROMPT, self.traceMatcher, self.logMatcher], timeout=6000)
            if foundIndex == 1: # Stack trace
                self.dumpTrace()
                self.s.prompt()
                self.getStatusYml()
                return FAIL, ["Client raised an exception."]
            elif foundIndex == 0: # BC exited
                if self.s.before.strip():
                    self.debugOutput("Remaining output: %s" % self.s.before.strip())
                self.s.setecho(False)
                self.s.sendline("echo $?")
                self.s.prompt()
                try:
                    returnCode = int(str(self.s.before.split()[0].strip()))
                except Exception, e:
                    self.debugOutput( str(e) )
                    self.errorOutput( "Invalid returncode: ('%s')" % self.s.before)
                    returnCode = FAIL
                break
            elif foundIndex == 2: # Log message
                messageType, message = self.s.match.groups()
                if not self.processMessage(message):
                    message=message.strip()
                    if DEBUG or messageType != "DEBUG":
                        if self.termcolor != NO_COLOR and messageType in ["WARNING", "ERROR", "CRITICAL"]:
                            colorCode = WARNING_COLOR[self.termcolor]
                            message = "\033%s%s\033[m" % (colorCode, message)
                        self.fromOutput(message)

    def process(self, action, packageNames, scriptName, debug):
        self.reportInfo = ''
        self.debug = debug
        self.pullReport = True
        self.debugOutput("", "Progress: ")
        if action == EXECUTE:
            self.clearScriptOutput(scriptName)
        if self.freshen() != OK:
            if not self.debug:
                self.outputHandle.write('\n')
            return FAIL, ["UNABLE TO CONNECT TO %s. No actions are available." % self.hostName]
        returnCode = OK
        try:
            if action != INIT:
                self.uploadNewPackages()
            self.runBc(action, packageNames, scriptName, debug)
        except KeyboardInterrupt:
            self.debugOutput("Cleaning up...", "\ncleaning up...")
            if self.terminate() == OK:
                self.debugOutput("Disconnected", "\ndisconnected")
            else:
                self.errorOutput("Could not disconnect.")
            raise KeyboardInterrupt
        except ClientUnavailableException:
            return FAIL, ["Remote system refused connection"]
        except ClientConfigurationException:
            return FAIL, []
        except Exception, e:
            e = StringIO.StringIO()
            traceback.print_exc(file=e)
            e.seek(0)
            data = e.read()
            ermsg = ''
            if not self.debug:
                self.outputHandle.write('\n')
            for line in data.split('\n'):
                ermsg = "%% %s" % line
                self.debugOutput(ermsg, ermsg)
            return FAIL, ["Exception in client-handling code."]

        self.getStatusYml()
        if not self.debug:
            self.outputHandle.write('\n')
        if action == EXECUTE:
            if self.reportInfo:
                fileName = os.path.join(self.serverHome, "output", "%s-%s.yml" % (self.hostName, scriptName))
                open(fileName, 'w').write(self.reportInfo)
            else:
                if not self.pullReport:
                    return returnCode, []
                self.getScriptOutput(scriptName)

            try:
                reportData = yaml.load(self.reportInfo)
            except:
                return FAIL, ["Bad output from script:", self.reportInfo]
            return returnCode, reportData
        return returnCode, []

    def clearScriptOutput(self, scriptName):
        fileName = self.serverHome+"/output/%s-%s.yml" % (self.hostName, scriptName)
        if os.path.isfile(fileName):
            os.unlink(fileName)
        return

    def getScriptOutput(self, scriptName):
        remoteFilename = "%s-output.yml" % (scriptName)
        self.localFilename  = "%s-%s.yml" % (self.hostName, scriptName)
        self.get("%s/output/%s" % (self.spkgDir, remoteFilename))
        if os.path.isfile(remoteFilename):
            os.system("mv -f %s %s/output/%s" % (remoteFilename, self.serverHome, self.localFilename) )
            self.reportInfo = open(self.serverHome+"/output/%s" % self.localFilename).read()
        return

    def getStatusYml(self):
        statusDir = os.path.join(self.serverHome, 'status')
        if not os.path.isdir( statusDir ):
            os.makedirs( statusDir )

        self.s.sendline ('cat %s/%s/status.yml' % (self.spkgDir, self.hostName))
        self.s.prompt()
        statusYml = str(self.s.before).split("status:")[0]
        statusYml = statusYml.replace('\r','')
        try:
            syck.load(statusYml)
        except:
            self.errorOutput("status.yml could not be parsed (writing to error.yml)")
            open( os.path.join(statusDir, "error.yml"), 'w' ).write(statusYml)
            return
        open( os.path.join(statusDir, "%s.yml" % self.hostName), 'w' ).write(statusYml)

