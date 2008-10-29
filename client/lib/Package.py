#!/cygdrive/c/Python24/python.exe

# Package.py: This guy is responsible for most of the actual work that
# gets done in Bombardier. It's responsible for getting, extracting,
# installing, verifying, etc. packages from the repository onto the
# local system.

# Copyright (C) 2005 Peter Banka

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

import os, time, datetime, glob, random
import sys, StringIO, traceback

import Spkg
import miniUtility, MetaData, Logger
from Exceptions import BadPackage, FeatureRemovedException, RebootRequiredException
from staticData import *
from threading import Thread

class JobThread(Thread):
    def __init__(self, importString, cmd, config):
        Thread.__init__(self)
        self.importString = importString
        self.cmd = cmd
        self.config = config
        self.cmdStatus = None

    def run(self):
        Logger.info("Running %s..." % self.cmd)
        try:
            exec(self.importString)
            exec("self.cmdStatus = %s" % self.cmd)
        except StandardError, e:
            Logger.error( "Failed to run %s" % self.cmd )
            e = StringIO.StringIO()
            traceback.print_exc(file=e)
            e.seek(0)
            data = e.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            Logger.error(ermsg)
            self.cmdStatus = FAIL

class Job:
    def __init__(self, importString, cmd, config):
        self.importString = importString
        self.cmd = cmd
        self.config = config
        self.startTime = None
        self.jt = None
        self.jobStatus = None

    def isRunning(self):
        if self.jt:
            if self.jt.isAlive():
                return True
            Logger.info("-- status: %s" % (self.jt.cmdStatus))
            self.jobStatus = self.jt.cmdStatus
        return False

    def execute(self):
        self.startTime = time.time()
        status = FAIL
        if self.isRunning():
            Logger.error("Refusing to run %s; it is already running" % self.name)
            return FAIL
        self.jt = JobThread(self.importString, self.cmd, self.config)
        self.jt.start()
        Logger.info("Started job...")
        counter = 1
        while self.isRunning():
            time.sleep(1)
            counter += 1
            if not counter % 100:
                Logger.info("Waiting for completion (%s)..." % time.strftime(time.ctime()))
                counter = 1
        status = self.jt.cmdStatus
        return status

    def killThread(self, timeout=5):
        if not self.isRunning():
            return OK
        self.jt.join(timeout)
        return OK

class Package:

    """This class provides an abstraction for a downloadable,
    installable, verifiable, and uninstallable thing. Each
    package is held in the repository"""

    ### TESTED
    def __init__(self, name, repository, config, filesystem, operatingSystem, instanceName):
        self.name         = name
        self.repository   = repository
        self.filesystem   = filesystem
        self.operatingSystem      = operatingSystem
        self.instanceName = instanceName
        self.action       = INSTALL
        self.installed    = False
        self.dependencies = []
        self.dependsOnMe  = []
        self.console      = False
        self.fullName     = ''
        self.reboot       = False
        self.packageVersion = 0
        self.checksum     = ''
        self.config       = config
        self.status       = OK
        self.priority     = AVERAGE
        self.metaData     = MetaData.MetaData({})
        self.workingDir   = ''
        self.scriptsDir   = ''
        self.maintDir     = ''
        self.downloaded   = False
        self.dependencyErrors = []

    def invalidate(self):
        erstr = "INVALID PACKAGE: %s" % self.name
        Logger.error(erstr)
        self.status = FAIL
        
    def getConfiguration(self):
        return self.metaData.data.get("configuration")

    def gatherDependencies(self):
        self.dependencies = self.metaData.data.get("dependencies")
        if type(self.dependencies) == type(None):
            self.dependencies = []
        elif type(self.dependencies) == type('string'):
            self.dependencies = [self.dependencies]
        elif type(self.dependencies) == type([]):
            pass
        else:
            errmsg = "Unsupported dependencies data for %s. "\
                     "Must be a list. [%s] instead." % (self.name, self.dependencies)
            Logger.warning(errmsg)
            self.dependencies = []
    
    def addDependencyError(self, dependencyName):
        errmsg = "BOM file is incomplete: should contain %s" % dependencyName
        Logger.warning(errmsg)
        self.dependencyErrors.append(dependencyName)

    def evalPriority(self):
        self.priority = self.metaData.data["install"].get('priority')
        if not self.priority:
            ermsg = "Package %s does not have a priority "\
                    "(assuming %s)" % (self.name, AVERAGE)
            Logger.warning(ermsg)
            self.priority = AVERAGE
        else:
            try:
                self.priority = int(self.priority)
            except ValueError:
                ermsg = "Package %s has an invalid priority value"\
                        "(assuming %s)" % (self.name, AVERAGE)
                Logger.warning(ermsg)
                self.priority = AVERAGE

    def checkMetaData(self):
        self.status = FAIL
        if self.metaData.data:
            if type(self.metaData.data) == type(dict()):
                if self.metaData.data.get("install"):
                    if type(self.metaData.data["install"]) == type({}):
                        self.fullName = self.metaData.data['install'].get('fullName')
                        if self.fullName:
                            self.status = OK
                            return
                        else:
                            msg = "Package does not exist on the server"
                            raise BadPackage, (self.name, msg)
                    else:
                        msg = "The server's record is completely corrupt"
                        raise BadPackage, (self.name, msg)
                else:
                    msg = "server record is corrupt: no 'install' section"
                    raise BadPackage, (self.name, msg)
            else:
                msg = "server record is corrupt: not a dictionary"
                raise BadPackage, (self.name, msg)
        else:
            msg = "No metadata found for this package"
            raise BadPackage, (self.name, msg)

    def initialize(self):
        self.metaData = self.repository.getMetaData(self.name)
        self.checkMetaData()
        self.checksum = self.metaData.data['install'].get('md5sum')
        if not self.checksum:
            ermsg = "Package %s does not have a checksum field"\
                    " (not checking)" % (self.name)
            Logger.warning(ermsg)
        if self.metaData.data['install'].get('md5list'):
            self.checksum = self.metaData.data['install']['md5list']
        chk = self.metaData.data["install"].get('console')
        self.console = miniUtility.evalBoolean(chk)
        chk = self.metaData.data["install"].get('reboot')
        self.reboot = miniUtility.evalBoolean(chk)
        chk = self.metaData.data.get("package-version")
        if type(chk) == type(1):
            self.packageVersion = chk
        self.evalPriority()
        self.gatherDependencies()


    def initializeFromFilesystem(self):
        """ Expects a standard package to be extracted in the packages
        directory """
        packagePath = miniUtility.getPackagePath(self.instanceName)
        if not self.fullName:
            raise BadPackage(self.name, "Could not find full name.")
        newDir = os.path.join(packagePath, self.fullName)
        self.scriptsDir = os.path.join(newDir, "scripts")
        if self.packageVersion > 3:
            self.maintDir = os.path.join(newDir, "maint")
        else:
            self.maintDir = os.path.join(self.scriptsDir, "maint")
        if not self.filesystem.isdir(self.scriptsDir):
            errmsg = "Scripts directory does not exist"
            raise BadPackage(self.name, errmsg)
        injectorDir = os.path.join(newDir, "injector")
        if self.filesystem.isdir(injectorDir):
            self.workingDir = injectorDir
        else:
            errmsg = "The injector directory does not exist for [%s]" % self.fullName
            raise BadPackage(self.name, errmsg)

    def configure(self):
        self.download()
        return self.findCmd(CONFIGURE)

    def findCmd(self, action, packageList=[], dryRun=False):
        if self.packageVersion > 1:
            return self.findCmd2(action, packageList, dryRun=dryRun)
        else:
            return self.findCmd1(action, packageList)

    def findCmd1(self, action, packageList=[]):
        fullCmd = ''
        extensions = [".py", ".bat", ".pl", ".sh"]
        cmds = {INSTALL: "installer", UNINSTALL: "uninstaller",
                VERIFY: "verify", CONFIGURE: "configure" }
        cmd = cmds.get(action)
        if not cmd:
            raise FeatureRemovedException(action)
        for extension in extensions:
            testCmd = os.path.join(self.scriptsDir, cmd+extension)
            #Logger.debug( testCmd )
            if self.filesystem.isfile(testCmd):
                fullCmd = testCmd
                break
        if not fullCmd:
            errMsg = "Could not find an appropriate script in %s." % self.scriptsDir
            raise BadPackage( self.name, errMsg )
        if packageList:
            fullCmd += " %s" % ','.join(packageList)

        status = self.operatingSystem.run(fullCmd, self.workingDir, self.console)
        return status

    def findCmd2(self, action, packageList=[], dryRun=False):
        cwd = self.filesystem.getcwd()
        sys.path.insert(0, self.scriptsDir)
        self.filesystem.chdir(self.scriptsDir)
        files = glob.glob("*.py")
        files = [x.split('.py')[0] for x in files]
        status = FAIL
        fileFound = False
        for fileName in files:  # FIXME this is stupid
            if fileName.split('.')[0] in ["installer", "verify", "uninstaller", "configure"]:
                continue
            try:
                obj = Spkg.SpkgV4(self.config, Logger.logger)
                self.filesystem.chdir(self.workingDir)
                letters = [ chr( x ) for x in range(65, 91) ]
                random.shuffle(letters)
                randString = ''.join(letters)
                exec("import %s as %s" % (fileName, randString)) # FIXME
                Logger.debug("This is package version %s" % self.packageVersion)
                if self.packageVersion == 2:
                    exec("obj = %s.%s(self.config)" % (randString, fileName))
                elif self.packageVersion == 3:
                    self.config["__INSTANCE__"] = self.instanceName
                    cmdString = "obj = %s.%s(self.config, packageList, Logger.logger)"
                    exec(cmdString % (randString, fileName))
                elif self.packageVersion == 4:
                    self.config["__FUTURE_PACKAGES__"] = packageList
                    self.config["__INSTANCE__"] = self.instanceName
                    cmdString = "obj = %s.%s(self.config, Logger.logger)"
                    exec(cmdString % (randString, fileName))
                else:
                    raise BadPackage( self.name, "Unknown package version %s" % self.packageVersion )
                fileFound = True
                if not dryRun:
                    if action == INSTALL:
                        status = obj.installer()
                    elif action == VERIFY:
                        status = obj.verify()
                    elif action == UNINSTALL:
                        status = obj.uninstaller()
                    elif action == CONFIGURE:
                        status = obj.configure()
                    else:
                        raise FeatureRemovedException(action)
                else:
                    status = OK
                del randString
                if fileName in sys.modules:
                    sys.modules.pop(fileName)
                while self.scriptsDir in sys.path:
                    sys.path.remove(self.scriptsDir)
                break
            except ImportError:
                Logger.debug("File %s is not runnable. Looking for others" % fileName)
                continue
            except SystemExit, e:
                if e.code:
                    status = e.code
                else:
                    status = 0
                fileFound = True
                del randString
                break
            except KeyboardInterrupt:
                Logger.error("Keyboard interrupt detected. Exiting...")
                sys.exit(10)
            except StandardError, e:
                Logger.error("Error detected in %s (%s)." % (fileName, e))
                e = StringIO.StringIO()
                traceback.print_exc(file=e)
                e.seek(0)
                data = e.read()
                ermsg = ''
                for line in data.split('\n'):
                    ermsg += "\n||>>>%s" % line
                Logger.error(ermsg)
                Logger.error("Error ocurred in %s" % fileName)
                fileFound = True
                status = FAIL
                del randString
                break
        self.filesystem.chdir(cwd)
        if not fileFound:
            raise BadPackage(self.name, "Unable to find a suitable script to install.")
        if status == None:
            status = OK
        if status == REBOOT:
            raise RebootRequiredException(self.name)
        if status != OK:
            erstr = "%s: failed with status %s" % (self.fullName, status)
            Logger.error(erstr)
            return FAIL
        return OK

    def download(self):
        if not self.downloaded:
            self.repository.getPackage(self.name, checksum=self.checksum)
            self.initializeFromFilesystem()
            self.downloaded = True
    
    def installAndVerify(self, packageList=[], dryRun=False):
        self.download()
        if self.action == INSTALL:
            status = self.install(packageList, dryRun=dryRun)
            if not dryRun:
                if status == OK:
                    status = self.verify()
                self.writeProgress()
            return status
        raise FeatureRemovedException(self.action)

    def packageStuff(self):
        if not self.fullName:
            Logger.error("Package %s is invalid")
            return FAIL
        return OK
    
    # TESTED
    def install(self, packageList, dryRun=False): 
        dryRunString = ""
        if dryRun:
            dryRunString = " --DRY_RUN-- "
        self.download()
        message = "Beginning installation of (%s)%s" % (self.fullName, dryRunString)
        Logger.info(message)
        self.status = self.findCmd(INSTALL, packageList, dryRun=dryRun)
        Logger.info("Install result for %s%s : %s" % (self.fullName, dryRunString, self.status))
        return self.status

    def executeMaintScript(self, scriptName):
        self.download()
        # remove old history
        outputPath = os.path.join(miniUtility.getSpkgPath(), self.instanceName, 
                                  "output", "%s-output.yml" % scriptName)
        self.filesystem.rmScheduledFile(outputPath)
        message = "Executing (%s) inside package (%s)" %(scriptName, self.fullName)
        Logger.info(message)
        scriptPath = "%s/%s.py" %(self.maintDir, scriptName)
        startDir = os.getcwd()
        os.chdir(self.maintDir)
        if not os.path.isfile(scriptPath):
            msg = "%s does not exist" %scriptPath
            raise BadPackage, (self.name, msg)
        sys.path.append(self.maintDir )
        importString = 'import %s'%scriptName
        status = FAIL # For PyChecker
        cmd = 'status = %s.execute(self.config, Logger.logger)'%scriptName
        job = Job(importString, cmd, self.config)
        status = job.execute()
        #exec('status = %s.execute(self.config, Logger.logger)'%scriptName)
        sys.path.remove(self.maintDir)
        os.chdir(startDir)
        if status == None:
            status = OK
        if type(status) != type(1):
            Logger.warning("Invalid status type (%s: '%s')" % (type(status), status))
            status = FAIL
        return status

    # TESTED
    def verify(self): 
        self.download()
        message = "Verifying package %s" % self.fullName
        Logger.info(message)
        self.status = self.findCmd(VERIFY)
        if self.action != INSTALL:
            self.writeProgress()
        Logger.info("Verify result for %s : %s" % (self.fullName, self.status))
        return self.status

    # TESTED
    def uninstall(self, dryRun=False):
        self.action = UNINSTALL
        self.download()
        dryRunString = ""
        if dryRun:
            dryRunString = " --DRY_RUN-- "
        Logger.info("Uninstalling package %s%s" % (self.name, dryRunString))
        self.status = self.findCmd(UNINSTALL, dryRun=dryRun)
        if not dryRun:
            self.writeProgress()
        Logger.info("Uninstall result for %s%s : %s" % (self.fullName, dryRunString, self.status))
        return self.status

    # TESTED
    def getDateString(self):
        d = datetime.datetime.today()
        dateString = "%d-%d-%d-%d-%d-%d" %(d.year, d.month, d.day, d.hour, d.minute, d.second ) 
        return( dateString )

    ### TESTED
    def writeProgress(self):
        timeString = time.ctime()
        progressData = self.filesystem.getProgressData(self.instanceName)
        if not progressData.has_key(self.fullName):
            progressData[self.fullName] = {"INSTALLED": "NA", "UNINSTALLED": "NA", "VERIFIED": "NA", "DEPENDENCY_ERRORS": []}
        if self.action == INSTALL:
            if progressData.get(self.fullName) == None:
                Logger.warning("UNREACHABLE CODE REACHED!")
                progressData[self.fullName] = {}
            if self.fullName:
                if self.status == OK:
                    progressData[self.fullName]['INSTALLED']   = timeString
                    # FIXME: shouldn't this next line be 'NA'? -- pbanka
                    progressData[self.fullName]['VERIFIED']    = timeString 
                    progressData[self.fullName]['UNINSTALLED'] = 'NA'

                else:
                    progressData[self.fullName]['INSTALLED']   = "BROKEN"
                    progressData[self.fullName]['VERIFIED']    = 'NA'
                    progressData[self.fullName]['UNINSTALLED'] = 'NA'
            else:
                Logger.error("Unnamed package installed: (%s)" % (self.name))
                return FAIL
    
        elif self.action == UNINSTALL:
            likePackages = [ p for p in progressData if p.startswith(self.name) and p != self.fullName ]
            for p in likePackages:
                del progressData[p]
            if self.status == OK:
                progressData[self.fullName]['UNINSTALLED'] = timeString
                progressData[self.fullName]['INSTALLED']   = 'NA'
            else:
                progressData[self.fullName]['UNINSTALLED'] = "BROKEN"

        elif self.action == VERIFY:
            progressData[self.fullName]['VERIFIED'] = timeString
        progressData[self.fullName]['DEPENDENCY_ERRORS'] = self.dependencyErrors
        self.filesystem.updateProgress({"install-progress":progressData}, self.instanceName, overwrite=True)
        return OK
