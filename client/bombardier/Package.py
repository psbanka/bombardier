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

import os, string, time, gc, datetime, ConfigParser

import miniUtility, MetaData, Exceptions, Logger
from staticData import *

class Package:

    """This class provides an abstraction for a downloadable,
    installable, verifiable, backupable, and uninstallable thing. Each
    package is held in a database on the server and downloaded from
    the server."""

    ### TESTED
    # FIXME: Refactor: we do not need a config *and* a repository value
    def __init__(self, name, repository, config, filesystem, server, windows):
        self.name         = name
        self.repository   = repository
        self.filesystem   = filesystem
        self.server       = server
        self.windows      = windows
        self.action       = INSTALL
        self.installed    = False
        self.dependencies = []
        self.dependsOnMe  = []
        self.console      = False
        self.fullName     = ''
        self.reboot       = False
        self.preboot      = False
        self.autoReboot   = False
        self.checksum     = ''
        self.config       = config
        self.status       = OK
        self.priority     = AVERAGE
        self.metaData     = MetaData.MetaData({})
        self.workingDir   = ''
        self.scriptsDir   = ''
        self.downloaded   = False

    def invalidate(self):
        erstr = "INVALID PACKAGE: %s" % self.name
        Logger.error(erstr)
        self.filesystem.warningLog(erstr, self.server)
        self.status = FAIL
        
    def gatherDependencies(self):
        dependencies = 0
        while 1 == 1:
            try:
                depstr = "dep"+`dependencies`
                dep = self.metaData.get("dependencies", depstr)
                self.dependencies.append(dep)
                dependencies += 1
            except ConfigParser.NoSectionError:
                break
            except ConfigParser.NoOptionError:
                break
    
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
                            raise Exceptions.BadPackage, (self.name, msg)
                    else:
                        msg = "The server's record is completely corrupt"
                        raise Exceptions.BadPackage, (self.name, msg)
                else:
                    msg = "server record is corrupt: no 'install' section"
                    raise Exceptions.BadPackage, (self.name, msg)
            else:
                msg = "server record is corrupt: not a dictionary"
                raise Exceptions.BadPackage, (self.name, msg)
        else:
            msg = "No metadata found for this package"
            raise Exceptions.BadPackage, (self.name, msg)

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
        chk = self.metaData.data["install"].get('autoreboot')
        self.autoReboot = miniUtility.evalBoolean(chk)
        chk = self.metaData.data["install"].get('preboot')
        self.preboot = miniUtility.evalBoolean(chk)
        self.evalPriority()
        self.gatherDependencies()


    def injector(self):  
        """ Expects a standard package to be extracted in the packages
        directory """
        packagePath = miniUtility.getPackagePath()
        if not self.fullName:
            self.status = FAIL
            return FAIL
        newDir = os.path.join(packagePath, self.fullName)
        self.scriptsDir = os.path.join(newDir, "scripts")
        if not self.filesystem.isdir(self.scriptsDir):
            self.filesystem.updateCurrentAction("Package is corrupt or missing.", 0, self.server)
            Logger.error("Scripts directory does not exist")
            self.status = FAIL
            return FAIL
        backupDir = os.path.join(newDir, "backup")
        injectorDir = os.path.join(newDir, "injector")
        if self.filesystem.isdir(backupDir):
            self.workingDir = backupDir
            return OK
        elif self.filesystem.isdir(injectorDir):
            self.workingDir = injectorDir
            return OK
        else:
            Logger.error("Neither the injector nor the "\
                              "backup directory exists for [%s]" % self.fullName)
            self.filesystem.updateCurrentAction("Package is corrupt or missing.", 0, self.server)
            self.status = FAIL
            return FAIL

    def preload(self): ### TESTED

        """Expects there to be a .ini file that belongs to the package
        under evaluation in the spkg packages directory. Will determine if
        there are any files to be downloaded to be placed into the
        injector directory.

        Note that self.fullName is expected to be a 'dash-version' name.

        This will download all directives into the current directory.
        """
        if not self.fullName:
            return OK
        downloads = 0
        while 1 == 1:
            try:
                downloadUrl = self.metaData.get("install",
                                                "download"+`downloads`)
                downloadSource = downloadUrl[:downloadUrl.rfind("/")]
                downloadFile   = downloadUrl[downloadUrl.rfind("/")+1:]
                erstr = "Package contains download directives: "\
                        "Downloading %s from %s..." % \
                        (downloadFile, downloadSource)
                Logger.info(erstr)
                self.filesystem.updateCurrentAction("Downloading dependency: %s" % downloadFile,
                                                    27, self.server)
                status = OK
                if status == FAIL:
                    Logger.error("Unable to download (%s) from %s, aborting"\
                                      "package installation." % (downloadFile, downloadSource))
                    for entry in self.repository.packages.keys():
                        if entry.startswith(self.name[:len(self.name) / 2]):
                            Logger.info("Possible completion: (%s)" % entry)
                            Logger.info("Data: %s" % self.repository.packages[entry])
                    return FAIL
                downloads += 1
            except ConfigParser.NoOptionError:
                break
            except ConfigParser.NoSectionError:
                break
        return OK

    def findCmd(self, action, abortIfTold, packageList=[]):
        fullCmd = ''
        extensions = [".py", ".bat", ".pl"]
        cmds = {INSTALL: "installer", UNINSTALL: "uninstaller", VERIFY: "verify", BACKUP: "backup"}
        cmd = cmds.get(action)
        if not cmd:
            Logger.error("Unknown action: [%s]" % action)
            return FAIL
        for extension in extensions:
            testCmd = os.path.join(self.scriptsDir, cmd+extension)
            Logger.debug( testCmd )
            if self.filesystem.isfile(testCmd):
                fullCmd = testCmd
                break
        if not fullCmd:
            Logger.error("Could not find an appropriate script.")
            return FAIL
        erstr = "Unable to run script script: %s" % fullCmd
        if sys.platform != "linux2" and self.console and action== INSTALL:
            abortIfTold()
            self.filesystem.beginConsole()
            if fullCmd.endswith(".py"):
                self.windows.runPython( fullCmd, self.workingDir )
            elif fullCmd.endswith(".bat"):
                self.windows.runCmd( fullCmd, self.workingDir )
            else: # crutch for running perl until we decide on a permanent location
                if sys.platform != "linux2":
                    import win32api
                    win32api.ShellExecute(0, "open", "perl.exe", fullCmd, self.workingDir, 1)
            status = self.filesystem.watchForTermination(sleepTime=1, abortIfTold=abortIfTold)
        else:
            if packageList: # don't know how to do this with shellExecute
                fullCmd += " %s" %string.join(packageList,',')
            status = self.filesystem.execute(fullCmd, erstr, dieOnExit=0, captureOutput=True,
                                             workingDirectory=self.workingDir)
        if status == REBOOT:
            erstr = "%s %s: indicated reboot action is necessary." % (self.fullName, status)
            Logger.warning(erstr)
            return REBOOT
        if status != OK:
            erstr = "%s %s: failed with status %s" % (cmd, self.fullName, status)
            Logger.error(erstr)
            return FAIL
        return OK

    def download(self, abortIfTold):
        if not self.downloaded:
            self.filesystem.updateCurrentAction("Downloading package...", 10,
                                                self.server, fastUpdate=True)
            abortIfTold()
            status = self.repository.getPackage(self.name, abortIfTold, checksum=self.checksum)
            if status == FAIL:
                self.status = FAIL
                self.filesystem.warningLog("Problems downloading package %s" % self.name,
                                           self.server)
                return FAIL
            status = self.injector()
            self.downloaded = True
        return OK
    
    def process(self, abortIfTold, packageList=[]):
        self.download(abortIfTold)
        if self.status == FAIL:
            # FIXME: may be a good idea to re-download the package.
            erstr = "Package %s is corrupt or could not be "\
                    "downloaded." % self.fullName
            Logger.error(erstr)
            self.filesystem.warningLog(erstr, self.server)
            return FAIL
        if self.action == INSTALL:
            abortIfTold()
            if self.autoReboot:
                Logger.info("This is an auto-reboot package. "\
                                 "Assuming package installs successfully.")
                self.windows.autoLogin(self.config)
                self.windows.restartOnLogon()
                self.writeProgress()
            self.install(packageList, abortIfTold)
            Logger.info("Install result: %s" % self.status)
            if self.status != FAIL:
                abortIfTold()
                self.verify(abortIfTold)
                Logger.info("Verify result: %s" % self.status)
            self.writeProgress()
            return self.status
        Logger.error("Unknown action: [%s]" % self.action)
        return FAIL

    def chdir(self):
        self.cwd = os.getcwd()
        Logger.debug(" Changing directory to %s" % os.path.join(miniUtility.getSpkgPath(), PACKAGES))
        self.filesystem.chdir(os.path.join(miniUtility.getSpkgPath(), PACKAGES))
        if self.fullName:
            try:
                self.filesystem.chdir(self.fullName)
                return OK
            except OSError, e:
                ermsg = "Unable to change directory to package. Aborting. (%s)" % `e`
                Logger.error(ermsg)
        else:
            Logger.error("No package fullname -- package not found. Aborting.")
        self.status = FAIL
        return FAIL

    def packageStuff(self):
        if not self.fullName:
            Logger.error("Package %s is invalid")
            return FAIL
        return OK
    
    # TESTED
    def install(self, packageList, abortIfTold): 
        self.download(abortIfTold)
        self.filesystem.updateCurrentAction("Installing...", 50, self.server,
                                            fastUpdate=True)
        message = "Beginning installation of (%s)" % self.fullName
        Logger.info(message)
        self.preload()
        abortIfTold()
        self.status = self.findCmd(INSTALL, abortIfTold, packageList)
        return self.status
    
    # TESTED
    def verify(self, abortIfTold): 
        self.download(abortIfTold)
        self.filesystem.updateCurrentAction("Verifying...", 90, self.server,
                                            fastUpdate=True)
        message = "Verifying package %s" % self.fullName
        Logger.info(message)
        abortIfTold()
        self.status = self.findCmd(VERIFY, abortIfTold)
        if self.action != INSTALL:
            self.writeProgress()
        return self.status

    # TESTED
    def uninstall(self, abortIfTold):
        self.download(abortIfTold)
        abortIfTold()
        status = self.backup(abortIfTold) #^ CHECK
        if status != OK:
            ermsg = "Aborting uninstall: Unable to back up "\
                    "%s before uninstalling." % self.fullName
            Logger.error(ermsg)
            return FAIL
        if self.status == FAIL:
            Logger.info("Not attempting to uninstall malformed package.")
            return FAIL
        Logger.info("Uninstalling package %s" % self.name)
        self.filesystem.updateCurrentAction("Uninstalling...", 70, self.server)
        abortIfTold()
        self.status = self.findCmd(UNINSTALL, abortIfTold)
        self.writeProgress()
        return self.status

    # TESTED
    def getDateString(self):
        d = datetime.datetime.today()
        dateString = "%d-%d-%d-%d-%d-%d" %(d.year, d.month, d.day, d.hour, d.minute, d.second ) 
        return( dateString )

    #^ UNTESTED
    def cleanBackupPath(self):
        if os.path.isdir(self.backupPath):
            Logger.info("Removing current backup directory")
            try:
                self.filesystem.rmtree(self.backupPath)
            except OSError, e:
                Logger.error("Unable to remove old backup directory (%s)" % self.backupPath)
                Logger.error("Error message: %s" % e)
                return FAIL
        self.filesystem.mkdir(self.backupPath)
        return OK

    ## TESTED
    def backup(self, abortIfTold):
        def returnNice(startDir, status, filesystem):
            filesystem.chdir(startDir)
            return status
        startDir = self.filesystem.getcwd()
        Logger.info("---------------------------Backing up %s" %self.fullName)
        # FIXME: cheap hack -- should use something like findcmd. -pbanka
        filePath = os.path.join(miniUtility.getPackagePath(), self.fullName, "scripts", "backup.py")
        if not self.filesystem.isfile(filePath):
            Logger.info("No backup executable -- not backing up")
            return OK
        # / cheap hack

        try:
            self.rootName = self.metaData.get("backup", "rootname")
        except ConfigParser.NoOptionError:
            ermsg = "Using package %s name for root name because no rootname is defined" % self.name
            Logger.warning(ermsg)
            self.rootName = self.name
        except ConfigParser.NoSectionError:
            ermsg = "Using package %s name for root name because no rootname is defined" % self.name
            Logger.warning(ermsg)
            self.rootName = self.name
        self.tarFileName = self.rootName + ".tar.gz"
        self.dateString = self.getDateString()
        self.filesystem.updateCurrentAction("Backing up...", 40, self.server)
        Logger.info("Backing up package %s" % self.fullName)
        self.chdir()
        self.backupPath = os.path.join(miniUtility.getPackagePath(), self.fullName, 'backup')
        Logger.debug("Backing up to %s" % self.backupPath)
        abortIfTold()
        status = self.cleanBackupPath()
        if status == FAIL:
            return returnNice(startDir, FAIL, self.filesystem)
        status = self.injector()
        if status == FAIL:
            return returnNice(startDir, FAIL, self.filesystem)
        if self.workingDir.split(os.path.sep)[-1] != "backup":
            Logger.error("could not create a backup directory: %s" % os.getcwd())
            return returnNice(startDir, FAIL, self.filesystem)
        abortIfTold()
        status = self.findCmd(BACKUP, abortIfTold)
        if status != OK:
            Logger.info("No backup script was found. Not backing up.")
            return returnNice(startDir, OK, self.filesystem)
        Logger.info("Backup succeeded. Creating compressed package")
        self.filesystem.chdir( self.backupPath )
        abortIfTold()
        status = self.buildTarFile()
        if status == FAIL:
            return returnNice(startDir, FAIL, self.filesystem)
        abortIfTold()
        status = self.sendTarFileToWebService()
        return returnNice(startDir, status, self.filesystem)

    #^ UNTESTED
    def buildTarFile(self):
        self.filesystem.updateCurrentAction("Packing and compressing backup...", 50, self.server)
        errmsg = "Creating tarfile %s in backupPath %s..." %( self.tarFileName, self.backupPath )
        Logger.info(errmsg)
        if not self.filesystem.isdir( self.backupPath ):
            Logger.error("Directory does not exist: " + self.backupPath)
            return FAIL
        self.filesystem.chdir( self.backupPath )
        self.tarfileName = self.rootName + ".tar.gz"
        self.filesystem.createTar( self.tarfileName, self.backupPath )
        Logger.debug( "%s successfully created" %(self.tarFileName) )
        return OK

    #^ UNTESTED
    def sendTarFileToWebService( self ):
        self.filesystem.updateCurrentAction("Uploading backup to web service...", 60, self.server)
        self.newPackageName = "%s-%s" %(self.rootName, self.dateString)
        Logger.debug( "newPackageName : %s" %(self.newPackageName) )
        Logger.debug( "Creating spkg %s from %s using the webservice..."
                           %(self.newPackageName, self.tarFileName) )

        args = {'packagename':self.newPackageName, 'installscript':self.rootName}
        filePath = os.path.join( self.backupPath, self.tarFileName )
        putData = self.filesystem.getBinaryDataFromFilePath( filePath )
        if putData == None:
            Logger.error("No data in backup file %s" % filePath)
            return FAIL
        try:
            output = self.server.serviceYamlRequest( "website/service/package/", args, putData,
                                                     debug=True, legacyPathFix=False )
        except Exceptions.ServerUnavailable, e:
            Logger.error("Unable to send backup data to server %s" % e)
            return FAIL
        status = output.get("status")
        warnings = output.get("warnings")
        if not warnings:
            warnings = []
        errors   = output.get("errors")
        if not errors:
            errors = []
        for warning in warnings:
            Logger.warning("Warning returned from webservice: %s" %warning)
        for error in errors:
            Logger.error("Error returned from webservice: %s" %error)
        if status != OK:
            Logger.error("Uploading tarball to the webserver failed.")
            if not status:
                Logger.error("No return from server")
            return FAIL
        Logger.debug( "output from request: \n%s" %(output.get("logdata")) )
        return OK

    ### TESTED
    def pickMostRecentPackage(self):
        self.filesystem.updateCurrentAction("Determining most recent version...", 5, self.server)
        installablePackages = self.repository.getFullPackageNames()
        targetPackages = []
        for package in installablePackages:
            if package.upper().startswith(self.name.upper()):
                targetPackages.append(package)
        if len(targetPackages) == 0:
            return FAIL
        selection = targetPackages[0]
        for package in targetPackages:
            newVersion = int(package[package.rfind('-')+1:])
            currentVersion = int(selection[selection.rfind('-')+1:])
            if newVersion > currentVersion:
                selection = package
        self.fullName = selection
        return OK

    def mkInstallSummary(self, progressData):
        installed, uninstalled, brokenInstalled, brokenUninstalled = miniUtility.getInstalledUninstalledTimes(progressData)
        output = []
        for package in installed:
            output.append("%25s: %s" % (package[0], package[1]))
        if uninstalled:
            output.append("===============================")
            for package in uninstalled:
                output.append("(%25s: %s)" % (package[0], package[1]))
        if brokenInstalled:
            output.append("BROKEN:========================")
            for package in brokenInstalled:
                output.append("- %25s" % (package[0]))
        progressPath = miniUtility.getProgressPath2()

        fh = self.filesystem.open(progressPath, 'wb')
        fh.write('\n'.join(output))
        fh.flush()
        fh.close()
        del fh

    ### TESTED
    def writeProgress(self):
        self.filesystem.updateCurrentAction("Finished.", 100, self.server)

        timeString = time.ctime()
        progressData = self.filesystem.getProgressData()
        if progressData == {}:
            Logger.warning("Empty status data")
        # gc.collect() # does windows suck? here's proof. -pbanka
        if not progressData.has_key(self.fullName):
            progressData[self.fullName] = {"INSTALLED": "NA", "UNINSTALLED": "NA", "VERIFIED": "NA"}
        if self.action == INSTALL:
            if progressData.get(self.fullName) == None:
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
            if self.status == OK:
                progressData[self.fullName]['UNINSTALLED'] = timeString
                progressData[self.fullName]['INSTALLED']   = 'NA'
            else:
                progressData[self.fullName]['UNINSTALLED'] = "BROKEN"

        elif self.action == VERIFY:
            progressData[self.fullName]['VERIFIED'] = timeString
        self.filesystem.updateProgress({"install-progress":progressData},
                                       self.server, overwrite=True)
        # gc.collect() # just try removing these and see if the unit tests pass -pbanka
        self.mkInstallSummary(progressData)
        return OK
