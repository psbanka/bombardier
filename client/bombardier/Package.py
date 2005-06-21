#!/cygdrive/c/Python24/python.exe

import os, string, yaml, time, gc, datetime, win32api, ConfigParser

import bombardier.miniUtility as miniUtility
import bombardier.MetaData as MetaData
import bombardier.Exceptions as Exceptions

from bombardier.staticData import *

class Package:

    """This class provides an abstraction for a downloadable,
    installable, verifiable, backupable, and uninstallable thing. Each
    package is held in a database on the server and downloaded from
    the server."""

    ### TESTED
    # FIXME: Refactor: we do not need a config *and* a repository value
    def __init__(self, name, repository, config, logger, filesystem, server, windows):
        self.name         = name
        self.repository   = repository
        self.logger       = logger
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
        self.filesystem.updateProgressFile({"status": {"package":self.name}})
        self.initialize()

    def invalidate(self):
        erstr = "INVALID PACKAGE: %s" % self.name
        self.logger.error(erstr)
        self.server.serverLog("ERROR", erstr, self.name)
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
            self.logger.warning(ermsg)
            self.priority = AVERAGE
        else:
            try:
                self.priority = int(self.priority)
            except ValueError:
                ermsg = "Package %s has an invalid priority value"\
                        "(assuming %s)" % (self.name, AVERAGE)
                self.logger.warning(ermsg)
                self.priority = AVERAGE

    def checkMetaData(self):
        if self.metaData.data:
            if type(self.metaData.data) == type(dict()):
                if self.metaData.data.get("install"):
                    if type(self.metaData.data["install"]) == type({}):
                        self.fullName = self.metaData.data['install'].get('fullName')
                        if self.fullName:
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
        self.filesystem.updateCurrentAction("Initializing package.", 0)
        self.metaData = self.repository.getMetaData(self.name)
        self.checkMetaData()
        self.checksum = self.metaData.data['install'].get('md5sum')
        if not self.checksum:
            ermsg = "Package %s does not have a checksum field"\
                    " (not checking)" % (self.name)
            self.logger.warning(ermsg)
        if self.metaData.data['install'].get('md5list'):
            self.checksum = self.metaData.data['install']['md5list']
        chk = self.metaData.data["install"].get('console')
        self.console = miniUtility.evalBoolean(chk)
        chk = self.metaData.data["install"].get('reboot')
        self.reboot = miniUtility.evalBoolean(chk)
        chk = self.metaData.data["install"].get('autoreboot')
        self.autoReboot = miniUtility.evalBoolean(chk)
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
            self.filesystem.updateCurrentAction("Package is corrupt or missing.", 0)
            self.logger.error("Scripts directory does not exist")
            self.status = FAIL
            return FAIL
        backupDir = os.path.join(newDir, "backup")
        injectorDir = os.path.join(newDir, "injector")
        if self.filesystem.isdir(backupDir):
            self.workingDir = backupDir
            self.filesystem.updateCurrentAction("Verified package structure", 30)
            return OK
        elif self.filesystem.isdir(injectorDir):
            self.workingDir = injectorDir
            self.filesystem.updateCurrentAction("Verified package structure", 30)
            return OK
        else:
            self.logger.error("Neither the injector nor the "\
                              "backup directory exists for [%s]" % self.fullName)
            self.filesystem.updateCurrentAction("Package is corrupt or missing.", 0)
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
                self.logger.info(erstr)
                self.filesystem.updateCurrentAction("Downloading dependency: %s" % downloadFile, 27)
                # FIXME: this doesn't work any more, and I'm not sure it's useful.
                #status = utility.wget(downloadSource, downloadFile, self.config, self.logger,
                #                      debug = 1, retries = 1, dieOnFail = 0, destDir = directory)
                status = OK
                if status == FAIL:
                    self.logger.error("Unable to download (%s) from %s, aborting"\
                                      "package installation." % (downloadFile, downloadSource))
                    for entry in self.repository.packages.keys():
                        if entry.startswith(self.name[:len(self.name) / 2]):
                            self.logger.info("Possible completion: (%s)" % entry)
                            self.logger.info("Data: %s" % self.repository.packages[entry])
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
            self.logger.error("Unknown action: [%s]" % action)
            return FAIL
        for extension in extensions:
            testCmd = os.path.join(self.scriptsDir, cmd+extension)
            self.logger.debug( testCmd )
            if self.filesystem.isfile(testCmd):
                fullCmd = testCmd
                break
        if not fullCmd:
            self.logger.error("Could not find an appropriate script.")
            return FAIL
        erstr = "Unable to run script script: %s" % fullCmd
        if self.console and action== INSTALL:
            abortIfTold()
            self.filesystem.beginConsole(self.logger)
            pythonCmd = miniUtility.runPythonScript("")
            program = 'win32api.ShellExecute(0, "open", %s, %s, %s, 1)' % (pythonCmd, fullCmd, self.workingDir)
            self.logger.info("running: %s" % program)
            win32api.ShellExecute(0, "open", pythonCmd, fullCmd, self.workingDir, 1)
            status = self.filesystem.watchForTermination(logger=self.logger,
                                                         sleepTime=1, abortIfTold=abortIfTold)
        else:
            if packageList: # don't know how to do this with shellExecute
                fullCmd += " %s" %string.join(packageList,',')
            status = self.filesystem.execute(fullCmd, erstr, dieOnExit=0, captureOutput=True,
                                             logger = self.logger, workingDirectory=self.workingDir)
        if status == REBOOT:
            erstr = "%s %s: indicated reboot action is necessary." % (self.fullName, status)
            self.logger.warning(erstr)
            return REBOOT
        if status != OK:
            erstr = "%s %s: failed with status %s" % (cmd, self.fullName, status)
            self.logger.error(erstr)
            return FAIL
        return OK

    def download(self, abortIfTold):
        if not self.downloaded:
            self.filesystem.updateCurrentAction("Downloading package...", 10)
            abortIfTold()
            status = self.repository.getPackage(self.name, abortIfTold, checksum=self.checksum)
            if status == FAIL:
                self.status = FAIL
                self.server.serverLog("ERROR", "Problems downloading package %s" % self.name,
                                      self.name)
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
            self.logger.error(erstr)
            self.server.serverLog("ERROR", erstr, self.name)
            return FAIL
        if self.action == INSTALL:
            abortIfTold()
            if self.autoReboot:
                self.logger.info("This is an auto-reboot package. "\
                                 "Assuming package installs successfully.")
                self.config.autoLogin()
                self.windows.restartOnLogon()
                self.writeProgress()
            installResult = self.install(packageList, abortIfTold)
            self.logger.info("Install result: %s" % installResult)
            if installResult != FAIL:
                abortIfTold()
                verifyResult = self.verify(abortIfTold)
                self.logger.info("Verify result: %s" % verifyResult)
                if verifyResult == OK:
                    self.writeProgress()
                    return installResult
            return FAIL
        self.logger.error("Unknown action: [%s]" % self.action)
        return FAIL

    def chdir(self):
        self.cwd = os.getcwd()
        self.filesystem.chdir(os.path.join(miniUtility.getSpkgPath(), PACKAGES))
        if self.fullName:
            try:
                self.filesystem.chdir(self.fullName)
                return OK
            except OSError, e:
                ermsg = "Unable to change directory to package. Aborting. (%s)" % `e`
                self.logger.error(ermsg)
        else:
            self.logger.error("No package fullname -- package not found. Aborting.")
        self.status = FAIL
        return FAIL

    def packageStuff(self):
        if not self.fullName:
            self.logger.error("Package %s is invalid")
            return FAIL
        return OK
    
    # TESTED
    def install(self, packageList, abortIfTold): 
        self.download(abortIfTold)
        self.filesystem.updateCurrentAction("Installing...", 50)
        message = "Beginning installation of (%s)" % self.fullName
        self.logger.info(message)
        self.preload()
        abortIfTold()
        status = self.findCmd(INSTALL, abortIfTold, packageList)
        return status
    
    # TESTED
    def verify(self, abortIfTold): 
        self.download(abortIfTold)
        self.filesystem.updateCurrentAction("Verifying...", 90)
        message = "Verifying package %s" % self.fullName
        self.logger.info(message)
        abortIfTold()
        status = self.findCmd(VERIFY, abortIfTold)
        if self.action != INSTALL:
          self.writeProgress()
        return status

    # TESTED
    def uninstall(self, abortIfTold):
        self.download(abortIfTold)
        abortIfTold()
        status = self.backup(abortIfTold) #^ CHECK
        if status != OK:
            ermsg = "Aborting uninstall: Unable to back up "\
                    "%s before uninstalling." % self.fullName
            self.logger.error(ermsg)
            return FAIL
        if self.status == FAIL:
            self.logger.info("Not attempting to uninstall malformed package.")
            return FAIL
        self.logger.info("Uninstalling package %s" % self.name)
        self.filesystem.updateCurrentAction("Uninstalling...", 70)
        abortIfTold()
        status = self.findCmd(UNINSTALL, abortIfTold)
        if status != FAIL:
            self.writeProgress()
        return status

    # TESTED
    def getDateString(self):
        d = datetime.datetime.today()
        dateString = "%d-%d-%d-%d-%d-%d" %(d.year, d.month, d.day, d.hour, d.minute, d.second ) 
        return( dateString )

    #^ UNTESTED
    def cleanBackupPath(self):
        if os.path.isdir(self.backupPath):
            self.logger.info("Removing current backup directory")
            try:
                self.filesystem.rmtree(self.backupPath)
            except OSError:
                self.logger.error("Unable to remove old backup directory")
                return FAIL
        self.filesystem.mkdir(self.backupPath)
        return OK

    ## TESTED
    def backup(self, abortIfTold):
        def returnNice(startDir, status, filesystem):
            filesystem.chdir(startDir)
            return status
        startDir = self.filesystem.getcwd()
        self.logger.info("---------------------------Backing up %s" %self.fullName)
        # FIXME: cheap hack -- should use something like findcmd. -pbanka
        filePath = os.path.join(miniUtility.getPackagePath(), self.fullName, "scripts", "backup.py")
        if not self.filesystem.isfile(filePath):
            self.logger.info("No backup executable -- not backing up")
            return OK
        # / cheap hack

        try:
            self.rootName = self.metaData.get("backup", "rootname")
        except ConfigParser.NoOptionError:
            ermsg = "Using package %s name for root name because no rootname is defined" % self.name
            self.logger.warning(ermsg)
            self.rootName = self.name
        except ConfigParser.NoSectionError:
            ermsg = "Using package %s name for root name because no rootname is defined" % self.name
            self.logger.warning(ermsg)
            self.rootName = self.name
        self.tarFileName = self.rootName + ".tar.gz"
        self.dateString = self.getDateString()
        self.filesystem.updateCurrentAction("Backing up...", 40)
        self.logger.info("Backing up package %s" % self.fullName)
        self.chdir()
        self.backupPath = os.path.join(miniUtility.getPackagePath(), self.fullName, 'backup')
        abortIfTold()
        status = self.cleanBackupPath()
        if status == FAIL:
            return returnNice(startDir, FAIL, self.filesystem)
        status = self.injector()
        if status == FAIL:
            return returnNice(startDir, FAIL, self.filesystem)
        if self.workingDir.split('\\')[-1] != "backup":
            self.logger.error("could not create a backup directory: %s" % os.getcwd())
            return returnNice(startDir, FAIL, self.filesystem)
        abortIfTold()
        status = self.findCmd(BACKUP, abortIfTold)
        if status != OK:
            self.logger.info("No backup script was found. Not backing up.")
            return returnNice(startDir, OK, self.filesystem)
        self.logger.info("Backup succeeded. Creating compressed package")
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
        self.filesystem.updateCurrentAction("Packing and compressing backup...", 50)
        errmsg = "Creating tarfile %s in backupPath %s..." %( self.tarFileName, self.backupPath )
        self.logger.info(errmsg)
        if not self.filesystem.isdir( self.backupPath ):
            self.logger.error("Directory does not exist: " + self.backupPath)
            return FAIL
        self.filesystem.chdir( self.backupPath )
        self.tarfileName = self.rootName + ".tar.gz"
        self.filesystem.createTar( self.tarfileName, self.backupPath )
        self.logger.debug( "%s successfully created" %(self.tarFileName) )
        return OK

    #^ UNTESTED
    def sendTarFileToWebService( self ):
        self.filesystem.updateCurrentAction("Uploading backup to web service...", 60)
        self.newPackageName = "%s-%s" %(self.rootName, self.dateString)
        self.logger.debug( "newPackageName : %s" %(self.newPackageName) )
        self.logger.debug( "Creating spkg %s from %s using the webservice..."
                           %(self.newPackageName, self.tarFileName) )

        args = {'packagename':self.newPackageName, 'installscript':self.rootName}
        filePath = os.path.join( self.backupPath, self.tarFileName )
        putData = self.filesystem.getBinaryDataFromFilePath( filePath, self.logger )
        if putData == None:
            self.logger.error("No data in backup file %s" % filePath)
            return FAIL
        try:
            output = self.server.serviceYamlRequest( "package", args, putData, debug=True )
        except Exceptions.ServerUnavailable, e:
            self.logger.error("Unable to send backup data to server %s" % e)
            return FAIL
        status = output.get("status")
        warnings = output.get("warnings")
        if not warnings:
            warnings = []
        errors   = output.get("errors")
        if not errors:
            errors = []
        for warning in warnings:
            self.logger.warning("Warning returned from webservice: %s" %warning)
        for error in errors:
            self.logger.error("Error returned from webservice: %s" %error)
        if status != OK:
            self.logger.error("Uploading tarball to the webserver failed.")
            if not status:
                self.logger.error("No return from server")
            return FAIL
        self.logger.debug( "output from request: \n%s" %(output.get("logdata")) )
        return OK

    ### TESTED
    def pickMostRecentPackage(self):
        self.filesystem.updateCurrentAction("Determining most recent version...", 5)
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

    ### TESTED
    def writeProgress(self):
        # FIXME: REMOVE EVEN IF NOT SAME VERSION
        """ will write its progress status to the
        'install-progress.yml' file.  """
        self.filesystem.updateCurrentAction("Finished.", 100)

        progressPath = miniUtility.getProgressPath()
        timeString = time.ctime()
        progressData = {}

        if self.filesystem.isfile(progressPath):
          content = self.filesystem.convertProgressData(open(progressPath).read())
          try:
            progressData = yaml.load(content).next()
          except Exception, e:
            self.logger.error("Loading the progress data: %s" % progressPath)
            self.logger.error("%s" % e)
            return FAIL

        gc.collect() # does windows suck? here's proof. -pbanka
        if not progressData.has_key(self.fullName):
            progressData[self.fullName] = {"INSTALLED": "NA", "UNINSTALLED": "NA", "VERIFIED": "NA"}
        if self.action == INSTALL:
          if progressData.get(self.fullName) != None:
            if  progressData[self.fullName]['INSTALLED'] != 'NA':
              erstr = "Possible installation cycling with '%s'." % self.fullName
              self.logger.error(erstr)
              return OK
          else:
            progressData[self.fullName] = {}
          if self.fullName:
            progressData[self.fullName]['INSTALLED']   = timeString
            progressData[self.fullName]['VERIFIED']    = timeString
            progressData[self.fullName]['UNINSTALLED'] = 'NA'
          else:
            self.logger.error("Unnamed package installed: (%s)" % (self.name))
            return FAIL

        elif self.action == UNINSTALL:
          progressData[self.fullName]['UNINSTALLED'] = timeString
          progressData[self.fullName]['INSTALLED']   = 'NA'

        elif self.action == VERIFY:
          progressData[self.fullName]['VERIFIED'] = timeString

        yamlString = yaml.dump(progressData)
        try:
            os.unlink(progressPath)
        except:
            gc.collect() # paranoia
            pass
        fh = self.filesystem.open(progressPath, 'wb')
        fh.write(yamlString)
        fh.flush()
        fh.close()
        del fh
        gc.collect() # just try removing these and see if the unit tests pass -pbanka
        hostname = os.environ["COMPUTERNAME"]
        status = self.server.serviceYamlRequest("clientstatus", 
                                                {"client":hostname, "message":"install"},
                                                progressData)
        if status == "FAIL":
            ermsg = "Unable to upload installation progress"
            self.logger.warning(ermsg)
        return OK
