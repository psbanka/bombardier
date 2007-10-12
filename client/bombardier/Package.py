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

import os, time, datetime, ConfigParser, glob, random
import sys, StringIO, traceback

import miniUtility, MetaData, Exceptions, Logger
from staticData import *

class Package:

    """This class provides an abstraction for a downloadable,
    installable, verifiable, and uninstallable thing. Each
    package is held in a database on the server and downloaded from
    the server."""

    ### TESTED
    def __init__(self, name, repository, config, filesystem, server, operatingSystem):
        self.name         = name
        self.repository   = repository
        self.filesystem   = filesystem
        self.server       = server
        self.operatingSystem      = operatingSystem
        self.action       = INSTALL
        self.installed    = False
        self.dependencies = []
        self.dependsOnMe  = []
        self.console      = False
        self.fullName     = ''
        self.reboot       = False
        self.preboot      = False
        self.autoReboot   = False
        self.packageVersion = 0
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
        chk = self.metaData.data.get("package-version")
        if type(chk) == type(1):
            self.packageVersion = chk
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
        injectorDir = os.path.join(newDir, "injector")
        if self.filesystem.isdir(injectorDir):
            self.workingDir = injectorDir
            return OK
        else:
            Logger.error("The injector directory does not exist for [%s]" % self.fullName)
            self.filesystem.updateCurrentAction("Package is corrupt or missing.", 0, self.server)
            self.status = FAIL
            return FAIL

    def preload(self): ### TESTED

        """Note that self.fullName is expected to be a 'dash-version' name.

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

    def configure(self):
        status = self.download()
        if status == OK:
            return self.findCmd(CONFIGURE)
        else:
            return FAIL

    def findCmd(self, action, packageList=[]):
        if self.packageVersion > 1:
            return self.findCmd2(action, packageList)
        else:
            return self.findCmd1(action, packageList)

    def findCmd1(self, action, packageList=[]):
        fullCmd = ''
        extensions = [".py", ".bat", ".pl", ".sh"]
        cmds = {INSTALL: "installer", UNINSTALL: "uninstaller",
                VERIFY: "verify", CONFIGURE: "configure" }
        cmd = cmds.get(action)
        if not cmd:
            Logger.error("Unknown action: [%s]" % action)
            return FAIL
        for extension in extensions:
            testCmd = os.path.join(self.scriptsDir, cmd+extension)
            #Logger.debug( testCmd )
            if self.filesystem.isfile(testCmd):
                fullCmd = testCmd
                break
        if not fullCmd:
            Logger.error("Could not find an appropriate script in %s." % self.scriptsDir)
            return FAIL
        status = OK
        if packageList:
            fullCmd += " %s" % ','.join(packageList)

        status = self.operatingSystem.run(fullCmd, self.workingDir, self.console)
        return status

    def findCmd2(self, action, packageList=[]):
        cwd = self.filesystem.getcwd() # FIXME
        sys.path.append(self.scriptsDir)
        self.filesystem.chdir(self.scriptsDir)
        files = glob.glob("*.py")
        files = [x.split('.py')[0] for x in files]
        status = FAIL
        fileFound = False
        for file in files:  # FIXME this is stupid
            if file.split('.')[0] in ["installer", "verify", "uninstaller", "configure"]:
                continue
            try:
                self.filesystem.chdir(self.workingDir)
                letters = [ chr( x ) for x in range(65, 91) ]
                random.shuffle(letters)
                randString = ''.join(letters)
                exec("import %s as %s" % (file, randString)) # FIXME
                Logger.debug("This is package version %s" % self.packageVersion)
                if self.packageVersion == 2:
                    exec("obj = %s.%s(self.config)" % (randString, file))
                elif self.packageVersion == 3:
                    cmdString = "obj = %s.%s(self.config, packageList, Logger.logger)"
                    exec(cmdString % (randString, file))
                else:
                    Logger.error("Unknown package version %s" % self.packageVersion)
                fileFound = True
                if action == INSTALL:
                    status = obj.installer()
                elif action == VERIFY:
                    status = obj.verify()
                elif action == UNINSTALL:
                    status = obj.uninstaller()
                elif action == CONFIGURE:
                    status = obj.configure()
                else:
                    Logger.error("Invalid action specified: %s" % action)
                del randString
                break
            except ImportError:
                Logger.debug("File %s is not runnable. Looking for others" % file)
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
                Logger.error("Error detected in %s (%s)." % (file, e))
                e = StringIO.StringIO()
                traceback.print_exc(file=e)
                e.seek(0)
                data = e.read()
                ermsg = ''
                for line in data.split('\n'):
                    ermsg += "\n||>>>%s" % line
                Logger.error(ermsg)
                Logger.error("Error ocurred in %s" % file)
                fileFound = True
                status = FAIL
                del randString
                break
        self.filesystem.chdir(cwd)
        if not fileFound:
            Logger.error("Unable to find a suitable script to install.")
            return FAIL
        if status == REBOOT:
            erstr = "%s %s: indicated reboot action is necessary." % (self.fullName, status)
            Logger.warning(erstr)
            return REBOOT
        if status == REBOOT_AND_TRY_AGAIN:
            erstr = "%s %s: indicated reboot action is necessary before installing." % (self.fullName, status)
            Logger.warning(erstr)
            return PREBOOT
        if status != OK:
            erstr = "%s: failed with status %s" % (self.fullName, status)
            Logger.error(erstr)
            return FAIL
        return OK

    def download(self):
        if not self.downloaded:
            self.filesystem.updateCurrentAction("Downloading package...", 10, self.server)
            status = self.repository.getPackage(self.name, checksum=self.checksum)
            if status == FAIL:
                self.status = FAIL
                self.filesystem.warningLog("Problems downloading package %s" % self.name,
                                           self.server)
                return FAIL
            status = self.injector()
            self.downloaded = True
        return OK
    
    def process(self, packageList=[]):
        self.download()
        if self.status == FAIL:
            # FIXME: may be a good idea to re-download the package.
            erstr = "Package %s is corrupt or could not be "\
                    "downloaded." % self.fullName
            Logger.error(erstr)
            self.filesystem.warningLog(erstr, self.server)
            return FAIL
        if self.action == INSTALL:
            if self.autoReboot:
                Logger.info("This is an auto-reboot package. "\
                                 "Assuming package installs successfully.")
                self.operatingSystem.autoLogin(self.config)
                self.operatingSystem.restartOnLogon()
                self.writeProgress()
            self.install(packageList)
            Logger.info("Install result: %s" % self.status)
            if self.status == PREBOOT:
                return PREBOOT
            if self.status == OK:
                self.verify()
                Logger.info("Verify result: %s" % self.status)
            self.writeProgress()
            return self.status
        Logger.error("Unknown action: [%s]" % self.action)
        return FAIL

    def chdir(self):
        self.cwd = os.getcwd()
        #Logger.debug(" Changing directory to %s" % os.path.join(miniUtility.getSpkgPath(), PACKAGES))
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
    def install(self, packageList): 
        self.download()
        self.filesystem.updateCurrentAction("Installing...", 50, self.server)
        message = "Beginning installation of (%s)" % self.fullName
        Logger.info(message)
        self.preload()
        self.status = self.findCmd(INSTALL, packageList)
        return self.status
    
    # TESTED
    def verify(self): 
        self.download()
        self.filesystem.updateCurrentAction("Verifying...", 90, self.server)
        message = "Verifying package %s" % self.fullName
        Logger.info(message)
        self.status = self.findCmd(VERIFY)
        if self.action != INSTALL:
            self.writeProgress()
        return self.status

    # TESTED
    def uninstall(self):
        self.download()
        if self.status == FAIL:
            return FAIL
        Logger.info("Uninstalling package %s" % self.name)
        self.filesystem.updateCurrentAction("Uninstalling...", 70, self.server)
        self.status = self.findCmd(UNINSTALL)
        self.writeProgress()
        return self.status

    # TESTED
    def getDateString(self):
        d = datetime.datetime.today()
        dateString = "%d-%d-%d-%d-%d-%d" %(d.year, d.month, d.day, d.hour, d.minute, d.second ) 
        return( dateString )

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
        try:
            fh.write('\n'.join(output))
        except UnicodeEncodeError:
            Logger.error("Could not encode data: %s" % output)
        fh.flush()
        fh.close()
        del fh

    ### TESTED
    def writeProgress(self):
        self.filesystem.updateCurrentAction("Finished.", 100, self.server)

        timeString = time.ctime()
        progressData = self.filesystem.getProgressData()
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
