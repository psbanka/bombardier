#!/cygdrive/c/python24/python.exe

import sets, os, time
import ConfigParser, string
import miniUtility, Package, Exceptions, Logger
from staticData import *

class PackageChain:
    def __init__(self, priority, startPackageName, packages,
                 installedPackageNames, repository, config, 
                 filesystem, server, windows):
        self.priority   = priority
        self.packages   = packages
        self.filesystem = filesystem
        self.server     = server
        self.windows    = windows
        self.chain      = [startPackageName]
        self.repository = repository
        self.config     = config
        self.vPackages  = VirtualPackages(repository.packages)
        self.installedPackageNames = self.vPackages.resolveVPkgList( installedPackageNames )
        self.packageChain(startPackageName)


    ### TESTED
    def addToDependencyErrors(self, packageName, dependencyName):
        errmsg = "BOM file is incomplete: should contain %s" % dependencyName
        self.server.serverLog("WARNING", errmsg, packageName)
        bomPath = miniUtility.getBomPath()
        if self.filesystem.getAllFromFile(".*"+dependencyName, bomPath):
            return FAIL
        deps = ConfigParser.ConfigParser()
        depsPath = os.path.join(miniUtility.getSpkgPath(), BOM_DEPS)
        try:
            fh = self.filesystem.open(depsPath,"r")
            deps.readfp(fh)
        except IOError:
            errmsg = "Creating file %s to keep track "\
                     "of dependency errors" % ERRORS_FILE
            Logger.warning(errmsg)
        if not deps.has_section(packageName):
            deps.add_section(packageName)
        index=0
        while deps.has_option(packageName, "dep%s" % index):
            if deps.get(packageName, "dep%s" % index) == dependencyName:
                Logger.warning("We have already recorded that %s "\
                               "depends on %s" % (packageName, dependencyName))
                return OK
            index += 1
        deps.set(packageName, "dep%s" % index, dependencyName)
        fh = self.filesystem.open(depsPath,"w")
        deps.write(fh)
        fh.flush()
        fh.close()
        return OK

    # TESTED
    def packageChain(self, pkgName):
        for depName in self.packages[ self.getActualPkgName( pkgName ) ].dependencies:
            if depName not in self.installedPackageNames: 
                self.chain.insert( 0, self.getActualPkgName( depName ) )
            self.syncDependencies( depName, pkgName )
            self.priority = max( self.packages[self.getActualPkgName( depName )].priority, self.priority )
            self.packageChain( depName )

    def syncDependencies( self, depName, pkgName ):
        if depName not in ( self.vPackages.resolveVPkgList( self.installedPackageNames ) + \
                            self.vPackages.resolveVPkgList( self.packages.keys() ) ):
            self.packages[depName] = self.getNewPackage( depName )
            self.addToDependencyErrors(pkgName, depName)

    def getActualPkgName( self, pkgName ):
        return( self.vPackages.getActualPkgName( pkgName, self.packages.keys() ) )

    def getNewPackage( self, pkgName ):
        newPackage = Package.Package(pkgName, self.repository, self.config, 
                                     self.filesystem, self.server, self.windows) 
        return newPackage

class VirtualPackages:
    def __init__( self, packages ):
        self.packages = packages
        self.virtualPackagesDict = {}
        self.virtualNameLookupDict = {}
        self.initVPkgDictionaries()

    def initVPkgDictionaries( self ):
        for packageName in self.packages:
            package = self.packages[packageName]
            if package.get( 'dependencies' ) and package['dependencies'].get( 'virtualpackage' ):
                virtualPackage = package['dependencies']['virtualpackage'] 
                self.virtualNameLookupDict.setdefault( packageName, virtualPackage )
                self.virtualPackagesDict.setdefault( virtualPackage,[] ).append( packageName )

    def getPkgNameListFromVPkgName( self, virtualPackageName ):
        return( self.virtualPackagesDict.get( virtualPackageName, [] ) )

    def getVPkgNameFromPkgName( self, packageName ):
        return( self.virtualNameLookupDict.get( packageName, packageName ) )
    
    def resolveVPkgList( self, packageList ):
        return( [self.getVPkgNameFromPkgName( packageName ) for packageName in packageList] )

    def getActualPkgName( self, packageName, packageNames ):
        resolvedPackageNames = self.getPkgNameListFromVPkgName( packageName )
        pName = [actualName for actualName in resolvedPackageNames if actualName in packageNames] + [packageName]
        return( pName[0] ) 

def nop():
    return False

class Bombardier:

    def __init__(self, repository, config, 
                 filesystem, server, windows):
        self.repository = repository
        self.config     = config
        self.filesystem = filesystem
        self.server     = server
        self.windows    = windows
        self.testStop   = nop
        
    ### TESTED
    def dependenciesInstalled(self, bomPackageNames):
        installedDependencies = []
        deps = ConfigParser.ConfigParser()
        try:
            depsPath = os.path.join(miniUtility.getSpkgPath(), BOM_DEPS)
            fh = self.filesystem.open(depsPath,"r")
            deps.readfp(fh)
        except IOError:
            return []
        for predecessorPackageName in deps.sections():
            if predecessorPackageName in bomPackageNames:
                index=0
                while deps.has_option(predecessorPackageName, "dep%s" % index):
                    installedDependencies.append(deps.get(predecessorPackageName,
                                                          "dep%s" % index))
                    index += 1
        return installedDependencies

    ### WON'T BE TESTED
    def handleConsole(self, package):
        if self.windows.testConsole() == FAIL:
            self.filesystem.updateCurrentStatus(IDLE, "Rebooting for console")
            erstr = "Logging in for console access "\
                    "for package %s..." % (package.name)
            Logger.info(erstr)
            self.filesystem.clearLock()
            status = self.windows.autoLogin(self.config)
            if status == FAIL:
                ermsg = "Cannot gain console access because this system "\
                        "does not have valid login credentials."
                Logger.error(ermsg)
                self.filesystem.updateCurrentStatus(ERROR,"ERROR: Cannot log in")
                self.server.serverLog("CRITICAL", ermsg)
                return FAIL
            self.windows.restartOnLogon()
            self.windows.rebootSystem(message="Rebooting to gain console access")
        return OK

    ### WON'T BE TESTED
    def rebootForMoreInstallation(self, package, packages):
        self.filesystem.clearLock()
        if packages:
            erstr = "Setting system to auto-login "\
                    "to process the following %s" % `packages.keys()`
            Logger.info(erstr)
            status = self.windows.autoLogin(self.config)
            if status == FAIL:
                self.filesystem.updateCurrentStatus(ERROR,"ERROR: Cannot log in")
                ermsg = "Cannot continue installation because this "\
                        "system does not have valid login credentials."
                Logger.error(ermsg)
                self.server.serverLog("CRITICAL", ermsg)
                return FAIL
            self.windows.restartOnLogon()
        else:
            self.windows.noAutoLogin()
            self.windows.noRestartOnLogon()
        if package.autoReboot:
            self.filesystem.updateCurrentStatus(IDLE, "Waiting for reboot")
            erstr = "Waiting for package %s to reboot "\
                    "the system..." % (package.fullName)
            Logger.info(erstr)
        else:
            self.filesystem.updateCurrentStatus(IDLE, "Rebooting to continue install")
            self.windows.rebootSystem(message="Rebooting after installing %s" % package.fullName)
        return OK


    ### WON'T BE TESTED
    def checkReboot(self, package, packages):
        self.abortIfTold()
        if package.action == INSTALL and (package.reboot or package.autoReboot):
            if not packages:
                erstr = "Setting system NOT to auto-login "\
                        "because all packages are processed."
                Logger.info(erstr)
                self.filesystem.updateCurrentStatus(IDLE, "Idle")
                self.cleanup(OK, logmessage="Rebooting system to continue installation")
            self.rebootForMoreInstallation(package, packages)

    ### TESTED
    def inMaintenanceWindow(self):
        windowTime = ''
        # Window format : DayOfWeek-Abbrev HH:MM MM
        windowTime = self.config.get("system", "maintenanceWindow",default="NONE")
        if windowTime == "NONE":
            Logger.warning("Maintenance window is not set. "\
                           "All maintenance is allowed.")
            return True
        validDays = ["SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"]
        if len(windowTime.split()) != 3:
            self.filesystem.updateCurrentStatus(IDLE, "Invalid maintenance window. No installs")
            Logger.warning("Maintenance window [%s] should be of "\
                           "the format Day HH:MM MM." % windowTime)
            return False
        day, clock, duration = windowTime.split()
        if day.upper() not in validDays:
            Logger.warning("Maintenance window day [%s] should be one "\
                           "of %s" % (windowTime, `validDays`))
            return False
        if day.upper() == time.strftime("%a").upper():
            hour, minute = clock.split(':')
            try:
                hour     = int(hour)
                minute   = int(minute)
                duration = int(duration)
                assert hour >= 0
                assert hour <= 24
                assert minute >= 0
                assert minute <= 60
                assert duration >= 0
                assert duration <= 1440
            except (AssertionError, ValueError):
                erstr = "Improperly formatted maintenance window. [%s] No "\
                        "maintenance will occur." % windowTime
                Logger.warning(erstr)
                return False
            startMin = hour*24 + minute
            nowMin   = time.localtime()[3]*24 + time.localtime()[4]
            if nowMin - startMin >= 0: # must wait for it to open
                if (nowMin - startMin) < duration: # must be inside the window
                    return True
        return False

    # TESTED
    def createPackageChains(self, packages):
        chains = []
        installedPackageNames = self.filesystem.getPackagesFromFile(PROGRESS_FILE,
                                                               stripVersionFromName = 1)
        for packageName in packages.keys():
            if packageName not in installedPackageNames:
                chainPriority = packages[packageName].priority
            else:
                chainPriority = 0
            try:
                newChain = PackageChain(chainPriority, packageName, packages,
                                        installedPackageNames, self.repository,
                                        self.config, self.filesystem,
                                        self.server, self.windows)
            except Exceptions.BadPackage, e:
                errmsg = "Package %s will not be installed because it is "\
                         "dependent upon one or more broken packages:" % packageName
                Logger.warning(errmsg)
                Logger.warning(`e`)
                continue
            chains.append([newChain.priority, newChain.chain])
        return chains

    # TESTED
    def getTopPriority(self, chains):
        topPriority = 0
        for priority, chain in chains:
            if priority > topPriority:
                topPriority = priority
        return topPriority

    # TESTED
    def installList(self, packages):
        """ - Create chains of package dependencies, the priority of each
        chain is the package with the highest priority in it, unless
        that package is already installed.
        - A package can appear in more than one chain
        - A package chain can have a single package in it if it does not
        have any dependencies and is not dependent on others."""

        chains = self.createPackageChains(packages)
        installedPackageNames = self.filesystem.getPackagesFromFile(PROGRESS_FILE,
                                                               stripVersionFromName = 1)
        # - Put all the packages of each chain into the installation
        # order, excluding those that have already been installed in order
        # of chain priority. If a package is already in the installation
        # List, do not put it in twice.
        installOrder = []
        while chains:
            topPriority = self.getTopPriority(chains)
            for priority, chain in chains:
                if priority == topPriority:
                    for packageName in chain:
                        if packageName not in installedPackageNames:
                            if packageName not in installOrder:
                                installOrder.append(packageName)
                    chains.remove([priority, chain])
        return installOrder # returns a list of packageNames in the correct installation order

    def getDetailedTodolist(self, installList):
        todo = {}
        pkgGroups  = self.config.getPackageGroups()
        for packageName in installList:
            todo[packageName] = []
            for groupName in pkgGroups:
                groupPackages = self.filesystem.getPackagesFromFile(groupName+".BOM")
                if packageName in groupPackages:
                    todo[packageName].append(groupName)
        output = []
        for packageName in todo.keys():
            if len(todo[packageName]) == 0:
                output.append("%s,<<dependency>>" % (packageName))
            else:
                output.append("%s,%s" % (packageName, "/".join(todo[packageName])))
        return output

    # TESTED
    def installPackages(self, packages):
        self.filesystem.updateCurrentStatus(INSTALLING, "Installing packages")
        installList = self.installList(packages)
        detailedTodos = self.getDetailedTodolist(installList)
        self.filesystem.updateProgressFile({"todo": detailedTodos}, True)
        for packageName in installList:
            self.abortIfTold()
            package = packages[packageName]
            self.filesystem.updateProgressFile({"status": {"package":packageName}})
            erstr = "Currently installing package "\
                    "priority %s [%s]" % (package.priority, packageName)
            Logger.info(erstr)
            if package.console:
                if self.handleConsole(package) == FAIL:
                    return FAIL
            if package.status == FAIL:
                Logger.warning("Package could not download. Giving up.")
                return FAIL
            if package.preboot:
                Logger.warning("Package %s wants the system to "\
                               "reboot before installing..." % package)
                self.rebootForMoreInstallation(package, packages)
            status = package.process(self.abortIfTold, installList)
            if status == REBOOT:
                Logger.warning("Package %s wants the system to "\
                               "reboot after installing..." % package)
                self.rebootForMoreInstallation(package, packages)
            if status == FAIL:
                erstr = "B11 Aborting due to package "\
                        "installation failure"
                Logger.error(erstr)
                return FAIL
            self.checkReboot(package, packages)
        return OK

    def uninstallPackages(self, packages):
        self.filesystem.updateCurrentStatus(INSTALLING, "Un-installing packages")
        for packageName in packages.keys():
            self.abortIfTold()
            package = packages[packageName]
            erstr = "Currently removing package "\
                    "priority %s [%s]" % (package.priority, packageName)
            Logger.info(erstr)
            if package.console:
                if self.handleConsole(package) == FAIL:
                    return FAIL
            status = package.uninstall(self.abortIfTold)
            if status == REBOOT:
                self.abortIfTold()
                Logger.warning("Package %s wants the system to "\
                               "reboot after uninstalling..." % package)
                self.rebootForMoreInstallation(package, packages)
            if status == FAIL:
                erstr = "B11 Aborting due to package "\
                        "uninstallation failure"
                Logger.error(erstr)
                return FAIL
            self.checkReboot(package, packages)
        return OK

    ### WON'T BE TESTED
    def cleanup(self, status, logmessage=''):
        self.windows.noRestartOnLogon()
        self.windows.noAutoLogin()
        if status == FAIL:
            if logmessage:
                Logger.error(logmessage)
                self.server.serverLog("ERROR", logmessage)
            return FAIL
        else: # Controversial! Should you unlock anyway?
            if logmessage:
                Logger.info(logmessage)
                self.server.serverLog("INFO", logmessage)
            self.filesystem.clearLock()
        if self.config.automated:
            Logger.info("Rebooting after auto-logon...")
            self.abortIfTold()
            self.windows.rebootSystem(message = "Final reboot after package installation")
        return OK

    ### TESTED
    def getPackagesToAdd(self, addPackageNames):
        packages = {}
        for packageName in addPackageNames:
            try:
                newPackage = Package.Package(packageName, self.repository,
                                             self.config, self.filesystem,
                                             self.server, self.windows)
                packages[packageName] = newPackage
            except Exceptions.BadPackage, e:
                errmsg = "Skipping %s" % `e`
                Logger.warning(errmsg)
                self.server.serverLog("ERROR", errmsg, section=packageName)
        return packages

    ### TESTED
    def getPackagesToRemove(self, delPackageNames):
        vPackages = VirtualPackages(self.repository.packages)
        packages = {}
        installedPackageNames = self.filesystem.getPackagesFromFile(PROGRESS_FILE,
                                                               stripVersionFromName = 1)
        # create package objects for all the packages
        # in the list that we want to remove
        for packageName in delPackageNames:
            # FIXME: Can't remove packages that were installed
            # as a result of being a dependency of another package
            Logger.info("Scheduling package %s for removal because it is "\
                        "not on the bill of materials." % packageName)
            packages[packageName] = Package.Package(packageName, self.repository,
                                                    self.config, self.filesystem,
                                                    self.server, self.windows)
            packages[packageName].action = UNINSTALL
        if sets.Set(installedPackageNames) == sets.Set(packages.keys()):
            Logger.info("all packages are being removed.")
            return packages
        # add any packages that are installed already
        # which are dependent upon those to the list as well
        while delPackageNames:
            newDependencyNames = []
            delPackageNames = []
            for packageName in installedPackageNames:
                Logger.info("checking dependencies of %s" % packageName)
                if packageName in packages.keys():
                    Logger.debug("Package %s will already be deleted -- "\
                                 "ignoring" % packageName)
                    continue # already know that one will be deleted
                package = Package.Package(packageName, self.repository, self.config, 
                                          self.filesystem, self.server, self.windows)
                package.action = UNINSTALL
                for tombstonedPackageName in packages.keys():
                    if vPackages.getVPkgNameFromPkgName( tombstonedPackageName )in package.dependencies:
                        erstr = "Scheduling package %s for removal "\
                                "because it depends on %s" % \
                                (packageName, tombstonedPackageName)
                        Logger.info(erstr)
                        packages[tombstonedPackageName].dependsOnMe.append(packageName)
                        if packageName not in packages.keys():
                            if packageName not in newDependencyNames:
                                packages[packageName] = package
                                newDependencyNames.append(packageName)
                delPackageNames = newDependencyNames
        return packages

    def writeSystemType(self, pkgGroups):
        spkgPath = miniUtility.getSpkgPath()
        systemTypePath = os.path.join(spkgPath, SYSTEM_TYPE_FILE)
        stf = self.filesystem.open(systemTypePath, 'w')
        Logger.info("Writing system type to file %s" % systemTypePath)
        stf.write(string.join(pkgGroups,'|'))
        stf.close()

    ### TESTED
    def downloadBom(self, pkgGroups):
        if type(pkgGroups) != type(["list"]):
            ermsg = "Invalid input to function. Should be a list of strings, got: %s" % pkgGroups
            Logger.error(ermsg)
            return FAIL
        self.filesystem.updateCurrentAction("Downloading Bill of Materials...", 0)
        spkgPath = miniUtility.getSpkgPath()
        self.writeSystemType(pkgGroups)

        if pkgGroups == []:
            if self.windows.testConsole(): # Try to pull up the WebUI
                hostName = os.environ["COMPUTERNAME"]
                configUrl = "%s/website/client/clientpackages?"\
                            "client=%s" % (self.config.repository["address"],hostName)
                self.windows.ShellExecuteSimple(configUrl)
                self.filesystem.clearLock()
        packageNames = sets.Set([])
        for pkgGroup in pkgGroups:
            pkgString = self.server.serviceRequest("pkggroups", args={"group":pkgGroup})
            if pkgString == '':
                ermsg = "Package group %s does not exist on the repository (ignoring)" % pkgGroup
                Logger.warning(ermsg)
                continue
            newPackageNames = sets.Set(pkgString.split())
            packageNames = packageNames.union(newPackageNames)
        if len(list(packageNames)) == 0:
            ermsg = "No packages configured for this system"
            self.filesystem.updateCurrentStatus(ERROR, "System does not have a Bill of Materials")
            self.server.serverLog("CRITICAL", ermsg)
            Logger.error(ermsg)
            return FAIL
        fh = self.filesystem.open(os.path.join(spkgPath, BOM_FILE), 'w')
        fh.write("\n".join(list(packageNames)))
        fh.flush()
        fh.close()
        return OK

    def checkBom(self):
        """ Check through what should be installed on the system and what
        is installed on the system and determine what packages aren't
        installed that should be and what packages are installed that
        shouldn't be."""

        shouldBeInstalled = []
        shouldntBeInstalled = []
        bomPackageNames = self.filesystem.getPackagesFromFile(BOM_FILE)
        installed = self.filesystem.getPackagesFromFile(PROGRESS_FILE,
                                                   stripVersionFromName = 1)
        dependencyErrors = self.dependenciesInstalled(bomPackageNames)
        errmsg = "The following packages are installed as "\
                 "dependencies %s" % dependencyErrors
        Logger.debug(errmsg)
        bomPackageNames += dependencyErrors
        for package in installed:
            if package not in bomPackageNames:
                shouldntBeInstalled.append(package)
        for package in bomPackageNames:
            if package not in installed:
                shouldBeInstalled.append(package)
        return shouldBeInstalled, shouldntBeInstalled

    def verifySystem(self, testStop):
        self.testStop = testStop

        pkgList = self.filesystem.getPackagesFromFile(PROGRESS_FILE, stripVersionFromName=True)
        progress = self.filesystem.getProgressData()

        testResults = {}

        for pkg in pkgList:
            try:
                package = Package.Package(pkg, self.repository, self.config,
                                          self.filesystem, self.server, self.windows)
            except Exceptions.BadPackage:
                errmsg = "Not testing %s because the server doesn't know about it" % pkg
                Logger.warning(errmsg)
                self.server.serverLog("WARNING", errmsg, pkg)
                continue
            interval = package.metaData.get('verify','verifyInterval', VERIFY_INTERVAL)

            Logger.info("Trying to verify %s" % package.name) 

            if not progress.has_key(package.fullName):
                return None
            timeString = progress[package.fullName]['VERIFIED']
            timer = time.mktime(time.strptime(timeString))

            if timer + interval <= time.time():
                package.action = VERIFY
                testResults[pkg] = package.verify(self.abortIfTold)

        return testResults

    def abortIfTold(self):
        if self.testStop():
            ermsg = "Bombardier received a message to stop. Aborting all operations."
            Logger.warning(ermsg)
            self.filesystem.updateCurrentStatus(IDLE, "Aborted last action")
            raise Exceptions.StoppedExecution

    ### TESTED
    def reconcileSystem(self, testStop, testing=False):
        self.testStop = testStop
        spkgPath = miniUtility.getSpkgPath()
        pkgGroups  = self.config.getPackageGroups()
        if not self.inMaintenanceWindow():
            erstr = "Currently a maintenance window: no activity will take place"
            Logger.warning(erstr)
            return self.cleanup(FAIL)
        if self.filesystem.setLock() == FAIL:
            return self.cleanup(FAIL)
        self.abortIfTold()
        self.filesystem.updateCurrentStatus(IDLE, "Initializing")
        self.filesystem.chdir(spkgPath)
        if not testing:
            Logger.debug("Downloading BOM")
            status = self.downloadBom(pkgGroups)
            if status == FAIL:
                errmsg = "Aborting installation -- cannot download bill of materials"
                Logger.error(errmsg)
                return self.cleanup(OK, logmessage="Problem with the Bill of Materials")
        if not self.filesystem.isfile(miniUtility.getBomPath()):
            Logger.error("Aborting installation -- cannot find a bill of materials")
            return self.cleanup(OK, logmessage="Problem with the Bill of Materials")
        self.abortIfTold()
        addPackageNames, delPackageNames = self.checkBom()
        addPackages = self.getPackagesToAdd(addPackageNames)
        try:
            delPackages = self.getPackagesToRemove(delPackageNames)
        except Exceptions.BadPackage, e:
            logmessage = "Halting: there are packages installed that cannot be removed"
            logmessage += `e`
            self.cleanup(OK, logmessage)
        self.abortIfTold()
        Logger.info("Packages to install: %s" % addPackages.keys())
        Logger.info("Packages to remove: %s" % delPackages.keys())
        status = self.uninstallPackages(delPackages)
        if status == FAIL:
            errmsg = "Uninstallation failed. Not continuing with "\
                     "installation of %s" % addPackages
            Logger.error(errmsg)
            return self.cleanup(FAIL, logmessage="Finished installing.")
        self.abortIfTold()
        addPackageNames, delPackageNames = self.checkBom()
        self.abortIfTold()
        addPackages = self.getPackagesToAdd(addPackageNames)
        status = self.installPackages(addPackages)
        return self.cleanup(status, logmessage="Finished installing.")

if __name__ == "__main__":
    message =  """NOTE: This program is no longer
used to run the bombardier client from the command
line. Please use 'bc.py' instead."""
    print message

