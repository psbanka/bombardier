#!/cygdrive/c/Python24/python.exe

# BombardierClass.py: This is the central focal point of most of the
# activities of this software. The reconcileSystem method kicks off
# all activity for uninstalling and installing modules. The
# verifySystem method kicks off a verification round of activities.

# Copyright (C) 2005 Peter Banka, Shawn Sherwood, Mark Hickman

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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

import sets, os, time
import ConfigParser, string
import miniUtility, Package, Exceptions, Logger
from staticData import *

class PackageChain:
    def __init__(self, priority, startPackageName, packages,
                 installedPackageNames, brokenPackageNames, repository,
                 config, filesystem, server, operatingSystem):
        self.depth       = 0
        self.priority    = priority
        self.packages    = packages
        self.filesystem  = filesystem
        self.server      = server
        self.operatingSystem    = operatingSystem
        self.chain       = [startPackageName]
        self.repository  = repository
        self.config      = config
        self.vPackages   = VirtualPackages(repository.packages)
        self.installedPackageNames = self.vPackages.resolveVPkgList( installedPackageNames )
        self.brokenPackageNames = brokenPackageNames
        self.packageChain(startPackageName)
        self.filterBadPackages()

    def filterBadPackages(self):
        for pkgIndex in range(0, len(self.chain)):
            packageName = self.chain[pkgIndex]
            if packageName in self.brokenPackageNames:
                pkgIndex -= 1
                Logger.warning("Omitting packages %s due to bad package" % (self.chain[pkgIndex:]))
                break
        self.chain = self.chain[:pkgIndex+1]

    ### TESTED
    def addToDependencyErrors(self, packageName, dependencyName):
        errmsg = "BOM file is incomplete: should contain %s" % dependencyName
        self.filesystem.warningLog(errmsg, self.server)
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
        self.depth += 1
        if self.depth > MAX_CHAIN_DEPTH:
            raise Exceptions.DependencyLoopException(self.chain)
        for depName in self.packages[ self.getActualPkgName( pkgName ) ].dependencies:
            if depName not in self.installedPackageNames: 
                self.chain.insert( 0, self.getActualPkgName( depName ) )
            else:
                continue
            self.syncDependencies( depName, pkgName )
            dependency = self.packages.get(self.getActualPkgName( depName ))
            if depName in self.brokenPackageNames:
                self.priority = 0
            else:
                self.priority = max( dependency.priority, self.priority )
            self.packageChain( depName )
        self.depth -= 1

    def syncDependencies( self, depName, pkgName ):
        if depName not in ( self.vPackages.resolveVPkgList( self.installedPackageNames ) + \
                            self.vPackages.resolveVPkgList( self.packages.keys() ) ):
            self.packages[depName] = self.getNewPackage( depName )
            self.addToDependencyErrors(pkgName, depName)

    def getActualPkgName( self, pkgName ):
        return( self.vPackages.getActualPkgName( pkgName, self.packages.keys() ) )

    def getNewPackage( self, pkgName ):
        newPackage = Package.Package(pkgName, self.repository, self.config, 
                                     self.filesystem, self.server, self.operatingSystem) 
        newPackage.initialize()
        return newPackage

class VirtualPackages:
    def __init__( self, packageDict ):
        self.packageDict = packageDict
        self.virtualPackagesDict = {}
        self.virtualNameLookupDict = {}
        self.initVPkgDictionaries()

    def initVPkgDictionaries( self ):
        for packageName in self.packageDict:
            packageData = self.packageDict[packageName]
            if packageData.get( 'virtualpackage' ):
                virtualPackageName = packageData['virtualpackage'] 
                self.virtualNameLookupDict.setdefault( packageName, virtualPackageName )
                self.virtualPackagesDict.setdefault( virtualPackageName,[] ).append( packageName )

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

def findDifferences(packageConfig, configDiff, differences=[], chain=[]):
    for key in packageConfig.keys():
        if type(packageConfig[key]) == type({}):
            if key not in configDiff:
                continue
            newdif = findDifferences(packageConfig[key], configDiff[key],
                                     differences, chain + [key])
            continue
        if key in configDiff.keys():
            differences.append("%s/%s" % ('/'.join(chain), key))
            continue
    return differences

class Bombardier:

    def __init__(self, repository, config, 
                 filesystem, server, operatingSystem):
        self.repository = repository
        self.config     = config
        self.filesystem = filesystem
        self.server     = server
        self.operatingSystem    = operatingSystem
        self.addPackages = {}
        self.delPackages = {}
        
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
        if self.operatingSystem.testConsole() == FAIL:
            erstr = "Logging in for console access "\
                    "for package %s..." % (package.name)
            Logger.info(erstr)
            self.filesystem.clearLock()
            status = self.operatingSystem.autoLogin(self.config)
            if status == FAIL:
                ermsg = "Cannot gain console access because this system "\
                        "does not have valid login credentials."
                Logger.error(ermsg)
                self.filesystem.warningLog(ermsg, self.server)
                return FAIL
            self.operatingSystem.restartOnLogon()
            msg = "System requires a console to install this package and one does not exist. " \
                  "Please log in to continue the installation."
            Logger.warning(msg)
            return REBOOT
        return OK

    ### WON'T BE TESTED
    def checkReboot(self, package, returnCode):
        if package.action == INSTALL:
            if package.autoReboot:
                erstr = "Waiting for package %s to reboot "\
                        "the system..." % (package.fullName)
                Logger.info(erstr)
                while True:
                    time.sleep(1)
            if package.reboot or returnCode == REBOOT:
                self.filesystem.clearLock()
                msg = "The last package installed indicated that a reboot is necessary before continuing. " \
                      "Please reboot the system before performing more installation activity."
                Logger.warning(msg)
                return REBOOT

        if not self.addPackages:
            erstr = "System has been reconciled."
            Logger.info(erstr)
            return self.cleanup(OK, logmessage="Exiting gracefully.")

    ### TESTED
    def inMaintenanceWindow(self):
        windowTime = ''
        # Window format : DayOfWeek-Abbrev HH:MM MM
        windowTime = self.config.get("system", "maintenanceWindow",default="NONE")
        if windowTime == "NONE":
            return True
        validDays = ["SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"]
        if len(windowTime.split()) != 3:
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

    def printChains(self, chainTuples):
        ordinal = 0
        for chainTuple in chainTuples:
            priority, chain = chainTuple
            print "CHAIN %s; priority: %s; items: %s" % (ordinal, priority, chain)
            Logger.info( "CHAIN %s; priority: %s; items: %s" % (ordinal, priority, chain))
            ordinal += 1

    # TESTED
    def createPackageChains(self, packageDict):
        chains = []
        packageData = self.filesystem.getProgressData(stripVersionFromName = True)
        installedPackageNames, brokenPackageNames = miniUtility.getInstalled(packageData)
        for packageName in packageDict.keys():
            if packageName in brokenPackageNames:
                Logger.warning("Skipping broken package %s" % packageName)
                continue
            if packageName not in installedPackageNames:
                chainPriority = packageDict[packageName].priority
            else:
                chainPriority = 0
            try:
                newChain = PackageChain(chainPriority, packageName, packageDict,
                                        installedPackageNames, brokenPackageNames,
                                        self.repository,
                                        self.config, self.filesystem,
                                        self.server, self.operatingSystem)
            except Exceptions.BadPackage, e:
                errmsg = "Package %s will not be installed because it is "\
                         "dependent upon one or more broken packages" % packageName
                Logger.warning(errmsg)
                Logger.warning(`e`)
                continue
            chains.append([newChain.priority, newChain.chain])
        #self.printChains(chains)
        return chains

    # TESTED
    def getTopPriority(self, chains):
        topPriority = 0
        for priority, chain in chains:
            if priority > topPriority:
                topPriority = priority
        return topPriority

    # TESTED
    def installList(self, packageDict):
        """ - Create chains of package dependencies, the priority of each
        chain is the package with the highest priority in it, unless
        that package is already installed.
        - A package can appear in more than one chain
        - A package chain can have a single package in it if it does not
        have any dependencies and is not dependent on others."""

        chains = self.createPackageChains(packageDict)
        progressData = self.filesystem.getProgressData(stripVersionFromName = True)
        installedPackageNames, brokenPackageNames = miniUtility.getInstalled(progressData)
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

    def preboot(self, packageName):
        Logger.warning("Package %s wants the system to "\
                       "reboot before installing..." % packageName) # FIXME
        errmsg = "Rebooting prior to installing package %s" % packageName
        self.filesystem.clearLock()
        status = self.operatingSystem.autoLogin(self.config)
        self.operatingSystem.restartOnLogon()
        msg = "The current package requires a reboot before it can install. " \
              "Please reboot the system before installing it."
        Logger.warning(msg)
        return REBOOT

    # TESTED
    def installPackages(self):
        makingProgress = True
        packageNamesLeft = ['initialize']
        while makingProgress and packageNamesLeft:
            makingProgress = False
            installList = self.installList(self.addPackages)
            packageNamesLeft = []
            [ packageNamesLeft.append(x) for x in installList ]
            for packageName in installList:
                Logger.info("Packages remaining to install (in order): %s" % packageNamesLeft)
                packageNamesLeft.remove(packageName)
                package = self.addPackages[packageName]
                erstr = "Currently installing package "\
                        "priority %s [%s]" % (package.priority, packageName)
                Logger.info(erstr)
                if package.console:
                    if self.handleConsole(package) == FAIL:
                        return FAIL
                if package.status == FAIL:
                    Logger.warning("Package could not download. Giving up.")
                    return FAIL
                if package.preboot and not self.config.freshStart:
                    if self.preboot(package.name) == REBOOT:
                        return REBOOT
                status = package.process(packageNamesLeft)
                hashPath = os.path.join(miniUtility.getPackagePath(), package.fullName, HASH_FILE)
                #Logger.info("writing configuration fingerprint to %s" % hashPath)
                self.config.saveHash(hashPath)
                self.config.freshStart = False
                if status == PREBOOT:
                    if self.preboot(package.name) == REBOOT:
                        return REBOOT
                if self.checkReboot(package, status) == REBOOT:
                    return REBOOT
                if status == FAIL:
                    erstr = "Package installation failure -- re-calculating package installation order"
                    Logger.error(erstr)
                    break
                else:
                    makingProgress = True
        if packageNamesLeft:
            Logger.error("There are packages that are broken, and we have done all we can do.")
            return FAIL
        return OK

    def uninstallPackages(self, packages):
        for packageName in packages.keys():
            package = packages[packageName]
            erstr = "Currently removing package "\
                    "priority %s [%s]" % (package.priority, packageName)
            Logger.info(erstr)
            if package.console:
                if self.handleConsole(package) == FAIL:
                    return FAIL
            status = package.uninstall()
            self.checkReboot(package, status)
            if status == FAIL:
                erstr = "B11 Aborting due to package "\
                        "uninstallation failure"
                Logger.error(erstr)
                return FAIL
        return OK

    ### WON'T BE TESTED
    def cleanup(self, status, logmessage=''):
        self.operatingSystem.noRestartOnLogon()
        self.operatingSystem.noAutoLogin()
        self.filesystem.clearLock()
        if status == FAIL:
            if logmessage:
                Logger.error(logmessage)
                self.filesystem.warningLog(logmessage, self.server)
            return status
        Logger.info(logmessage)
        return status

    def checkConfiguration(self, package):
        if package.metaData.has_key("configuration"):
            requiredConfigs = package.metaData["configuration"]
            diff = miniUtility.diffDicts(requiredConfigs, self.config.data)
            if diff != {}:
                errmsg = "This machine does not have sufficient "\
                         "configuration data to install %s " % package.name
                diffTxt = ''
                for key in diff:
                    diffTxt += "key: %s; value: %s" % (key, diff[key])
                Logger.warning(errmsg)
                Logger.warning(diffTxt)
                return FAIL
        return OK

    ### TESTED
    def getPackagesToAdd(self, addPackageNames):
        packages = {}
        for packageName in addPackageNames:
            try:
                newPackage = Package.Package(packageName, self.repository,
                                             self.config, self.filesystem,
                                             self.server, self.operatingSystem)
                newPackage.initialize()
                if self.checkConfiguration(newPackage) == FAIL:
                    raise Exceptions.BadPackage(newPackage.name, "Bad Config")
                packages[packageName] = newPackage
            except Exceptions.BadPackage, e:
                errmsg = "Skipping bad package %s: %s" % (e.packageName, e.errmsg)
                Logger.warning(errmsg)
                self.filesystem.warningLog(errmsg, self.server)
        return packages

    def createPackageDict(self, packageList, action):
        packageDict = {}
        for packageName in packageList:
            if action == UNINSTALL:
                Logger.info("Package %s is "\
                            "not on the bill of materials." % packageName)
            try:
                newPackage = Package.Package(packageName, self.repository,
                                             self.config, self.filesystem,
                                             self.server, self.operatingSystem)
                newPackage.initialize()
            except Exceptions.BadPackage, e:
                errmsg = "Skipping Bad package: %s" % `e`
                Logger.warning(errmsg)
                self.filesystem.warningLog(errmsg, self.server)
            newPackage.action = action
            packageDict[packageName] = newPackage
        return packageDict

    def getUninstallPackageDependencies(self, packageDict, delPackageNames, installedPackageNames):
        # add any packages that are installed already
        # which are dependent upon those to the list as well
        vPackages = VirtualPackages(self.repository.packages)
        while delPackageNames:
            newDependencyNames = []
            delPackageNames = []
            for packageName in installedPackageNames:
                #Logger.info("checking dependencies of %s" % packageName)
                if packageName in packageDict.keys():
                    Logger.debug("Package %s will already be deleted -- "\
                                 "ignoring" % packageName)
                    continue
                package = Package.Package(packageName, self.repository, self.config, 
                                          self.filesystem, self.server, self.operatingSystem)
                package.initialize()
                package.action = UNINSTALL
                for tombstonedPackageName in packageDict.keys():
                    if vPackages.getVPkgNameFromPkgName( tombstonedPackageName ) in package.dependencies:
                        erstr = "Scheduling package %s for removal "\
                                "because it depends on %s" % \
                                (packageName, tombstonedPackageName)
                        Logger.info(erstr)
                        packageDict[tombstonedPackageName].dependsOnMe.append(packageName)
                        if packageName not in packageDict.keys():
                            if packageName not in newDependencyNames:
                                packageDict[packageName] = package
                                newDependencyNames.append(packageName)
                delPackageNames = newDependencyNames
        return packageDict

    ### TESTED
    def getPackagesToRemove(self, delPackageNames):
        progressData = self.filesystem.getProgressData(stripVersionFromName = True)
        installedPackageNames, brokenPackageNames = miniUtility.getInstalled(progressData)
        packageDict = self.createPackageDict(delPackageNames, UNINSTALL)
        if sets.Set(installedPackageNames) == sets.Set(packageDict.keys()):
            return packageDict
        packageDict = self.getUninstallPackageDependencies(packageDict,
                                                           delPackageNames,
                                                           installedPackageNames)
        return packageDict

    def writeSystemType(self, pkgGroups):
        spkgPath = miniUtility.getSpkgPath()
        systemTypePath = os.path.join(spkgPath, SYSTEM_TYPE_FILE)
        stf = self.filesystem.open(systemTypePath, 'w')
        stf.write(string.join(pkgGroups,'|'))
        stf.close()

    ### TESTED
    def downloadBom(self, pkgGroups):
        Logger.info("configured for the following package groups: %s" % pkgGroups)
        if type(pkgGroups) != type(["list"]):
            ermsg = "Invalid input to function. Should be a list of strings, got: %s" % str(pkgGroups)
            raise Exceptions.BadBillOfMaterials(ermsg)
        self.writeSystemType(pkgGroups)
        if pkgGroups == []:
            Logger.warning("No package groups configured for this system")
            return []
        packageNames = sets.Set([])
        for pkgGroup in pkgGroups:
            try:
                newPackageNames = self.server.bomRequest(pkgGroup)
            except Exceptions.FileNotFound:
                Logger.warning("Package Group %s was not found on the server." % pkgGroup)
                continue
            if newPackageNames == []:
                ermsg = "Package group %s does not exist on the repository (ignoring)" % pkgGroup
                Logger.warning(ermsg)
                continue
            packageNames = packageNames.union(set(newPackageNames))
        if len(list(packageNames)) == 0:
            ermsg = "No packages configured for this system"
            self.filesystem.warningLog(ermsg, self.server)
            Logger.error(ermsg)
            raise Exceptions.BadBillOfMaterials(ermsg)
        return list(packageNames)

    def checkBom(self, bomPackageNames):
        """ Check through what should be installed on the system and what
        is installed on the system and determine what packages aren't
        installed that should be and what packages are installed that
        shouldn't be."""
        shouldBeInstalled = []
        shouldntBeInstalled = []
        progressData = self.filesystem.getProgressData(stripVersionFromName = True)
        installedPackageNames, brokenPackageNames = miniUtility.getInstalled(progressData)
        dependencyErrors = self.dependenciesInstalled(bomPackageNames)
        if dependencyErrors:
            errmsg = "The following packages are installed as "\
                     "dependencies %s" % dependencyErrors
            Logger.debug(errmsg)
        bomPackageNames += dependencyErrors
        for packageName in installedPackageNames:
            if packageName not in bomPackageNames:
                shouldntBeInstalled.append(packageName)
        for packageName in bomPackageNames:
            if packageName not in installedPackageNames:
                shouldBeInstalled.append(packageName)
        return shouldBeInstalled, shouldntBeInstalled

    def verifySystem(self):
        self.server.clearCache()
        progressData = self.filesystem.getProgressData()
        installedPackageNames, brokenPackageNames = miniUtility.getInstalled(progressData)
        testResults = {}

        for fullPackageName in installedPackageNames:
            try:
                shortPackageName = miniUtility.stripVersion(fullPackageName)
                package = Package.Package(shortPackageName, self.repository, self.config,
                                          self.filesystem, self.server, self.operatingSystem)
                package.initialize()
            except Exceptions.BadPackage, e:
                errmsg = "Not testing %s (%s)" % (fullPackageName, e)
                Logger.warning(errmsg)
                self.filesystem.warningLog(errmsg, self.server)
                continue
            
            interval = package.metaData.get('verify','verifyInterval', VERIFY_INTERVAL)

            if not progressData.has_key(package.fullName): # don't verify if the package isn't installed
                return {}
            Logger.info("Trying to verify %s" % package.name) 
            timeString = progressData[package.fullName]['VERIFIED']
            timer = time.mktime(time.strptime(timeString))

            if timer + interval <= time.time():
                package.action = VERIFY
                package.verify()
                testResults[shortPackageName] = package.status

        return testResults

    def checkInstallationStatus(self, packageNames = None):
        self.packageNames = packageNames
        spkgPath          = miniUtility.getSpkgPath()
        self.server.clearCache()
        pkgGroups,individualPackageNames  = self.config.getPackageGroups()
        Logger.info("package groups: %s, individuals: %s" % (pkgGroups, individualPackageNames)) #FIXME
        if not self.inMaintenanceWindow():
            erstr = "Currently a maintenance window: no activity will take place"
            Logger.warning(erstr)
            return self.cleanup(FAIL)
        self.filesystem.chdir(spkgPath)
        if self.packageNames == None:
            Logger.info("Downloading Bill of Materials")
            self.packageNames = self.downloadBom(pkgGroups)
            self.packageNames += individualPackageNames
        if self.packageNames == []:
            self.filesystem.clearLock()
            raise Exceptions.BadBillOfMaterials("Empty Bill of Materials")
        addPackageNames, delPackageNames = self.checkBom(self.packageNames)
        self.addPackages = self.getPackagesToAdd(addPackageNames)
        try:
            self.delPackages = self.getPackagesToRemove(delPackageNames)
        except Exceptions.BadPackage, e:
            logmessage = "Halting: there are packages installed that cannot be removed"
            logmessage += `e`
            return self.cleanup(OK, logmessage)
        Logger.info("Packages to install: %s" % self.addPackages.keys())
        removeFullPackageNames = [self.delPackages[x].fullName for x in self.delPackages.keys()]
        Logger.info("Packages to remove: %s" % removeFullPackageNames)
        return OK

    ### TESTED
    def reconcileSystem(self, packageNames = None):
        status = self.checkInstallationStatus(packageNames)
        if status != OK:
            return self.cleanup(status)
        if self.filesystem.setLock() == FAIL:
            return FAIL
        if self.delPackages.keys():
            status = self.uninstallPackages(self.delPackages)
        if status == FAIL:
            errmsg = "Uninstallation failed. Not continuing with "\
                     "installation of %s" % self.addPackages
            Logger.error(errmsg)
            return self.cleanup(FAIL, logmessage="Finished installing (ERRORS ENCOUNTERED).")
        addPackageNames, delPackageNames = self.checkBom(self.packageNames)
        self.getPackagesToAdd(addPackageNames)
        status = self.installPackages()
        return self.cleanup(status, logmessage="Finished installing.")

    def checkSystem(self, packageNames = None):
        if self.checkInstallationStatus(packageNames) != OK:
            return FAIL
        progressData = self.filesystem.getProgressData(stripVersionFromName = True)
        fullProgressData = self.filesystem.getProgressData(stripVersionFromName = False)
        fullInstalledPackageNames, fullBrokenPackageNames = miniUtility.getInstalled(fullProgressData)
        Logger.info("packages that are installed: %s" % ' '.join(fullInstalledPackageNames))
        installedPackageNames, brokenPackageNames = miniUtility.getInstalled(progressData)
        shouldBeInstalled, shouldntBeInstalled = self.checkBom(self.packageNames)
        # check the configuration for each installed package
        packageInfo = {"ok":installedPackageNames,
                       "reconfigure": {},
                       "not-installed": shouldBeInstalled,
                       "remove": shouldntBeInstalled,
                       "broken": brokenPackageNames}
        for packageName in shouldntBeInstalled:
            packageInfo["ok"].remove(packageName)
        for packageName in installedPackageNames:
            newPackage = Package.Package(packageName, self.repository, self.config, 
                                         self.filesystem, self.server, self.operatingSystem) 
            newPackage.initialize()
            packageConfig = newPackage.getConfiguration()
            configHashPath = os.path.join(miniUtility.getSpkgPath(), "packages",
                                          newPackage.fullName, HASH_FILE)
            configDiff = self.config.checkHash(configHashPath)
            differences = findDifferences(packageConfig, configDiff, [])
            if differences:
                if packageName in packageInfo["ok"]:
                    packageInfo["reconfigure"][packageName] = differences
                    packageInfo["ok"].remove(packageName)
        return packageInfo

    def usePackage(self, packageName, action, scriptName=''):
        try:
            package = Package.Package(packageName, self.repository,
                                      self.config, self.filesystem,
                                      self.server, self.operatingSystem)
            package.initialize()
            if action == 'install':
                progressData = self.filesystem.getProgressData(stripVersionFromName = False)
                installedPackageNames, brokenPackageNames = miniUtility.getInstalled(progressData)
                for p in installedPackageNames:
                    if p.startswith(packageName):
                        Logger.error( 'Package %s is already installed.' %p )
                        return FAIL
                self.addPackages = {packageName:package}
                status = self.installPackages()
            if action == 'uninstall':
                status = package.uninstall()
            if action == 'verify':
                status = package.verify()
            if action == 'configure':
                if self.checkConfiguration(package) == FAIL:
                    return FAIL
                else:
                    status = package.configure()
                    hashPath = os.path.join(miniUtility.getPackagePath(), package.fullName, HASH_FILE)
                    Logger.info("writing configuration fingerprint to %s" % hashPath)
                    self.config.saveHash(hashPath)
            if action == 'execute':
                status = package.executeMaintScript(scriptName)
            return self.cleanup(status, logmessage="Finished %s for %s." %(action, packageName))
        except Exceptions.BadPackage, e:
            errmsg = "Cannot perform action %s on package %s: %s" % (action, e.packageName, e.errmsg)
            Logger.warning(errmsg)
            self.filesystem.warningLog(errmsg, self.server)
            return FAIL

if __name__ == "__main__":
    message =  """NOTE: This program is no longer
used to run the bombardier client from the command
line. Please use 'bc.py' instead."""
    print message

