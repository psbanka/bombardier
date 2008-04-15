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
import miniUtility, Package, Exceptions, Logger
from staticData import *

class PackageChain:
    def __init__(self, priority, startPackageName, packages,
                 installedPackageNames, brokenPackageNames, repository,
                 config, filesystem, operatingSystem, instanceName):
        self.depth       = 0
        self.priority    = priority
        self.packages    = packages
        self.filesystem  = filesystem
        self.operatingSystem    = operatingSystem
        self.instanceName = instanceName
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
            self.packages[pkgName].addDependencyError( depName )

    def getActualPkgName( self, pkgName ):
        return( self.vPackages.getActualPkgName( pkgName, self.packages.keys() ) )

    def getNewPackage( self, pkgName ):
        newPackage = Package.Package(pkgName, self.repository, self.config, self.filesystem, 
                                     self.operatingSystem, self.instanceName) 
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
    output = differences
    for key in packageConfig.keys():
        if type(packageConfig[key]) == type({}):
            if key not in configDiff:
                continue
            newdif = findDifferences(packageConfig[key], configDiff[key],
                                     output, chain + [key])
            pyChucker( newdif ) #FIXME!
            continue
        if key in configDiff.keys():
            output.append("%s/%s" % ('/'.join(chain), key))
            continue
    return output

class Bombardier:

    def __init__(self, repository, config, filesystem, operatingSystem, instanceName):
        self.repository = repository
        self.config     = config
        self.filesystem = filesystem
        self.operatingSystem = operatingSystem
        self.instanceName    = instanceName
        
    ### TESTED
    def getDependencyErrors(self, bomPackageNames, progressData):
        dependencyNames = set([])
        installProgress = progressData.get('install-progress', {})
        for packageName in installProgress:
            packageDict = installProgress[ packageName ]
            depErrors = set(packageDict.get('DEPENDENCY_ERRORS',[]))
            dependencyNames = dependencyNames.union(depErrors)    
        dependencyNames = list(dependencyNames - set(bomPackageNames))
        return dependencyNames

    ### WON'T BE TESTED
    def checkForConsoleReboot(self, package):
        if self.operatingSystem.testConsole() == FAIL:
            erstr = "Logging in for console access "\
                    "for package %s..." % (package.name)
            Logger.info(erstr)
            self.filesystem.clearLock()
            status = self.operatingSystem.autoLogin(self.config)
            if status == FAIL:
                errMsg = "Cannot gain console access because this system "\
                        "does not have valid login credentials."
                raise Exceptions.ConsoleException(errMsg)
            self.operatingSystem.restartOnLogon()
            msg = "System requires a console to install this package and one does not exist. " \
                  "Please log in to continue the installation."
            Logger.warning(msg)
            return REBOOT
        return OK

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
        packageData = self.filesystem.getProgressData(self.instanceName, stripVersionFromName = True)
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
                                        self.repository, self.config, self.filesystem,
                                        self.operatingSystem, self.instanceName)
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

    def uninstallOrder(self, packageDict):
        chains = self.createPackageChains(packageDict)
        progressData = self.filesystem.getProgressData(self.instanceName, stripVersionFromName = True)
        installedPackageNames, brokenPackageNames = miniUtility.getInstalled(progressData)
        uninstallOrder = []
        for priority, chain in chains:
            Logger.info(">>>>   CHAIN: %s" % chain )
            for packageName in chain:
                Logger.info(">>> PACKAGENAME: %s" % packageName)
                if packageName in installedPackageNames:
                    if packageName not in uninstallOrder:
                        uninstallOrder.append(packageName)
        return uninstallOrder # returns a list of packageNames in the correct installation order

    # TESTED
    def installOrder(self, packageDict):
        """ - Create chains of package dependencies, the priority of each
        chain is the package with the highest priority in it, unless
        that package is already installed.
        - A package can appear in more than one chain
        - A package chain can have a single package in it if it does not
        have any dependencies and is not dependent on others."""

        chains = self.createPackageChains(packageDict)
        progressData = self.filesystem.getProgressData(self.instanceName, stripVersionFromName = True)
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

    # TESTED
    def installPackages(self, addPackageDict):
        makingProgress = True
        packageNamesLeft = ['initialize']
        while makingProgress and packageNamesLeft:
            makingProgress = False
            installOrder = self.installOrder(addPackageDict)
            packageNamesLeft = []
            map(packageNamesLeft.append, installOrder) # Need a deep copy
            for packageName in installOrder:
                Logger.info("Packages remaining to install (in order): %s" % packageNamesLeft)
                packageNamesLeft.remove(packageName)
                package = addPackageDict[packageName]
                erstr = "Currently installing package "\
                        "priority %s [%s]" % (package.priority, packageName)
                Logger.info(erstr)
                if package.console and self.checkForConsoleReboot(package) == REBOOT:
                    return REBOOT
                status = package.installAndVerify(packageNamesLeft)
                hashPath = os.path.join(miniUtility.getPackagePath(self.instanceName), package.fullName, HASH_FILE)
                self.config.saveHash(hashPath)
                if status == FAIL:
                    erstr = "Package installation failure -- re-calculating package installation order"
                    Logger.error(erstr)
                    break
                else:
                    makingProgress = True
        if packageNamesLeft:
            Logger.error("There are packages that are broken, and we have done all we can do. ; ;")
            return FAIL
        return OK

    def uninstallPackages(self, delPackageDict):
        makingProgress = True
        packageNamesLeft = ['initialize']
        while makingProgress and packageNamesLeft:
            makingProgress = False
            uninstallOrder = self.uninstallOrder(delPackageDict)
            packageNamesLeft = []
            map(packageNamesLeft.append, uninstallOrder) # Need a deep copy
            for packageName in uninstallOrder:
                Logger.info("Packages remaining to uninstall (in order): %s" % packageNamesLeft)
                package = delPackageDict[packageName]
                packageNamesLeft.remove(packageName)
                status = package.uninstall()
                erstr = "Currently uninstalling package "\
                        "priority %s [%s]" % (package.priority, packageName)
                Logger.info(erstr)
                status = package.uninstall()
                if status == FAIL:
                    erstr = "Package uninstallation failure -- re-calculating package installation order"
                    Logger.error(erstr)
                    break
                else:
                    makingProgress = True
        if packageNamesLeft:
            Logger.error("There are packages that are broken, and we have done all we can do. ; ;")
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
    def getPackagesToAddDict(self, addPackageNames):
        packages = {}
        for packageName in addPackageNames:
            try:
                newPackage = Package.Package(packageName, self.repository, self.config, self.filesystem,
                                             self.operatingSystem, self.instanceName)
                newPackage.initialize()
                if self.checkConfiguration(newPackage) == FAIL:
                    raise Exceptions.BadPackage(newPackage.name, "Bad Config")
                packages[packageName] = newPackage
            except Exceptions.BadPackage, e:
                errmsg = "Skipping bad package %s: %s" % (e.packageName, e.errmsg)
                Logger.warning(errmsg)
        return packages

    def createPackageDict(self, packageList, action):
        packageDict = {}
        for packageName in packageList:
            if action == UNINSTALL:
                Logger.info("Package %s is "\
                            "not on the bill of materials." % packageName)
            try:
                newPackage = Package.Package(packageName, self.repository, self.config, self.filesystem,
                                             self.operatingSystem, self.instanceName)
                newPackage.initialize()
            except Exceptions.BadPackage, e:
                errmsg = "Skipping Bad package: %s" % `e`
                Logger.warning(errmsg)
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
                package = Package.Package(packageName, self.repository, self.config, self.filesystem, 
                                          self.operatingSystem, self.instanceName)
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
    def getPackagesToRemoveDict(self, delPackageNames):
        progressData = self.filesystem.getProgressData(self.instanceName, stripVersionFromName = True)
        installedPackageNames, brokenPackageNames = miniUtility.getInstalled(progressData)
        packageDict = self.createPackageDict(delPackageNames, UNINSTALL)
        if sets.Set(installedPackageNames) == sets.Set(packageDict.keys()):
            return packageDict
        packageDict = self.getUninstallPackageDependencies(packageDict,
                                                           delPackageNames,
                                                           installedPackageNames)
        return packageDict

    def checkBom(self, bomPackageNames):
        """ Check through what should be installed on the system and what
        is installed on the system and determine what packages aren't
        installed that should be and what packages are installed that
        shouldn't be."""
        shouldBeInstalled = []
        shouldntBeInstalled = []
        progressData = self.filesystem.getProgressData(self.instanceName, stripVersionFromName = True)
        installedPackageNames, brokenPackageNames = miniUtility.getInstalled(progressData)
        dependencyErrors = self.getDependencyErrors(bomPackageNames, progressData)
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
        progressData = self.filesystem.getProgressData(self.instanceName)
        installedPackageNames, brokenPackageNames = miniUtility.getInstalled(progressData)
        testResults = {}

        for fullPackageName in installedPackageNames:
            try:
                shortPackageName = miniUtility.stripVersion(fullPackageName)
                package = Package.Package(shortPackageName, self.repository, self.config,
                                          self.filesystem, self.operatingSystem,
                                          self.instanceName)
                package.initialize()
            except Exceptions.BadPackage, e:
                errmsg = "Not testing %s (%s)" % (fullPackageName, e)
                Logger.warning(errmsg)
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

    def checkInstallationStatus(self, packageNames = []):
        #self.packageNames = packageNames
        pkgGroups,individualPackageNames  = self.config.getPackageGroups()
        Logger.info("package groups: %s, individuals: %s" % (pkgGroups, individualPackageNames)) #FIXME
        #spkgPath          = miniUtility.getSpkgPath()
        #self.filesystem.chdir(spkgPath)
        if packageNames == []:
            packageNames = individualPackageNames
        if packageNames == []:
            self.filesystem.clearLock()
            raise Exceptions.BadBillOfMaterials("Empty Bill of Materials")
        addPackageNames, delPackageNames = self.checkBom(packageNames)
        addPackageDict = self.getPackagesToAddDict(addPackageNames)
        delPackageDict = self.getPackagesToRemoveDict(delPackageNames)
        Logger.info("Packages to install: %s" % addPackageDict.keys())
        removeFullPackageNames = [delPackageDict[x].fullName for x in delPackageDict.keys()]
        Logger.info("Packages to remove: %s" % removeFullPackageNames)
        return addPackageDict, delPackageDict

    ### TESTED
    def reconcileSystem(self, packageNames = []):
        addPackageDict, delPackageDict = self.checkInstallationStatus(packageNames)
        if self.filesystem.setLock() == FAIL:
            return FAIL
        for packageName in delPackageDict:
            status = delPackageDict[packageName].uninstall()
            if status == FAIL:
                errmsg = "Uninstallation failed on %s. Aborting reconcile." % packageName
                Logger.error(errmsg)
                return self.cleanup(FAIL, logmessage="Finished installing (ERRORS ENCOUNTERED).")

        addPackageDict, delPackageDict = self.checkInstallationStatus(packageNames)
        status = self.installPackages(addPackageDict)
        return self.cleanup(status, logmessage="Finished installing.")

    def checkConfigurationHash(self, packageName):
        newPackage = Package.Package(packageName, self.repository, self.config, 
                                     self.filesystem, self.operatingSystem,
                                     self.instanceName) 
        newPackage.initialize()
        packageConfig = newPackage.getConfiguration()
        configHashPath = os.path.join(miniUtility.getPackagePath(self.instanceName), 
                                      newPackage.fullName, HASH_FILE)
        configDiff = self.config.checkHash(configHashPath)
        differences = findDifferences(packageConfig, configDiff, [])
        return differences

    def checkSystem(self, packageNames = []):
        addPackageDict, delPackageDict = self.checkInstallationStatus(packageNames)
        progressData = self.filesystem.getProgressData(self.instanceName, stripVersionFromName = True)
        fullProgressData = self.filesystem.getProgressData(self.instanceName, stripVersionFromName = False)
        fullInstalledPackageNames, fullBrokenPackageNames = miniUtility.getInstalled(fullProgressData)
        Logger.info("packages that are installed: %s" % ' '.join(fullInstalledPackageNames))
        installedPackageNames, brokenPackageNames = miniUtility.getInstalled(progressData)
        shouldBeInstalled, shouldntBeInstalled = self.checkBom(packageNames)
        # check the configuration for each installed package
        packageInfo = {"ok":installedPackageNames,
                       "reconfigure": {},
                       "not-installed": shouldBeInstalled,
                       "remove": shouldntBeInstalled,
                       "broken": brokenPackageNames}
        for packageName in shouldntBeInstalled:
            packageInfo["ok"].remove(packageName)
        for packageName in installedPackageNames:
            differences = self.checkConfigurationHash(packageName)
            if differences:
                if packageName in packageInfo["ok"]:
                    packageInfo["reconfigure"][packageName] = differences
                    packageInfo["ok"].remove(packageName)
        return packageInfo

    def usePackage(self, packageName, action, scriptName=''):
        try:
            package = Package.Package(packageName, self.repository, self.config, self.filesystem,
                                      self.operatingSystem, self.instanceName)
            package.initialize()
            if action == INSTALL:
                addPackageDict, delPackageDict = self.checkInstallationStatus()
                Logger.info("AddPackageDict: %s --- delPacakgeDict: %s" % (addPackageDict.keys(), delPackageDict.keys()))
                if not packageName in addPackageDict:
                    Logger.error("Package %s cannot be installed." % packageName)
                    return FAIL
                addPackageDict = {packageName:package}
                status = self.installPackages(addPackageDict)
            if action == UNINSTALL:
                addPackageDict, delPackageDict = self.checkInstallationStatus()
                if packageName in addPackageDict:
                    Logger.error("Package %s is not installed." % packageName)
                    return FAIL
                delPackageDict = {packageName:package}
                status = self.uninstallPackages(delPackageDict)
            if action == VERIFY:
                status = package.verify()
            if action == CONFIGURE:
                if self.checkConfiguration(package) == FAIL:
                    return FAIL
                else:
                    status = package.configure()
                    hashPath = os.path.join(miniUtility.getPackagePath(self.instanceName), 
                                            package.fullName, HASH_FILE)
                    Logger.info("writing configuration fingerprint to %s" % hashPath)
                    self.config.saveHash(hashPath)
            if action == EXECUTE:
                status = package.executeMaintScript(scriptName)
            return self.cleanup(status, logmessage="Finished %s for %s." %(ACTION_DICT[action], packageName))
        except Exceptions.BadPackage, e:
            errmsg = "Cannot perform action %s on package %s: %s" % (ACTION_DICT[action], e.packageName, e.errmsg)
            Logger.warning(errmsg)
            return FAIL

if __name__ == "__main__":
    message =  """NOTE: This program is no longer
used to run the bombardier client from the command
line. Please use 'bc.py' instead."""
    print message

