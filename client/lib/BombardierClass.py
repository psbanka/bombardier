#!/cygdrive/c/Python24/python.exe

# BombardierClass.py: This is the central focal point of most of the
# activities of this software. The reconcile_system method kicks off
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

import sets, os, time, copy
import Package, Exceptions
from bombardier_core.Logger import Logger
from bombardier_core.mini_utility import getInstalled, getPackagePath
from bombardier_core.mini_utility import diffDicts, strip_version
from bombardier_core.static_data import OK, FAIL, MAX_CHAIN_DEPTH, HASH_FILE
from bombardier_core.static_data import UNINSTALL, RECONCILE, DRY_RUN, VERIFY
from bombardier_core.static_data import VERIFY_INTERVAL, INSTALL, CONFIGURE, ACTION_DICT
from bombardier_core.static_data import EXECUTE, ACTION_DICT

def swap(listObj, index1, index2):
    newList = copy.deepcopy(listObj)
    newList[index1] = listObj[index2]
    newList[index2] = listObj[index1]
    return newList

class PackageChain:
    def __init__(self, priority, startPackageName, packages,
                 installedPackageNames, brokenPackageNames, repository,
                 config, filesystem, operatingSystem, instance_name, record_errors):
        self.depth       = 0
        self.priority    = priority
        self.packages    = packages
        self.filesystem  = filesystem
        self.operatingSystem    = operatingSystem
        self.instance_name = instance_name
        self.chain       = [startPackageName]
        self.repository  = repository
        self.config      = config
        self.record_errors = record_errors
        self.vPackages   = VirtualPackages(repository.package_data)
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
            if self.record_errors:
                self.packages[pkgName].add_dependency_error( depName )

    def getActualPkgName( self, pkgName ):
        return( self.vPackages.getActualPkgName( pkgName, self.packages.keys() ) )

    def getNewPackage( self, pkgName ):
        newPackage = Package.Package(pkgName, self.repository, self.config, self.filesystem, 
                                     self.operatingSystem, self.instance_name) 
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
            package_data = self.packageDict[packageName]
            if package_data.get( 'virtualpackage' ):
                virtualPackageName = package_data['virtualpackage'] 
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
            _newdif = findDifferences(packageConfig[key], configDiff[key],
                                     output, chain + [key])
            continue
        if key in configDiff.keys():
            output.append("%s/%s" % ('/'.join(chain), key))
            continue
    return output

class Bombardier:

    def __init__(self, repository, config, filesystem, operatingSystem, instance_name):
        self.repository = repository
        self.config     = config
        self.filesystem = filesystem
        self.operatingSystem = operatingSystem
        self.instance_name    = instance_name
        self.record_errors = True
        self.repository.checkLocalPackages()
        self.operation_status = OK
        self.operation_output = []

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
        package_data = self.filesystem.getProgressData(self.instance_name, stripVersionFromName = False)
        installedPackageNames, brokenPackageNames = getInstalled(package_data)
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
                                        self.operatingSystem, self.instance_name, self.record_errors)
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
    def installOrder(self, packageDict):
        """ - Create chains of package dependencies, the priority of each
        chain is the package with the highest priority in it, unless
        that package is already installed.
        - A package can appear in more than one chain
        - A package chain can have a single package in it if it does not
        have any dependencies and is not dependent on others."""

        chains = self.createPackageChains(packageDict)
        progressData = self.filesystem.getProgressData(self.instance_name, stripVersionFromName = False)
        installedPackageNames, brokenPackageNames = getInstalled(progressData)
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
    def installPackages(self, addPackageDict, dryRun=False):
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
                status = package.install_and_verify(packageNamesLeft)
                if not dryRun:
                    hashPath = os.path.join(getPackagePath(self.instance_name), package.full_name, HASH_FILE)
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

    ### WON'T BE TESTED
    def cleanup(self):
        self.operatingSystem.noRestartOnLogon()
        self.operatingSystem.noAutoLogin()
        self.filesystem.clearLock()
        if self.operation_status == FAIL:
            log_function = Logger.error
        else:
            log_function = Logger.info
        for message in self.operation_output:
            log_function("==OUTPUT==:%s" % message)
        return self.operation_status

    def checkConfiguration(self, package):
        if package.meta_data.has_key("configuration"):
            requiredConfigs = package.meta_data["configuration"]
            diff = diffDicts(requiredConfigs, self.config.data)
            if diff != {}:
                self.operation_status = FAIL
                errmsg = "This machine does not have sufficient "\
                         "configuration data to install %s " % package.name
                diffTxt = ''
                for key in diff:
                    diffTxt += "key: %s; value: %s" % (key, diff[key])
                Logger.warning(errmsg)
                Logger.warning(diffTxt)
                raise Exceptions.BadPackage(package.name, "Bad Config")

    ### TESTED
    def getPackagesToAddDict(self, addPackageNames):
        packages = {}
        for packageName in addPackageNames:
            try:
                Logger.warning("Preparing to ADD package: %s" % packageName)
                newPackage = Package.Package(packageName, self.repository, self.config, self.filesystem,
                                             self.operatingSystem, self.instance_name)
                newPackage.initialize()
                self.checkConfiguration(newPackage)
                packages[packageName] = newPackage
            except Exceptions.BadPackage, e:
                errmsg = "Skipping bad package %s: %s" % (e.packageName, e.errmsg)
                self.operation_output.append(errmsg)
                Logger.warning(errmsg)
        return packages

    def createPackageDict(self, packageList, action):
        packageDict = {}
        for packageName in packageList:
            if action == UNINSTALL:
                Logger.warning("Preparing to REMOVE package: %s" % packageName)
            try:
                newPackage = Package.Package(packageName, self.repository, self.config, self.filesystem,
                                             self.operatingSystem, self.instance_name)
                newPackage.initialize()
            except Exceptions.BadPackage, e:
                errmsg = "Skipping Bad package: %s" % `e`
                Logger.warning(errmsg)
            newPackage.action = action
            packageDict[packageName] = newPackage
        return packageDict

    def sortUninstalledPackages(self, uninstallOrder):
        "Make sure packages get uninstalled in the right order"
        dependencyDict = {}
        if len(uninstallOrder) > 1:
            Logger.info("Determining uninstallation order...")
            for packageName in uninstallOrder:
                dependencyDict[packageName] = []

            for packageName in uninstallOrder:
                package = Package.Package(packageName, self.repository, self.config, self.filesystem, 
                                          self.operatingSystem, self.instance_name)
                package.initialize()
                for otherPackageName in uninstallOrder:
                    if otherPackageName in package.dependencies:
                        dependencyDict[otherPackageName].append(packageName)
        else:
            return uninstallOrder

        newProperOrder = copy.deepcopy(uninstallOrder)
        swapped = True

        while swapped:
            properOrder = copy.deepcopy(newProperOrder)
            swapped = False
            for packageName in properOrder:
                dependencyList = dependencyDict[packageName]
                for dependency in dependencyList:
                    index1 = properOrder.index(dependency)
                    index2 = properOrder.index(packageName)
                    if index1 > index2:
                        newProperOrder = swap(properOrder, index1, index2)
                        swapped = True
                        break
                if swapped:
                    break

        Logger.info("Uninstallation Order: %s" % newProperOrder)
        return newProperOrder

    def getUninstallPackageDependencies(self, packageDict, delPackageNames, installedPackageNames):
        # add any packages that are installed already
        # which are dependent upon those to the list as well
        Logger.info("Checking dependencies of packages to be uninstalled %s..." % delPackageNames)
        vPackages = VirtualPackages(self.repository.package_data)
        uninstallOrder = copy.deepcopy(delPackageNames)
        while delPackageNames:
            newDependencyNames = []
            delPackageNames = []
            for packageName in installedPackageNames:
                if packageName in packageDict.keys():
                    Logger.debug("Package %s will already be deleted -- "\
                                 "ignoring" % packageName)
                    continue
                package = Package.Package(packageName, self.repository, self.config, self.filesystem, 
                                          self.operatingSystem, self.instance_name)
                package.initialize()
                package.action = UNINSTALL
                for tombstonedPackageName in packageDict.keys():
                    if vPackages.getVPkgNameFromPkgName( tombstonedPackageName ) in package.dependencies:
                        erstr = "Adding to package removal list: %s (depends on %s)"
                        uninstallOrder.insert(0, packageName)
                        Logger.info(erstr % (packageName, tombstonedPackageName))
                        packageDict[tombstonedPackageName].depends_on_me.append(packageName)
                        if packageName not in packageDict.keys():
                            if packageName not in newDependencyNames:
                                packageDict[packageName] = package
                                newDependencyNames.append(packageName)
                delPackageNames = newDependencyNames

        properOrder = self.sortUninstalledPackages(uninstallOrder)
        return packageDict, properOrder

    ### TESTED
    def getPackagesToRemoveDict(self, delPackageNames):
        uninstallOrder = []
        progressData = self.filesystem.getProgressData(self.instance_name, stripVersionFromName = False)
        installedPackageNames, brokenPackageNames = getInstalled(progressData)
        packageDict = self.createPackageDict(delPackageNames, UNINSTALL)
        if sets.Set(installedPackageNames) == sets.Set(packageDict.keys()):
            return packageDict, packageDict.keys()
        if delPackageNames:
            packageDict, uninstallOrder = self.getUninstallPackageDependencies(packageDict, delPackageNames,
                                                                           installedPackageNames)
        return packageDict, uninstallOrder

    def checkBom(self, bomPackageNames):
        """ Check through what should be installed on the system and what
        is installed on the system and determine what packages aren't
        installed that should be and what packages are installed that
        shouldn't be."""
        shouldBeInstalled = []
        shouldntBeInstalled = []
        progressData = self.filesystem.getProgressData(self.instance_name, stripVersionFromName = False)
        installedPackageNames, brokenPackageNames = getInstalled(progressData)
        dependency_errors = self.getDependencyErrors(bomPackageNames, progressData)
        if dependency_errors:
            errmsg = "The following packages are installed as "\
                     "dependencies %s" % dependency_errors
            Logger.debug(errmsg)
        bomPackageNames += dependency_errors
        for packageName in installedPackageNames:
            if packageName not in bomPackageNames:
                shouldntBeInstalled.append(packageName)
        for packageName in bomPackageNames:
            if packageName not in installedPackageNames:
                shouldBeInstalled.append(packageName)
        return shouldBeInstalled, shouldntBeInstalled

    def verifySystem(self):
        progressData = self.filesystem.getProgressData(self.instance_name)
        installedPackageNames, brokenPackageNames = getInstalled(progressData)
        testResults = {}

        for fullPackageName in installedPackageNames:
            try:
                shortPackageName = strip_version(fullPackageName)
                package = Package.Package(shortPackageName, self.repository, self.config,
                                          self.filesystem, self.operatingSystem,
                                          self.instance_name)
                package.initialize()
            except Exceptions.BadPackage, e:
                errmsg = "Not testing %s (%s)" % (fullPackageName, e)
                Logger.warning(errmsg)
                continue
            
            interval = package.meta_data.get('verify','verifyInterval', VERIFY_INTERVAL)

            if not progressData.has_key(package.full_name): # don't verify if the package isn't installed
                return {}
            Logger.info("Trying to verify %s" % package.name) 
            timeString = progressData[package.full_name]['VERIFIED']
            timer = time.mktime(time.strptime(timeString))

            if timer + interval <= time.time():
                package.action = VERIFY
                package.verify()
                testResults[shortPackageName] = package.status

        return testResults

    def checkInstallationStatus(self, bomPackageNames = []):
        if bomPackageNames == []:
            bomPackageNames = self.config.getBomPackages()
            if bomPackageNames == []:
                Logger.warning("Empty Bill of Materials")
        addPackageNames, delPackageNames = self.checkBom(bomPackageNames)
        addPackageDict = self.getPackagesToAddDict(addPackageNames)
        delPackageDict, uninstallOrder = self.getPackagesToRemoveDict(delPackageNames)
        return addPackageDict, delPackageDict, uninstallOrder

    ### TESTED
    def reconcile_system(self, action=RECONCILE, packageNames = []):
        dryRun = False
        if action == DRY_RUN:
            dryRun = True
        addPackageDict, delPackageDict, uninstallOrder = self.checkInstallationStatus(packageNames)
        Logger.info("uninstallOrder: %s" % uninstallOrder)
        if self.filesystem.setLock() == FAIL:
            return FAIL
        status = self._uninstall_packages(delPackageDict, uninstallOrder, dryRun)
        if status == FAIL:
            self.operation_output.append("Uninstallation failed. Aborting reconcile.")
            self.operation_status = FAIL
            return self.cleanup()

        addPackageDict, delPackageDict, uninstallOrder = self.checkInstallationStatus(packageNames)
        status = self.installPackages(addPackageDict, dryRun)
        if status != OK:
            self.operation_status = FAIL
        self.operation_output.append("Finished installing")
        return self.cleanup()

    def checkConfigurationHash(self, packageName):
        newPackage = Package.Package(packageName, self.repository, self.config,
                                     self.filesystem, self.operatingSystem,
                                     self.instance_name)
        newPackage.initialize()
        packageConfig = newPackage.get_configuration()
        configHashPath = os.path.join(getPackagePath(self.instance_name),
                                      newPackage.full_name, HASH_FILE)
        configDiff = self.config.checkHash(configHashPath)
        differences = findDifferences(packageConfig, configDiff, [])
        return differences

    def check_system(self, packageNames = []):
        addPackageDict, delPackageDict, uninstallOrder = self.checkInstallationStatus(packageNames)
        progressData = self.filesystem.getProgressData(self.instance_name)
        fullProgressData = self.filesystem.getProgressData(self.instance_name, stripVersionFromName = False)
        fullInstalledPackageNames, fullBrokenPackageNames = getInstalled(fullProgressData)
        Logger.info("packages that are installed: %s" % ' '.join(fullInstalledPackageNames))
        installedPackageNames, brokenPackageNames = getInstalled(progressData)
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

    def _uninstall_packages(self, delPackageDict, uninstallOrder, dryRun=False):
        status = OK
        removeFullPackageNames = []
        Logger.info("UninstallOrder: %s" % uninstallOrder)
        for name in uninstallOrder:
            if delPackageDict[name].full_name:
                nameStr = delPackageDict[name].full_name
            else:
                nameStr = name
            removeFullPackageNames.append(nameStr)
        Logger.info("Packages to remove: %s" % removeFullPackageNames)
        for packageName in uninstallOrder:
            uninstallStatus = delPackageDict[packageName].uninstall(dryRun)
            if uninstallStatus == FAIL:
                return FAIL
        return status

    def use_package(self, packageName, action, scriptName=''):
        try:
            package = Package.Package(packageName, self.repository, self.config, self.filesystem,
                                      self.operatingSystem, self.instance_name)
            package.initialize()
            if action == INSTALL:
                progressData = self.filesystem.getProgressData(self.instance_name, stripVersionFromName = False)
                installedPackageNames, brokenPackageNames = getInstalled(progressData)
                if packageName in [installedPackageNames + brokenPackageNames]:
                    Logger.error("Package %s cannot be installed." % packageName)
                    return FAIL
                addPackageDict = {packageName:package}
                status = self.installPackages(addPackageDict)
            if action == UNINSTALL:
                progressData = self.filesystem.getProgressData(self.instance_name, stripVersionFromName = False)
                installedPackageNames, brokenPackageNames = getInstalled(progressData)
                bomPackageNames = installedPackageNames
                if packageName in bomPackageNames:
                    bomPackageNames.remove(packageName)
                self.config.setBomPackages(bomPackageNames)
                addPackageDict, delPackageDict, uninstallOrder = self.checkInstallationStatus()
                status = self._uninstall_packages(delPackageDict, uninstallOrder)
            if action == VERIFY:
                status = package.verify()
            if action == CONFIGURE:
                self.checkConfiguration(package)
                status = package.configure()
                hashPath = os.path.join(getPackagePath(self.instance_name), 
                                        package.full_name, HASH_FILE)
                Logger.info("writing configuration fingerprint to %s" % hashPath)
                self.config.saveHash(hashPath)
            if action == EXECUTE:
                status = package.execute_maint_script(scriptName)
                if status == FAIL:
                    self.operation_status = FAIL
            self.operation_output.append("Finished %s for %s." %(ACTION_DICT[action], packageName))
            return self.cleanup()
        except Exceptions.BadPackage, e:
            errmsg = "Cannot perform action %s on package %s: %s" % (ACTION_DICT[action], e.packageName, e.errmsg)
            Logger.warning(errmsg)
            return FAIL

if __name__ == "__main__":
    message =  """NOTE: This program is no longer
used to run the bombardier client from the command
line. Please use 'bc.py' instead."""
    print message

