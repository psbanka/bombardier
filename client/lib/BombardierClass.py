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
import PackageV4, PackageV5
import Exceptions
from bombardier_core.Logger import Logger
from bombardier_core.mini_utility import getInstalled, getPackagePath
from bombardier_core.mini_utility import diffDicts, strip_version
from bombardier_core.static_data import OK, FAIL, MAX_CHAIN_DEPTH, HASH_FILE
from bombardier_core.static_data import UNINSTALL, RECONCILE, DRY_RUN, VERIFY
from bombardier_core.static_data import VERIFY_INTERVAL, INSTALL, CONFIGURE
from bombardier_core.static_data import EXECUTE, ACTION_REVERSE_LOOKUP
from bombardier_core.static_data import VALID_PACKAGE_VERSIONS

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
            package_name = self.chain[pkgIndex]
            if package_name in self.brokenPackageNames:
                pkgIndex -= 1
                Logger.warning("Omitting packages %s due to bad package" % (self.chain[pkgIndex:]))
                break
        self.chain = self.chain[:pkgIndex+1]

    # TESTED
    def packageChain(self, package_name):
        self.depth += 1
        if self.depth > MAX_CHAIN_DEPTH:
            raise Exceptions.DependencyLoopException(self.chain)
        for depName in self.packages[ self.getActualPkgName( package_name ) ].dependencies:
            if depName not in self.installedPackageNames: 
                self.chain.insert( 0, self.getActualPkgName( depName ) )
            else:
                continue
            self.syncDependencies( depName, package_name )
            dependency = self.packages.get(self.getActualPkgName( depName ))
            if depName in self.brokenPackageNames:
                self.priority = 0
            else:
                self.priority = max( dependency.priority, self.priority )
            self.packageChain( depName )
        self.depth -= 1

    def syncDependencies( self, depName, package_name ):
        if depName not in ( self.vPackages.resolveVPkgList( self.installedPackageNames ) + \
                            self.vPackages.resolveVPkgList( self.packages.keys() ) ):
            self.packages[depName] = self.get_new_package( depName )
            if self.record_errors:
                self.packages[package_name].add_dependency_error( depName )

    def getActualPkgName( self, package_name ):
        return( self.vPackages.getActualPkgName( package_name, self.packages.keys() ) )

    def get_new_package( self, package_name ):
        version = self.determine_package_version(package_name)
        package = None
        if version == 4:
            package = PackageV4.PackageV4(package_name, self.repository, self.config, self.filesystem,
                                          self.operatingSystem, self.instance_name)
        else:
            package = PackageV5.PackageV5(package_name, self.repository, self.config, self.filesystem,
                                          self.operatingSystem, self.instance_name)
        new_package.initialize()
        return package

class VirtualPackages:
    def __init__( self, packageDict ):
        self.packageDict = packageDict
        self.virtualPackagesDict = {}
        self.virtualNameLookupDict = {}
        self.initVPkgDictionaries()

    def initVPkgDictionaries( self ):
        for package_name in self.packageDict:
            package_data = self.packageDict[package_name]
            if package_data.get( 'virtualpackage' ):
                virtualPackageName = package_data['virtualpackage'] 
                self.virtualNameLookupDict.setdefault( package_name, virtualPackageName )
                self.virtualPackagesDict.setdefault( virtualPackageName,[] ).append( package_name )

    def getPkgNameListFromVPkgName( self, virtualPackageName ):
        return( self.virtualPackagesDict.get( virtualPackageName, [] ) )

    def getVPkgNameFromPkgName( self, package_name ):
        return( self.virtualNameLookupDict.get( package_name, package_name ) )
    
    def resolveVPkgList( self, packageList ):
        return( [self.getVPkgNameFromPkgName( package_name ) for package_name in packageList] )

    def getActualPkgName( self, package_name, packageNames ):
        resolvedPackageNames = self.getPkgNameListFromVPkgName( package_name )
        pName = [actualName for actualName in resolvedPackageNames if actualName in packageNames] + [package_name]
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
        for package_name in installProgress:
            packageDict = installProgress[ package_name ]
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
        for package_name in packageDict.keys():
            if package_name in brokenPackageNames:
                Logger.warning("Skipping broken package %s" % package_name)
                continue
            if package_name not in installedPackageNames:
                chainPriority = packageDict[package_name].priority
            else:
                chainPriority = 0
            try:
                newChain = PackageChain(chainPriority, package_name, packageDict,
                                        installedPackageNames, brokenPackageNames,
                                        self.repository, self.config, self.filesystem,
                                        self.operatingSystem, self.instance_name, self.record_errors)
            except Exceptions.BadPackage, e:
                errmsg = "Package %s will not be installed because it is "\
                         "dependent upon one or more broken packages" % package_name
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
                    for package_name in chain:
                        if package_name not in installedPackageNames:
                            if package_name not in installOrder:
                                installOrder.append(package_name)
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
            for package_name in installOrder:
                Logger.info("Packages remaining to install (in order): %s" % packageNamesLeft)
                packageNamesLeft.remove(package_name)
                package = addPackageDict[package_name]
                erstr = "Currently installing package "\
                        "priority %s [%s]" % (package.get_priority(), package_name)
                Logger.info(erstr)
                status = package.install_and_verify(packageNamesLeft)
                if not dryRun:
                    hash_path = os.path.join(package.get_path(), HASH_FILE)
                    self.config.saveHash(hash_path)
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
        if package.get_configuration():
            required_configs = package.get_configuration()
            diff = diffDicts(required_configs, self.config.data)
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

    def get_new_package(self, package_name):
        version = self.determine_package_version(package_name)
        package = None
        if version == 4:
            new_package = PackageV4.PackageV4(package_name, self.repository, self.config, self.filesystem,
                                              self.operatingSystem, self.instance_name)
        else:
            new_package = PackageV5.PackageV5(package_name, self.repository, self.config, self.filesystem,
                                              self.operatingSystem, self.instance_name)
        new_package.initialize()
        return new_package

    ### TESTED
    def getPackagesToAddDict(self, addPackageNames):
        packages = {}
        for package_name in addPackageNames:
            try:
                Logger.warning("Preparing to ADD package: %s" % package_name)

                package = self.get_new_package(package_name)
                self.checkConfiguration(package)
                packages[package_name] = package
            except Exceptions.BadPackage, e:
                errmsg = "Skipping bad package %s: %s" % (e.package_name, e.errmsg)
                self.operation_output.append(errmsg)
                Logger.warning(errmsg)
        return packages

    def createPackageDict(self, packageList, action):
        packageDict = {}
        for package_name in packageList:
            if action == UNINSTALL:
                Logger.warning("Preparing to REMOVE package: %s" % package_name)
            try:
                package = self.get_new_package(package_name)
            except Exceptions.BadPackage, e:
                errmsg = "Skipping Bad package: %s" % `e`
                Logger.warning(errmsg)
            package.action = action
            packageDict[package_name] = package
        return packageDict

    def sortUninstalledPackages(self, uninstallOrder):
        "Make sure packages get uninstalled in the right order"
        dependencyDict = {}
        if len(uninstallOrder) > 1:
            Logger.info("Determining uninstallation order...")
            for package_name in uninstallOrder:
                dependencyDict[package_name] = []

            for package_name in uninstallOrder:
                package = self.get_new_package(package_name)
                for otherPackageName in uninstallOrder:
                    if otherPackageName in package.dependencies:
                        dependencyDict[otherPackageName].append(package_name)
        else:
            return uninstallOrder

        newProperOrder = copy.deepcopy(uninstallOrder)
        swapped = True

        while swapped:
            properOrder = copy.deepcopy(newProperOrder)
            swapped = False
            for package_name in properOrder:
                dependencyList = dependencyDict[package_name]
                for dependency in dependencyList:
                    index1 = properOrder.index(dependency)
                    index2 = properOrder.index(package_name)
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
            for package_name in installedPackageNames:
                if package_name in packageDict.keys():
                    Logger.debug("Package %s will already be deleted -- "\
                                 "ignoring" % package_name)
                    continue
                package = self.get_new_package(package_name)
                package.action = UNINSTALL
                for tombstonedPackageName in packageDict.keys():
                    if vPackages.getVPkgNameFromPkgName( tombstonedPackageName ) in package.dependencies:
                        erstr = "Adding to package removal list: %s (depends on %s)"
                        uninstallOrder.insert(0, package_name)
                        Logger.info(erstr % (package_name, tombstonedPackageName))
                        packageDict[tombstonedPackageName].depends_on_me.append(package_name)
                        if package_name not in packageDict.keys():
                            if package_name not in newDependencyNames:
                                packageDict[package_name] = package
                                newDependencyNames.append(package_name)
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
        for package_name in installedPackageNames:
            if package_name not in bomPackageNames:
                shouldntBeInstalled.append(package_name)
        for package_name in bomPackageNames:
            if package_name not in installedPackageNames:
                shouldBeInstalled.append(package_name)
        return shouldBeInstalled, shouldntBeInstalled

    def verifySystem(self):
        progressData = self.filesystem.getProgressData(self.instance_name)
        installedPackageNames, brokenPackageNames = getInstalled(progressData)
        testResults = {}

        for fullPackageName in installedPackageNames:
            try:
                short_package_name = strip_version(fullPackageName)
                package = self.get_new_package(short_package_name)
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
                testResults[short_package_name] = package.status

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

    def checkConfigurationHash(self, package_name):
        package = self.get_new_package(package_name)
        packageConfig = package.get_configuration()
        configHashPath = os.path.join(package.get_path(), HASH_FILE)
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
        for package_name in shouldntBeInstalled:
            packageInfo["ok"].remove(package_name)
        for package_name in installedPackageNames:
            differences = self.checkConfigurationHash(package_name)
            if differences:
                if package_name in packageInfo["ok"]:
                    packageInfo["reconfigure"][package_name] = differences
                    packageInfo["ok"].remove(package_name)
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
        for package_name in uninstallOrder:
            uninstallStatus = delPackageDict[package_name].uninstall(dryRun)
            if uninstallStatus == FAIL:
                return FAIL
        return status

    def determine_package_version(self, package_name):
        meta_data = self.repository.get_meta_data(package_name)
        version = meta_data.data.get("package-version")
        if type(version) == type(1):
            if version in VALID_PACKAGE_VERSIONS:
                return version
        msg = "Unknown package version"
        raise Exceptions.BadPackage, (package_name, msg)

    def use_package(self, package_name, action, scriptName=''):
        try:
            version = self.determine_package_version(package_name)
            package = self.get_new_package(package_name)
            if package.status == FAIL:
                self.operation_status = FAIL
                return self.cleanup()
            if action == INSTALL:
                progressData = self.filesystem.getProgressData(self.instance_name, stripVersionFromName = False)
                installedPackageNames, brokenPackageNames = getInstalled(progressData)
                if package_name in [installedPackageNames + brokenPackageNames]:
                    Logger.error("Package %s cannot be installed." % package_name)
                    self.operation_status = FAIL
                    return FAIL
                addPackageDict = {package_name:package}
                status = self.installPackages(addPackageDict)
            if action == UNINSTALL:
                progressData = self.filesystem.getProgressData(self.instance_name, stripVersionFromName = False)
                installedPackageNames, brokenPackageNames = getInstalled(progressData)
                bomPackageNames = installedPackageNames
                if package_name in bomPackageNames:
                    bomPackageNames.remove(package_name)
                self.config.setBomPackages(bomPackageNames)
                addPackageDict, delPackageDict, uninstallOrder = self.checkInstallationStatus()
                status = self._uninstall_packages(delPackageDict, uninstallOrder)
            if action == VERIFY:
                status = package.verify()
            if action == CONFIGURE:
                self.checkConfiguration(package)
                status = package.configure()
                hash_path = os.path.join(package.get_path(), HASH_FILE)
                Logger.info("writing configuration fingerprint to %s" % hash_path)
                self.config.saveHash(hash_path)
            if action == EXECUTE:
                status = package.execute_maint_script(scriptName)
                if status == FAIL:
                    self.operation_status = FAIL
            self.operation_output.append("Finished %s for %s." %(ACTION_REVERSE_LOOKUP[action], package_name))
            status = self.cleanup()
            return self.cleanup()
        except Exceptions.BadPackage, e:
            errmsg = "Cannot perform action %s on package %s: %s" % (ACTION_REVERSE_LOOKUP[action], e.package_name, e.errmsg)
            Logger.warning(errmsg)
            Logger.info("RETURN FAIL 1")
            return FAIL

if __name__ == "__main__":
    message =  """NOTE: This program is no longer
used to run the bombardier client from the command
line. Please use 'bc.py' instead."""
    print message

