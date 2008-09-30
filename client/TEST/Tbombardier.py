#!/cygdrive/c/Python25/python.exe

import unittest, sets, StringIO, time, sys

sys.path = ["..\\client"] + sys.path

import bombardier.BombardierClass
from bombardier.staticData import *
import bombardier.CommSocket

import MockObjects
  
def inMaintenance(config):
    import time
    day = time.strftime("%a")
    hour = time.localtime()[3]
    min  = time.localtime()[4]
    clock = time.strftime("%H:%M")
    config.data["system"] = {}
    config.data["system"]["maintenanceWindow"] = "%s %s 10" % (day, clock)

def outOfMaintenance(config):
    import time
    day = time.strftime("%a")
    hour = time.localtime()[3]
    min  = time.localtime()[4]
    clock = time.strftime("%H:%M")
    config.data["system"] = {}
    config.data["system"]["maintenanceWindow"] = "%s %s 0" % (day, clock)


class BombardierTest(unittest.TestCase):

    def setUp(self):
        data = {"testokpackage1":{"install": {"fullName": None}}}
        self.repository = MockObjects.MockRepository(data)
        self.filesystem = MockObjects.MockFilesystem()
        self.filesystem.environ["COMPUTERNAME"] = "computer1"
        self.filesystem.directories = []
        self.filesystem.files = []
        self.server = MockObjects.MockServer()
        self.logger = MockObjects.MockLogger()
        self.config = MockObjects.MockConfig()
        self.windows= MockObjects.MockWindows()
        self.commSocket = bombardier.CommSocket.CommSocket()
        self.bombardier = bombardier.BombardierClass.Bombardier(self.repository, self.config, self.logger,
                                                           self.filesystem, self.server, self.windows)
        
    def tearDown(self):
        pass

    def testDownloadBomSimple(self):
        packages = ["Hotfix-824146","Hotfix-828028","Hotfix-828035",
                    "Hotfix-828741","Hotfix-835732","Hotfix-manager"]
        self.server.serviceRequests = ["\n".join(packages)]
        status = self.bombardier.downloadBom(["web-1"])
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert status == OK
        assert len(fcalls) == 3
        assert `fcalls[0]`=="updateCurrentAction('Downloading Bill of Materials...', 0)"
        assert `fcalls[1]`.startswith("open")
        assert `fcalls[2]`.startswith("open(")
        assert len(scalls) == 1
        assert `scalls[0]` == "serviceRequest('pkggroups', {'address': 'http://127.0.0.1'}, "\
               "MOCK-LOGGER, {'group': 'web-1'}, None, False, None)", scalls[0]
        output1 = self.filesystem.writeFiles[0].buflist[0]
        assert output1 == "web-1"
        output2 = self.filesystem.writeFiles[1].buflist[0]
        for packageName in output2.split():
            assert packageName in packages, packageName
        assert len(wcalls) == 0

    def testDownloadBomBadgroup(self):
        self.server.serviceRequests = [""]
        status = self.bombardier.downloadBom(["foo"])
        assert status == FAIL, "Bad system type returned success"
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert len(scalls) == 2
        assert `scalls[0]`.startswith("serviceRequest('pkggroups'")
        assert `scalls[1]`.startswith("serverLog('CRITICAL', 'No packages "\
                                      "configured for this system',")
        assert len(fcalls) == 3
        assert `fcalls[2]` == "updateCurrentStatus('error', 'System does "\
               "not have a Bill of Materials')"
        assert len(wcalls) == 0

    def testDownloadBomEmpty(self):
        self.server.serviceRequests = [""]
        self.config.console = True
        status = self.bombardier.downloadBom([])
        assert status == FAIL, "Bad system type returned success"
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert len(fcalls) == 4, fcalls
        assert `fcalls[0]` == "updateCurrentAction('Downloading "\
               "Bill of Materials...', 0)"
        assert `fcalls[1]`.startswith("open")
        assert `fcalls[2]` == "clearLock(MOCK-LOGGER)"
        assert `fcalls[3]`.startswith("updateCurrentStatus")

        assert len(scalls) == 1, len(scalls)
        assert `scalls[0]`.startswith("serverLog('CRITICAL', 'No packages "\
                                      "configured for this system',")
        assert len(wcalls) == 2, len(wcalls)
        assert `wcalls[0]` == "testConsole(MOCK-LOGGER)", `wcalls[0]`
        assert `wcalls[1]`.startswith( "ShellExecuteSimple('http://127.0.0.1/"\
               "website/client/clientpackages?client=" ), `wcalls[1]`
        
    def testDownloadBomMultiple(self):
        packages1 = ["Hotfix-824146","Hotfix-828028","Hotfix-828035",
                     "Hotfix-828741","Hotfix-835732","Hotfix-manager"]
        packages2 = ["cheese", "Hotfix-828741"]
        self.server.serviceRequests = ["\n".join(packages1), "\n".join(packages2)]
        status = self.bombardier.downloadBom(["web-1", "foo-1"])
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert status == OK
        assert len(scalls) == 2
        assert `scalls[0]` == "serviceRequest('pkggroups', {'address': "\
               "'http://127.0.0.1'}, MOCK-LOGGER, {'group': 'web-1'}, "\
               "None, False, None)", `scalls[0]`
        assert `scalls[1]` == "serviceRequest('pkggroups', {'address': "\
               "'http://127.0.0.1'}, MOCK-LOGGER, {'group': 'foo-1'}, None, False, None)"
        assert len(fcalls) == 3
        assert `fcalls[0]`=="updateCurrentAction('Downloading Bill of Materials...', 0)"
        assert `fcalls[1]`.startswith("open")
        assert `fcalls[2]`.startswith("open(")
        output1 = self.filesystem.writeFiles[0].buflist[0]
        assert output1 == "web-1|foo-1", output1
        output2 = self.filesystem.writeFiles[1].buflist[0]
        uniquePackages = list(sets.Set(packages1 + packages2))
        for packageName in output2.split():
            assert packageName in uniquePackages, packageName
        assert len(wcalls) == 0

    def testDownloadBomGoofy1(self):
        self.server.serviceRequests = [""]
        status = self.bombardier.downloadBom([""])
        assert status == FAIL
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert len(scalls) == 2
        assert len(fcalls) == 3
        assert len(wcalls) == 0

    def testDownloadBomGoofy2(self):
        packages1 = ["Hotfix-824146","Hotfix-828028","Hotfix-828035",
                     "Hotfix-828741","Hotfix-835732","Hotfix-manager"]
        self.server.serviceRequests = ["\n".join(packages1), ""]
        status = self.bombardier.downloadBom(["web-1", "cheese"])
        assert status == OK
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert len(scalls) == 2
        assert len(fcalls) == 3
        assert len(wcalls) == 0
        ermsg = "Package group cheese does not exist on the repository (ignoring)"
        assert self.logger.data["warning"] == [ermsg], self.logger.data["warning"]

    def testDownloadBomPathological(self):
        status = self.bombardier.downloadBom(12)
        assert status == FAIL, "Bad pkgGroup returned success"        

      
    def testGetPackagesToAdd1(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"}},
                "pkg2": {"install": {"fullName":"pkg2-1"}}}
        repository = MockObjects.MockRepository(data)
        self.bombardier.repository = repository
        status, packages = self.bombardier.getPackagesToAdd(["pkg1", "pkg2"])
        pkg1 = packages["pkg1"]
        pkg2 = packages["pkg2"]
        assert pkg1.status == OK
        assert pkg2.status == OK
        assert status == OK
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        wcalls = self.windows.getAllCalls()
        rcalls = repository.getAllCalls()
        assert len(fcalls) == 4, len(fcalls)
        assert `fcalls[0]` == "updateProgressFile({'status': {'package': 'pkg1'}})"
        assert `fcalls[2]` == "updateProgressFile({'status': {'package': 'pkg2'}})"
        assert len(wcalls) == 0
        assert len(scalls) == 0
        assert len(rcalls) == 2
        assert `rcalls[0]` == "getMetaData('pkg1')"
        assert `rcalls[1]` == "getMetaData('pkg2')"

    def testGetPackagesToAdd2(self):
        data = {"pkg1": {"install": "hello"},
                "pkg2": "hello"}
        repository = MockObjects.MockRepository(data)
        self.bombardier.repository = repository
        status, packages = self.bombardier.getPackagesToAdd(["pkg9", "pkg1", "pkg2"])
        pkg9 = packages["pkg9"]
        pkg1 = packages["pkg1"]
        pkg2 = packages["pkg2"]
        assert pkg1.status == FAIL
        assert pkg2.status == FAIL
        assert pkg9.status == FAIL
        assert status == FAIL

    def testGetPackagesToRemove1(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": {"dep0":"pkg2"}},
                "pkg2": {"install": {"fullName":"pkg2-1"}}}
        self.filesystem.packagesFromFile[PROGRESS_FILE] = ["pkg1", "pkg2"]
        repository = MockObjects.MockRepository(data)
        self.config.repository = repository
        self.bombardier.repository = repository
        packages = self.bombardier.getPackagesToRemove(["pkg2"])
        test = ["pkg1", "pkg2"]
        for item in test:
            assert item in packages.keys(), "Dependency %s was expected in "\
                   "calculated dependency list %s" % (item, `packages.keys()`)
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        wcalls = self.windows.getAllCalls()
        rcalls = repository.getAllCalls()
        assert len(fcalls) == 5, len(fcalls)
        assert `fcalls[0]` == "getPackageFromFile('install-progress.yml', 1)"
        assert `fcalls[1]` == "updateProgressFile({'status': {'package': 'pkg2'}})"
        assert `fcalls[2]` == "updateCurrentAction('Initializing package.', 0)"
        assert len(wcalls) == 0
        assert len(scalls) == 0
        assert len(rcalls) == 2

    def testGetPackagesToRemove(self):
        data = {}
        self.filesystem.packagesFromFile[PROGRESS_FILE] = []
        repository = MockObjects.MockRepository(data)
        self.config.repository = repository
        self.bombardier.repository = repository
        packages = self.bombardier.getPackagesToRemove(["fugazi"])
        assert packages.keys() == ["fugazi"]

    def testGetVPkgNameFromPkgName(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": {"virtualpackage":"anypackage"}},
                "pkg2": {"install": {"fullName":"pkg2-1"}}}
        repository = MockObjects.MockRepository(data)
        vp = bombardier.BombardierClass.VirtualPackages(repository.packages)
        vPkgName = vp.getVPkgNameFromPkgName("pkg1")
        assert vPkgName == "anypackage", "Got back a weird virtual name "\
               "for pkg1-1: %s" % vPkgName
        vPkgName2 = vp.getVPkgNameFromPkgName("pkg2")
        assert vPkgName2 == "pkg2", "Got back a weird virtual name for pkg2: %s" % vPkgName2
        vPkgName3 = vp.getVPkgNameFromPkgName("shrubbery")
        assert vPkgName3 == "shrubbery", "Got back a weird virtual name "\
               "for nonsense package: %s" % vPkgName3

    def testGetPkgNameListFromVPkgName(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": {"virtualpackage":"anypackage"}},
                "pkg2": {"install": {"fullName":"pkg2-1"},
                         "dependencies": {"virtualpackage":"anypackage"}}}
        repository = MockObjects.MockRepository(data)
        vp = bombardier.BombardierClass.VirtualPackages(repository.packages)
        pkgNameSet = sets.Set(vp.getPkgNameListFromVPkgName("anypackage"))
        assert pkgNameSet == sets.Set(["pkg1", "pkg2"]), "Got back some weird set"\
             "of real package names: %s" % pkgNameSet

    def testResolveVpkgList(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": {"virtualpackage":"anypackage"}},
                "pkg2": {"install": {"fullName":"pkg2-1"}}}
        repository = MockObjects.MockRepository(data)
        vp = bombardier.BombardierClass.VirtualPackages(repository.packages)
        pkgNameSet = sets.Set(vp.resolveVPkgList(["anypackage", "pkg2"]))
        assert pkgNameSet == sets.Set(["anypackage", "pkg2"]),"Error in resolveVPkgList:"\
               "%s" % pkgNameSet

    def testGetActualPkgName(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": {"virtualpackage":"anypackage"}},
                "pkg2": {"install": {"fullName":"pkg2-1"}}}
        repository = MockObjects.MockRepository(data)
        vp = bombardier.BombardierClass.VirtualPackages(repository.packages)
        packageName = vp.getActualPkgName( "anypackage", ["pkg1", "pkg2"] )
        assert packageName == "pkg1", "Bad actual package name from "\
               "getActualPackageName: %s" % packageName
        
    def testRemoveVirtualPackage(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": {"dep0":"anypackage"}},
                "pkg2": {"install": {"fullName":"pkg2-1"},
                         "dependencies": {"virtualpackage":"anypackage"}}}
        repository = MockObjects.MockRepository(data)
        self.filesystem.packagesFromFile[PROGRESS_FILE] = ["pkg1", "pkg2"]
        self.config.repository = repository
        self.bombardier.repository = repository
        packages = self.bombardier.getPackagesToRemove(["pkg2"])
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        wcalls = self.windows.getAllCalls()
        rcalls = repository.getAllCalls()
        assert len(fcalls) == 5, len(fcalls)
        assert `fcalls[0]` == "getPackageFromFile('install-progress.yml', 1)"
        assert len(wcalls) == 0
        assert len(scalls) == 0
        assert len(rcalls) == 2
        testSet = sets.Set(["pkg1", "pkg2"])
        packageSet = sets.Set(packages.keys())
        assert testSet == packageSet, "Uninstalling a virtual package has "\
               "failed. %s != %s" % (testSet, packageSet)

    def testCheckBom(self):
        self.filesystem.packagesFromFile[PROGRESS_FILE] = ["pkg1", "pkg3"]
        self.filesystem.packagesFromFile[BOM_FILE] = ["pkg1", "pkg2"]
        shouldBeInstalled, shouldntBeInstalled = self.bombardier.checkBom()
        assert ["pkg2"] == shouldBeInstalled, shouldBeInstalled
        assert ["pkg3"] == shouldntBeInstalled, shouldntBeInstalled
        fcalls = self.filesystem.getAllCalls()
        assert len(fcalls) == 3
        assert `fcalls[0]` == "getPackageFromFile('BOM.txt', False)"
        assert `fcalls[1]` == "getPackageFromFile('install-progress.yml', 1)"
        assert `fcalls[2]`.startswith("open")
        
    def testDependenciesInstalled(self):
        data = """[pkg2]\ndep0=pkg1-1\n"""
        self.filesystem.readFiles = [StringIO.StringIO(data)]
        installedDependencies = self.bombardier.dependenciesInstalled(["pkg2"])
        assert installedDependencies == ["pkg1-1"]
        fcalls = self.filesystem.getAllCalls()
        assert len(fcalls) == 1
        assert `fcalls[0]`.startswith("open")
        
    def testAddToDependencyErrors(self):
        self.filesystem.readFiles = [StringIO.StringIO()]
        self.filesystem.writeFiles = [StringIO.StringIO()]
        pkg1 = MockObjects.MockPackage()
        pkgChain = bombardier.BombardierClass.PackageChain(100, "pkg1", {"pkg1": pkg1}, "pkg3",
                                                      self.repository, self.config, self.logger,
                                                      self.filesystem, self.server, self.windows)
        status = pkgChain.addToDependencyErrors("pkg1", "pkg2")
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        wcalls = self.windows.getAllCalls()
        rcalls = self.repository.getAllCalls()
        assert status == OK
        depErrorData = self.filesystem.writeFiles[0].buflist
        assert depErrorData == ['[pkg1]\n', 'dep0 = pkg2\n', '\n']
        assert len(scalls) == 1
        assert `scalls[0]` == "serverLog('WARNING', 'BOM file is incomplete: "\
               "should contain pkg2', {'address': 'http://127.0.0.1'}, MOCK-LOGGER, 'pkg1')", scalls[0]
        assert len(fcalls) == 3
        assert `fcalls[0]`.startswith("getAllFromFile(")
        assert `fcalls[1]`.startswith("open")
        assert `fcalls[2]`.startswith("open")

    def testPackageChain(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": {"dep0":"pkg5"}},
                "pkg5": {"install": {"fullName":"pkg5-1"},
                         "dependencies": {"dep0":"pkg6"}},
                "pkg6": {"install": {"fullName":"pkg6-1"}}}

        repository = MockObjects.MockRepository(data)
        pkg1 = MockObjects.MockPackage()
        pkg1.dependencies = ["pkg5"]
        packages = {"pkg1": pkg1}
        newChain = bombardier.BombardierClass.PackageChain(100, "pkg1", packages, ["pkg2", "pkg3"],
                                                      repository, self.config,
                                                      self.logger, self.filesystem,
                                                      self.server, self.windows)
        assert newChain.chain == ["pkg6", "pkg5", "pkg1"], newChain.chain
        pcalls = pkg1.getAllCalls()
        fcalls = self.filesystem.getAllCalls()
        rcalls = repository.getAllCalls()
        scalls = self.server.getAllCalls()
        assert len(pcalls) == 0
        assert len(scalls) == 2
        assert `scalls[0]` == "serverLog('WARNING', 'BOM file is incomplete: should contain "\
               "pkg5', {'address': 'http://127.0.0.1'}, MOCK-LOGGER, 'pkg1')"
        assert `scalls[1]` == "serverLog('WARNING', 'BOM file is incomplete: should contain "\
               "pkg6', {'address': 'http://127.0.0.1'}, MOCK-LOGGER, 'pkg5')"
        assert len(rcalls) == 2
        assert len(fcalls) == 10, len(fcalls)

    def testCreatePackageChains(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": {"dep0":"pkg5"}},
                "pkg5": {"install": {"fullName":"pkg5-1"},
                         "dependencies": {"dep0":"pkg6"}}}
        repository = MockObjects.MockRepository(data)
        self.filesystem.packagesFromFile[PROGRESS_FILE] = ["pkg2", "pkg3"]
        pkg1 = MockObjects.MockPackage()
        pkg1.dependencies = ["pkg5"]
        pkg5 = MockObjects.MockPackage()
        packages = {"pkg1": pkg1, "pkg5": pkg5}
        self.bombardier.repository = repository
        chains = self.bombardier.createPackageChains(packages)
        assert len(chains) == 2
        pcalls = pkg1.getAllCalls()
        fcalls = self.filesystem.getAllCalls()
        rcalls = self.repository.getAllCalls()
        scalls = self.server.getAllCalls()
        assert len(pcalls) == 0
        assert len(fcalls) == 1
        assert len(scalls) == 0
        assert len(rcalls) == 0

    def testGetTopPriority(self):
        chain1 = MockObjects.MockChain()
        chain2 = MockObjects.MockChain()
        chains = [(100, chain1), (300, chain2)]
        topPriority = self.bombardier.getTopPriority(chains)
        assert topPriority == 300

    def testInstallList(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": {"dep0":"pkg5"}},
                "pkg5": {"install": {"fullName":"pkg5-1"},
                         "dependencies": {"dep0":"pkg6"}}}
        repository = MockObjects.MockRepository(data)
        self.filesystem.packagesFromFile[PROGRESS_FILE] = ["pkg2", "pkg3"]
        pkg1 = MockObjects.MockPackage()
        pkg1.dependencies = ["pkg5"]
        pkg1.priority = 200
        pkg5 = MockObjects.MockPackage()
        pkg6 = MockObjects.MockPackage()
        pkg6.priority = 300
        packages = {"pkg1": pkg1, "pkg5": pkg5, "pkg6": pkg6}
        self.filesystem.packagesFromFile[PROGRESS_FILE] = ["pkg2", "pkg3"]
        installOrder  = self.bombardier.installList(packages)
        assert installOrder == ["pkg6", "pkg5", "pkg1"], "bad install order %s" % installOrder


    def testHandleConsole1(self):
        package = MockObjects.MockPackage()
        self.windows.testConsoleValue = False
        status = self.bombardier.handleConsole(package)
        assert status == OK
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert len(fcalls) == 2, len(fcalls)
        assert `fcalls[0]` == "updateCurrentStatus('idle', 'Rebooting for console')"
        assert `fcalls[1]` == "clearLock(MOCK-LOGGER)", `fcalls[1]`
        assert len(wcalls) == 4, wcalls
        assert `wcalls[0]` == "testConsole(MOCK-LOGGER)"
        assert `wcalls[1]` == "autoLogin(MOCK-CONFIG, MOCK-LOGGER)"
        assert `wcalls[2]` == "restartOnLogon()"
        assert `wcalls[3]` == "rebootSystem(logger=MOCK-LOGGER, "\
               "message='Rebooting to gain console access')"
        assert len(scalls) == 0
        
    def testHandleConsole2(self):
        package = MockObjects.MockPackage()
        status = self.bombardier.handleConsole(package)
        assert self.filesystem.getAllCalls() == []
        assert self.server.getAllCalls() == []
        wcalls = self.windows.getAllCalls()
        assert len(wcalls) == 1, wcalls
        assert `wcalls[0]` == "testConsole(MOCK-LOGGER)", `wcalls[0]`
        assert status == OK

    def testRebootForMoreInstallation(self):
        package = MockObjects.MockPackage()
        packages = {"pkg1": package}
        status = self.bombardier.rebootForMoreInstallation(package, packages)
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert len(fcalls) == 2
        assert `fcalls[0]` == "clearLock(MOCK-LOGGER)"
        assert `fcalls[1]` == "updateCurrentStatus('idle', 'Waiting for reboot')"
        assert len(wcalls) == 2, `wcalls`
        assert `wcalls[0]` == "autoLogin(MOCK-CONFIG, MOCK-LOGGER)"
        assert `wcalls[1]` == "restartOnLogon()"
        assert status == OK

    def testInstallPackages(self):
        pkg1 = MockObjects.MockPackage()
        packages = {"pkg1": pkg1}
        self.filesystem.packagesFromFile[PROGRESS_FILE] = ["pkg2", "pkg3"]
        status = self.bombardier.installPackages(packages)
        assert status == OK, "Perfectly good package failed to install"
        assert self.server.getAllCalls() == []
        assert self.windows.getAllCalls() == []
        assert self.repository.getAllCalls() == []
        fcalls = self.filesystem.getAllCalls()
        assert len(fcalls) == 5
        assert `fcalls[0]` == "updateCurrentStatus('installing', 'Installing packages')"
        assert `fcalls[1]` == "getPackageFromFile('install-progress.yml', 1)"
        assert `fcalls[2]` == "getPackageFromFile('install-progress.yml', 1)"
        assert `fcalls[3]` == "updateProgressFile({'todo': ['pkg1,<<dependency>>']}, True)"
        assert `fcalls[4]` == "updateProgressFile({'status': {'package': 'pkg1'}})"
        pcalls = pkg1.getAllCalls()
        assert len(pcalls) == 1
        assert `pcalls[0]`.startswith("process(<bound method Bombardier.abortIfTold"), `pcalls[0]`

    def testInstallPackagesNeedingConsole(self):
        pkg1 = MockObjects.MockPackage()
        pkg1.console = True
        packages = {"pkg1": pkg1}
        self.windows.testConsoleValue = False
        self.filesystem.packagesFromFile[PROGRESS_FILE] = ["pkg2", "pkg3"]
        status = self.bombardier.installPackages(packages)
        assert status == OK, "Console package failed to install"
        assert self.repository.getAllCalls() == []
        assert self.server.getAllCalls() == []
        fcalls = self.filesystem.getAllCalls()
        assert len(fcalls) == 7, len(fcalls)
        assert `fcalls[5]` == "updateCurrentStatus('idle', 'Rebooting for console')"
        assert `fcalls[6]` == "clearLock(MOCK-LOGGER)"
        pcalls = pkg1.getAllCalls()
        assert len(pcalls) == 1
        assert `pcalls[0]`.startswith("process(<bound method Bombardier.abortIfTold")
        wcalls = self.windows.getAllCalls()
        assert len(wcalls) == 4, wcalls
        assert `wcalls[0]` == "testConsole(MOCK-LOGGER)"
        assert `wcalls[1]` == "autoLogin(MOCK-CONFIG, MOCK-LOGGER)"
        assert `wcalls[2]` == "restartOnLogon()"
        assert `wcalls[3]` == "rebootSystem(logger=MOCK-LOGGER, "\
               "message='Rebooting to gain console access')"

    # WORKING
    def testInMaintenanceWindow(self):
        import time
        day = time.strftime("%a")
        hour = time.localtime()[3]
        min  = time.localtime()[4]
        clock = time.strftime("%H:%M")
        inMaintenance(self.config)
        status = self.bombardier.inMaintenanceWindow()
        assert status == True, "System should indicate that we are in a maintenance window"
        outOfMaintenance(self.config)
        status = self.bombardier.inMaintenanceWindow()
        assert status == False, "System should indicate that we are not in a maintenance window"
        self.config.data["system"]["maintenanceWindow"] = "%s 99:00 10" % (day)
        status = self.bombardier.inMaintenanceWindow()
        assert status == False, "System should indicate that we are not in a maintenance window"
        if day == "Sun":
            day = "Mon"
        else:
            day = "Sun"
        self.config.data["system"]["maintenanceWindow"] = "%s %s 10" % (day, clock)
        status = self.bombardier.inMaintenanceWindow()
        assert status == False, "System should indicate that we are not in a maintenance window"
        self.config.data["system"]["maintenanceWindow"] = "%s 99:00 10" % (day)
        status = self.bombardier.inMaintenanceWindow()
        assert status == False, "System should indicate that we are not in a maintenance window"
        self.config.data["system"]["maintenanceWindow"] = "Foo 00:00 10"
        status = self.bombardier.inMaintenanceWindow()
        assert status == False, "System should indicate that we are not in a maintenance window"
        self.config.data["system"]["maintenanceWindow"] = "Sun happy:00 10"
        status = self.bombardier.inMaintenanceWindow()
        assert status == False, "System should indicate that we are not in a maintenance window"
        self.config.data["system"]["maintenanceWindow"] = "cheese"
        status = self.bombardier.inMaintenanceWindow()
        assert status == False, "System should indicate that we are not in a maintenance window"

    def testVerifySystem1(self):
        packagesDat = {"pkg1": {"install": {"fullName":"pkg1-1"},
                                "dependencies": {"dep0":"pkg5"}},
                       "pkg2": {"install": {"fullName":"pkg2-1"},
                                "dependencies": {"dep0":"pkg6"}}}
        installProgress = {"pkg1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                      "UNINSTALLED": 'NA',
                                      "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                           "pkg2-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                      "UNINSTALLED": 'NA',
                                      "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}
        repository = MockObjects.MockRepository(packagesDat)
        self.config.repository = repository
        self.filesystem.progressData = installProgress
        self.server.yamlRequests = [{"status":OK}, {"status":OK}, {"status":OK}]
        base = os.path.join(os.getcwd(), "packages", "pkg1-1")
        self.filesystem.directories = [os.path.join(base, "scripts"),
                                       os.path.join(base, "injector")]
        self.filesystem.files = [os.path.join(base, "scripts", "verify.py")]
        self.filesystem.packagesFromFile[PROGRESS_FILE] = ["pkg1", "pkg2"] #Try testing bad pkgs

        self.bombardier.repository = repository
        messages = self.bombardier.verifySystem(self.commSocket.testStop)
        assert messages.has_key("pkg1")
        assert messages['pkg1'] == OK, \
               'messages dict is corrupt for testokpackage1'
        assert messages['pkg2'] == FAIL, \
               'messages dict is corrupt for testbadverifypackage1'
        assert len(messages.keys()) == 2, "Returned more then 2 results."
        fcalls = self.filesystem.getAllCalls()
        assert len(fcalls) == 27, len(fcalls)
        assert `fcalls[0]` == "getPackageFromFile('install-progress.yml', True)"
        assert `fcalls[1]` == "getProgressData()"
        assert `fcalls[5]`.startswith("isdir")
        assert `fcalls[6]`.startswith("isdir")
        assert `fcalls[7]`.startswith("isdir")
        assert `fcalls[10]`.startswith("isfile")
        assert `fcalls[11]`.startswith("execute")
        assert `fcalls[14]`.startswith("open")
        assert `fcalls[-1]`.startswith("open")
        scalls = self.server.getAllCalls()
        assert len(scalls) == 2

    def testVerifySystem2(self):
        packagesDat = {"pkg1": {"install": {"fullName":"pkg1-1"},
                                "dependencies": {"dep0":"pkg5"}},
                       "pkg2": {"install": {"fullName":"pkg2-1"},
                                "dependencies": {"dep0":"pkg6"}}}
        installProgress = {"pkg1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                      "UNINSTALLED": 'NA',
                                      "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                           "pkg2-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                      "UNINSTALLED": 'NA',
                                      "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}
        repository = MockObjects.MockRepository(packagesDat)
        self.config.repository = repository
        self.filesystem.progressData = installProgress
        self.server.yamlRequests = [{"status":OK}, {"status":OK}, {"status":OK}]
        base1 = os.path.join(os.getcwd(), "packages", "pkg1-1")
        base2 = os.path.join(os.getcwd(), "packages", "pkg2-1")
        self.filesystem.directories = [os.path.join(base1, "scripts"),os.path.join(base1, "injector"),
                                       os.path.join(base2, "scripts"),os.path.join(base2, "injector")]
        self.filesystem.files = [os.path.join(base1, "scripts", "verify.py"),
                                 os.path.join(base2, "scripts", "verify.py")]
        self.filesystem.packagesFromFile[PROGRESS_FILE] = ["pkg1", "pkg2"] #Try testing bad pkgs

        self.bombardier.repository = repository
        messages = self.bombardier.verifySystem(self.commSocket.testStop)
        assert messages == {"pkg1": OK, "pkg2":OK}

    def testVerifySystem3(self): # it's not time for this package to be verified
        packagesDat = {"pkg1": {"install": {"fullName":"pkg1-1"},
                                "dependencies": {"dep0":"pkg5"}}}
        installProgress = {"pkg1-1": {"INSTALLED": time.ctime(),
                                      "UNINSTALLED": 'NA',
                                      "VERIFIED": time.ctime()}}
        repository = MockObjects.MockRepository(packagesDat)
        self.config.repository = repository
        self.filesystem.progressData = installProgress
        self.server.yamlRequests = [{"status":OK}, {"status":OK}, {"status":OK}]
        self.filesystem.packagesFromFile[PROGRESS_FILE] = ["pkg1"]
        self.bombardier.repository = repository
        messages = self.bombardier.verifySystem(self.commSocket)
        assert messages == {}, `messages`

    def testVerifySystem4(self): # No packages installed, error in verify.
        packagesDat = {"pkg1": {"install": {"fullName":"pkg1-1"},
                                "dependencies": {"dep0":"pkg5"}}}
        repository = MockObjects.MockRepository(packagesDat)
        self.config.repository = repository
        self.filesystem.progressData = {}
        self.server.yamlRequests = [{"status":OK}, {"status":OK}, {"status":OK}]
        self.filesystem.packagesFromFile[PROGRESS_FILE] = ["pkg1"]
        self.bombardier.repository = repository

        messages = self.bombardier.verifySystem(self.commSocket)
        assert messages == None, `messages`

    def testReconcileSystem1(self):
        self.config.data = {"packageGroups": {"group0":"base"}}
        self.server.serviceRequests = ["pkg1\npkg2\n"]
        packagesDat = {"pkg1": {"install": {"fullName":"pkg1-1"}},
                       "pkg2": {"install": {"fullName":"pkg2-1"},
                                "dependencies": {"dep0": "pkg1"}}}
        repository = MockObjects.MockRepository(packagesDat)
        self.config.repository = repository
        inMaintenance(self.config)
        self.filesystem.packagesFromFile[PROGRESS_FILE] = []
        self.filesystem.packagesFromFile[BOM_FILE] = ["pkg1", "pkg2"]
        self.filesystem.packagesFromFile["base.BOM"] = ["pkg1", "pkg2"]
        base1 = os.path.join(os.getcwd(), "packages", "pkg1-1")
        base2 = os.path.join(os.getcwd(), "packages", "pkg2-1")
        bomfile = os.path.join(os.getcwd(), BOM_FILE)
        self.filesystem.directories = [os.path.join(base1, "scripts"),os.path.join(base1, "injector"),
                                       os.path.join(base2, "scripts"),os.path.join(base2, "injector")]
        self.filesystem.files = [os.path.join(base1, "scripts", "installer.py"),
                                 os.path.join(base1, "scripts", "verify.py"),
                                 os.path.join(base2, "scripts", "installer.py"),
                                 os.path.join(base2, "scripts", "verify.py"),
                                 bomfile]

        self.bombardier.repository = repository
        
        status = self.bombardier.reconcileSystem(self.commSocket.testStop)
        assert status == OK, "Simple package installation failed"
        scalls = self.server.getAllCalls()
        assert len(scalls) == 4, len(scalls)
        fcalls = self.filesystem.getAllCalls()
##         for i in range(0, len(fcalls)):
##             print "%d: %s" % (i, `fcalls[i]`)
        assert len(fcalls) == 60, len(fcalls)
        assert `fcalls[0]` == "setLock(MOCK-LOGGER)"
        assert `fcalls[7]` == "getPackageFromFile('BOM.txt', False)", `fcalls[7]`
        assert `fcalls[19]` == "updateProgressFile({'status': {'package': 'pkg1'}})"
        assert `fcalls[26]` == "getPackageFromFile('base.BOM', False)"
        assert `fcalls[27]` == "getPackageFromFile('base.BOM', False)" # FIXME
        assert `fcalls[44]` == "updateProgressFile({'status': {'package': 'pkg2'}})"
        assert `fcalls[59]` == "clearLock(MOCK-LOGGER)"
        wcalls = self.windows.getAllCalls()
        assert len(wcalls) == 2, `wcalls`
        assert `wcalls[0]` == "noRestartOnLogon()", `wcalls[0]`
        assert `wcalls[1]` == "noAutoLogin()"

    def testReconcileSystemWithDependencies(self):
        self.config.data = {"packageGroups": {"group0":"base"}}
        self.server.serviceRequests = ["pkg1\npkg2\n"]
        packagesDat = {"pkg1": {"install": {"fullName":"pkg1-1"}},
                       "pkg2": {"install": {"fullName":"pkg2-1"},
                                "dependencies": {"dep0": "pkg1"}}}
        repository = MockObjects.MockRepository(packagesDat)
        self.config.repository = repository
        inMaintenance(self.config)
        self.filesystem.packagesFromFile[PROGRESS_FILE] = ["pkg1", "pkg2"]
        self.filesystem.packagesFromFile[BOM_FILE] = ["pkg2"]
        base1 = os.path.join(os.getcwd(), "packages", "pkg1-1")
        base2 = os.path.join(os.getcwd(), "packages", "pkg2-1")
        bomfile = os.path.join(os.getcwd(), BOM_FILE)
        self.filesystem.directories = [os.path.join(base1, "scripts"),os.path.join(base1, "injector"),
                                       os.path.join(base2, "scripts"),os.path.join(base2, "injector")]
        self.filesystem.files = [os.path.join(base1, "scripts", "installer.py"),
                                 os.path.join(base1, "scripts", "verify.py"),
                                 os.path.join(base2, "scripts", "installer.py"),
                                 os.path.join(base2, "scripts", "verify.py"),
                                 bomfile]
        self.filesystem.readFiles = [StringIO.StringIO("[pkg2]\ndep0=pkg1")]
        self.bombardier.repository = repository

        status = self.bombardier.reconcileSystem(self.commSocket.testStop)
        fcalls = self.filesystem.getAllCalls()
        assert len(fcalls) == 20, len(fcalls)
        assert status == OK

    def testReconcileSystemUninstallWithBackup(self):
        self.config.data = {"packageGroups": {"group0":"base"}}
        self.server.serviceRequests = ["pkg1\n"]
        packagesDat = {"pkg1": {"install": {"fullName":"pkg1-1"}}}
        repository = MockObjects.MockRepository(packagesDat)
        self.config.repository = repository
        inMaintenance(self.config)
        self.filesystem.packagesFromFile[PROGRESS_FILE] = ["pkg1"]
        self.filesystem.packagesFromFile[BOM_FILE] = []
        base1 = os.path.join(os.getcwd(), "packages", "pkg1-1")
        bomfile = os.path.join(os.getcwd(), BOM_FILE)
        self.filesystem.directories = [os.path.join(base1, "scripts"),os.path.join(base1, "injector")]
        self.filesystem.files = [os.path.join(base1, "scripts", "uninstaller.py"),
                                 os.path.join(base1, "scripts", "backup.py"),
                                 bomfile]
        self.bombardier.repository = repository
        status = self.bombardier.reconcileSystem(self.commSocket.testStop)
        fcalls = self.filesystem.getAllCalls()
##         for i in range(0, len(fcalls)):
##             print "%d: %s" % (i, `fcalls[i]`)
        assert len(fcalls) == 51, len(fcalls)
        assert `fcalls[19]`.rfind('backup') != -1
        assert `fcalls[27]`.rfind('backup.py') != -1
        assert `fcalls[33]`.startswith("createTar('pkg1.tar.gz'")
        assert `fcalls[35]`.startswith("getBinaryDataFromFilePath(")
        assert `fcalls[39]`.startswith("execute")
        assert `fcalls[39]`.rfind("uninstaller.py'") != -1
        assert status == OK

    def testReconcileSystemBogus(self):
        self.config.data = {"packageGroups": {"group0":"base"}}
        self.server.serviceRequests = ["pkg1\n"]
        packagesDat = {"pkg1": {"install": {"fullName":"pkg1-1"}}}
        repository = MockObjects.MockRepository(packagesDat)
        self.config.repository = repository
        inMaintenance(self.config)
        self.filesystem.packagesFromFile[PROGRESS_FILE] = ["pkg1"]
        self.filesystem.packagesFromFile[BOM_FILE] = ["nonsense"]
        base1 = os.path.join(os.getcwd(), "packages", "pkg1-1")
        bomfile = os.path.join(os.getcwd(), BOM_FILE)
        self.filesystem.directories = [os.path.join(base1, "scripts"),os.path.join(base1, "injector")]
        self.filesystem.files = [os.path.join(base1, "scripts", "uninstaller.py"),
                                 os.path.join(base1, "scripts", "backup.py"),
                                 bomfile]
        self.bombardier.repository = repository
        status = self.bombardier.reconcileSystem(self.commSocket.testStop)
        assert status == FAIL
        assert self.logger.data["error"] == ["INVALID PACKAGE: nonsense"]

    def testGetDetailedTodolistSimple(self):
        # basic test case, no surprises
        self.config.data = {"packageGroups": {"group0":"base"}}
        installList = ["package1", "package2"]
        self.filesystem.packagesFromFile = {"base.BOM": ["package1", "package2"]}
        todolist = self.bombardier.getDetailedTodolist(installList)
        assert len(todolist) == 2
        assert todolist[0] == "package1,base", todolist

    def testGetDetailedTodolistMultiple(self):
        # coming from more than one source
        self.config.data = {"packageGroups": {"group0":"base", "group1":"foo"}}
        installList = ["package1", "package2"]
        self.filesystem.packagesFromFile = {"base.BOM": ["package1", "package2"],
                                            "foo.BOM": ["package1", "package2"]}
        todolist = self.bombardier.getDetailedTodolist(installList)
        assert todolist[0] == "package1,foo/base", todolist[0]

    def testGetDetailedTodolistDependency(self):
        # coming from no sources
        installList = ["package1", "package2"]
        installList.append("package3")
        todolist = self.bombardier.getDetailedTodolist(installList)
        assert "package3,<<dependency>>" in todolist, todolist
        
    def testSecurePackages(self):
        pass
##         #^ Change the system config so it is a "secured" system
##         status = bombardier.setSecurityLevel(config, logger, SECURITY_HIGH)
##         #^ tell the system to install a package which
##         #  shouldn't install if the system is secured
##         setBOM(["testlowsecurity"])
##         setInstallProgress([])
##         status = bombardier.reconcileSystem(config, logger, testing=True)
##         assert status == OK, "Low security package installation failed"

##         #^ verify the package installed

##         #^ verify the system is secured

##         #^ unsecure the system
        

if __name__ == "__main__":
    suite = unittest.TestSuite()
    #suite.addTest(BombardierTest("testPackageIntegration"))
    #suite.addTest(BombardierTest("testVirtualPackages"))
    #suite.addTest(BombardierTest("testRemoveVirtualPackage"))
    #suite.addTest(BombardierTest("testBackupUninstall"))
    #suite.addTest(BombardierTest("testGetDetailedTodolist"))
    #suite.addTest(BombardierTest("testSetSecurityLevel"))
    #suite.addTest(BombardierTest("testSecureSystem"))
    #suite.addTest(BombardierTest("testSecurePackages"))
    #suite.addTest(BombardierTest("testBackupUninstall"))
    #suite.addTest(BombardierTest("testZProcessPackages"))
    #suite.addTest(BombardierTest("testVerifySystem4"))
    #suite.addTest(BombardierTest("testDownloadBomMultiple"))
    suite.addTest(unittest.makeSuite(BombardierTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
