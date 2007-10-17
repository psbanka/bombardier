#!/cygdrive/c/Python25/python.exe

import unittest, sets, StringIO, time, sys, os, yaml
import Tcommon

sys.path = [os.path.join("..", "client")] + sys.path

import bombardier.BombardierClass
from bombardier.staticData import *
import bombardier.Exceptions as Exceptions
import bombardier.Logger as Logger
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
        self.config = MockObjects.MockConfig()
        self.windows= MockObjects.MockWindows()
        self.bombardier = bombardier.BombardierClass.Bombardier(self.repository, self.config,
                                                                self.filesystem, self.server,
                                                                self.windows)
        
    def tearDown(self):
        pass

    def testDownloadBomSimple(self):
        packages = ["Hotfix-824146","Hotfix-828028","Hotfix-828035",
                    "Hotfix-828741","Hotfix-835732","Hotfix-manager"]
        self.server.bom["web-1"] = packages
        packageNames = self.bombardier.downloadBom(["web-1"])
        scalls = self.server.getAllCalls()
        assert len(scalls) == 1, `scalls`
        assert `scalls[0]` == "bomRequest('web-1')", `scalls[0]`
        fcalls = self.filesystem.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert len(fcalls) == 1, `fcalls`
        assert `fcalls[0]`.startswith("open")
        output1 = self.filesystem.writeFiles[0].buflist[0]
        assert output1 == "web-1"
        for packageName in packageNames:
            assert packageName in packages, packageName
        assert len(wcalls) == 0

    def testCheckConfiguration(self):
        pkg1 = MockObjects.MockPackage()
        pkg1.dependencies = ["pkg2"]
        pkg1.priority = 200
        metaData = MockObjects.MockMetaData({})
        pkg1.metaData = metaData
        status = self.bombardier.checkConfiguration(pkg1)
        assert status == OK, "System does not have proper configuration"

        data = {"configuration":
                {"section1": {"item1":"spam","item2":"eggs"},
                 "section2": {"item1":"foo"}}
                }
        metaData = MockObjects.MockMetaData(data)
        pkg1.metaData = metaData
        status = self.bombardier.checkConfiguration(pkg1)
        assert status == FAIL, "System does not know its configuration is wrong"

        self.config.data = {"section1": {"item1": "foo", "item2": "bar", "item3": "baz"},
                            "section2": {"item1": 3},
                            "section3": {"item1": "cheeze"}}
        status = self.bombardier.checkConfiguration(pkg1)
        assert status == OK, "System does not know its configuration is correct"


    def testBogusDependency(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": ["pkg2"]},
                "pkg5": {"install": {"fullName":"pkg5-1"}}}
        self.filesystem.status = {}
        repository = MockObjects.MockRepository(data)
        self.config.repository = repository
        self.bombardier.repository = repository
        packages = self.bombardier.getPackagesToAdd(["pkg1", "pkg5"])
        assert set(["pkg1", "pkg5"]) == set(packages.keys())
        pkg1 = MockObjects.MockPackage()
        pkg1.dependencies = ["pkg2"]
        pkg1.priority = 200
        pkg5 = MockObjects.MockPackage()
        pkg5.priority = 35
        packages = {"pkg1": pkg1, "pkg5": pkg5}
        self.filesystem.status = {}
        installOrder  = self.bombardier.installList(packages)
        assert installOrder == ["pkg5"], installOrder

    def testDownloadBomBadgroup(self):
        self.server.yamlRequests = [[]]
        exceptionRaised = False
        self.server.bom["foo"] = []
        try:
            packageNames = self.bombardier.downloadBom(["foo"])
        except Exceptions.BadBillOfMaterials, e:
            assert `e` == "No packages configured for this system"
            exceptionRaised = True
        assert exceptionRaised == True
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert len(scalls) == 1
        assert `scalls[0]` == "bomRequest('foo')", `scalls[0]`
        assert len(fcalls) == 2, `fcalls`
        assert `fcalls[0]`.startswith("open"), `fcalls[1]`
        assert `fcalls[1]` == "warningLog('No packages configured for this system', <UNPRINTABLE>)"
        assert len(wcalls) == 0

    def testDownloadBomEmpty(self):
        self.server.yamlRequests = [[]]
        exceptionRaised = False
        try:
            packageNames = self.bombardier.downloadBom([])
        except Exceptions.BadBillOfMaterials, e:
            assert `e` == "No packages configured for this system", e
            exceptionRaised = True
        scalls = self.server.getAllCalls()
        assert len(scalls) == 0, `scalls`
        fcalls = self.filesystem.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert len(fcalls) == 1, fcalls
        assert `fcalls[0]`.startswith("open")
        assert len(wcalls) == 0, `wcalls`
        
    def testDownloadBomMultiple(self):
        packages1 = ["Hotfix-824146","Hotfix-828028","Hotfix-828035",
                     "Hotfix-828741","Hotfix-835732","Hotfix-manager"]
        packages2 = ["cheese", "Hotfix-828741"]
        self.server.bom["web-1"] = packages1
        self.server.bom["foo-1"] = packages2
        packageNames = self.bombardier.downloadBom(["web-1", "foo-1"])
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert len(scalls) == 2
        assert `scalls[0]` == "bomRequest('web-1')", `scalls[0]`
        assert `scalls[1]` == "bomRequest('foo-1')", `scalls[1]`
        assert len(fcalls) == 1, `fcalls`
        assert `fcalls[0]`.startswith("open")
        output1 = self.filesystem.writeFiles[0].buflist[0]
        assert output1 == "web-1|foo-1", output1
        uniquePackages = list(sets.Set(packages1 + packages2))
        for packageName in packageNames:
            assert packageName in uniquePackages, packageName
        assert len(wcalls) == 0

    def testDownloadBomGoofy1(self):
        self.server.yamlRequests = [[]]
        self.server.bom[''] = []
        try:
            packageNames = self.bombardier.downloadBom([""])
        except Exceptions.BadBillOfMaterials, e:
            assert `e` == "No packages configured for this system"
            exceptionRaised = True
        assert exceptionRaised == True
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert len(scalls) == 1, `scalls`
        assert len(fcalls) == 2, `fcalls`
        assert len(wcalls) == 0

    def testDownloadBomGoofy2(self):
        packages1 = ["Hotfix-824146","Hotfix-828028","Hotfix-828035",
                     "Hotfix-828741","Hotfix-835732","Hotfix-manager"]
        self.server.bom["web-1"] = packages1
        self.server.bom["cheese"] = []
        self.server.yamlRequests = [packages1,[]]
        packageNames = self.bombardier.downloadBom(["web-1", "cheese"])
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert len(scalls) == 2, `scalls`
        assert len(fcalls) == 1, `fcalls`
        assert len(wcalls) == 0

    def testDownloadBomPathological(self):
        exceptionRaised = False
        try:
            status = self.bombardier.downloadBom(12)
        except Exceptions.BadBillOfMaterials, e:
            assert `e` == "Invalid input to function. Should be a list of strings, got: 12", e
            exceptionRaised = True
        assert exceptionRaised == True

      
    def testGetPackagesToAdd1(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"}},
                "pkg2": {"install": {"fullName":"pkg2-1"}}}
        repository = MockObjects.MockRepository(data)
        self.bombardier.repository = repository
        packages = self.bombardier.getPackagesToAdd(["pkg1", "pkg2"])
        pkg1 = packages["pkg1"]
        pkg2 = packages["pkg2"]
        assert pkg1.status == OK
        assert pkg2.status == OK
        scalls = self.server.getAllCalls()
        assert len(scalls) == 0, `scalls`
        fcalls = self.filesystem.getAllCalls()
        wcalls = self.windows.getAllCalls()
        rcalls = repository.getAllCalls()
        assert len(fcalls) == 0, len(fcalls)
        assert len(wcalls) == 0
        assert len(rcalls) == 2
        assert `rcalls[0]` == "getMetaData('pkg1')"
        assert `rcalls[1]` == "getMetaData('pkg2')"

    def testGetPackagesToAdd2(self):
        data = {"pkg1": {"install": "hello"},
                "pkg2": "hello",
                "pkg5": {"install": {"fullName": "pkg5-1"}}}
        repository = MockObjects.MockRepository(data)
        self.bombardier.repository = repository
        packages = self.bombardier.getPackagesToAdd(["pkg9", "pkg1", "pkg2", "pkg5"])
        assert packages.keys() == ["pkg5"], packages.keys()

    def testGetPackagesToAdd3(self):
        data = {"pkg1": {"configuration": {"section1": {"item1": "foo"},
                                           "section2": {"item1": 3},
                                           "section3": {"item1": "cheeze"}},
                         "install": {"fullName": "pkg1-1"}}}
        repository = MockObjects.MockRepository(data)
        self.bombardier.repository = repository
        packages = self.bombardier.getPackagesToAdd(["pkg1"])
        assert packages.keys() == [], packages.keys()


        self.bombardier.config.data = {"section1": {"item1": "foo", "item2": "bar", "item3": "baz"},
                                       "section2": {"item1": 3},
                                       "section3": {"item1": "cheeze"}}
        packages = self.bombardier.getPackagesToAdd(["pkg1"])
        assert packages.keys() == ["pkg1"], packages.keys()
        

    def testGetPackagesToRemove1(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": ["pkg2"]},
                "pkg2": {"install": {"fullName":"pkg2-1"}}}
        installProgress1 = {"install-progress":
                            {"pkg1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                             "pkg2-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}}
        self.filesystem.status = installProgress1
        repository = MockObjects.MockRepository(data)
        self.config.repository = repository
        self.bombardier.repository = repository
        packages = self.bombardier.getPackagesToRemove(["pkg2"])
        test = ["pkg1", "pkg2"]
        for item in test:
            assert item in packages.keys(), "Dependency %s was expected in "\
                   "calculated dependency list %s" % (item, `packages.keys()`)
        scalls = self.server.getAllCalls()
        assert len(scalls) == 0, `scalls`
        fcalls = self.filesystem.getAllCalls()
        wcalls = self.windows.getAllCalls()
        rcalls = repository.getAllCalls()
        assert len(fcalls) == 1, len(fcalls)
        assert `fcalls[0]` == "getProgressData(True)", `fcalls[0]`
        assert len(wcalls) == 0
        assert len(rcalls) == 2

    def testGetPackagesToRemove(self):
        data = {}
        self.filesystem.status = {}
        repository = MockObjects.MockRepository(data)
        self.config.repository = repository
        self.bombardier.repository = repository
        status = FAIL
        packages = self.bombardier.getPackagesToRemove(["fugazi"])
        assert packages.keys() == ["fugazi"]

    def testGetVPkgNameFromPkgName(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "virtualpackage":"anypackage"},
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
                         "virtualpackage":"anypackage"},
                "pkg2": {"install": {"fullName":"pkg2-1"},
                         "virtualpackage":"anypackage"}}
        repository = MockObjects.MockRepository(data)
        vp = bombardier.BombardierClass.VirtualPackages(repository.packages)
        pkgNameSet = sets.Set(vp.getPkgNameListFromVPkgName("anypackage"))
        assert pkgNameSet == sets.Set(["pkg1", "pkg2"]), "Got back some weird set"\
             "of real package names: %s" % pkgNameSet

    def testResolveVpkgList(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "virtualpackage":"anypackage"},
                "pkg2": {"install": {"fullName":"pkg2-1"}}}
        repository = MockObjects.MockRepository(data)
        vp = bombardier.BombardierClass.VirtualPackages(repository.packages)
        pkgNameSet = sets.Set(vp.resolveVPkgList(["anypackage", "pkg2"]))
        assert pkgNameSet == sets.Set(["anypackage", "pkg2"]),"Error in resolveVPkgList:"\
               "%s" % pkgNameSet

    def testGetActualPkgName(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "virtualpackage":"anypackage"},
                "pkg2": {"install": {"fullName":"pkg2-1"}}}
        repository = MockObjects.MockRepository(data)
        vp = bombardier.BombardierClass.VirtualPackages(repository.packages)
        packageName = vp.getActualPkgName( "anypackage", ["pkg1", "pkg2"] )
        assert packageName == "pkg1", "Bad actual package name from "\
               "getActualPackageName: %s" % packageName
        
    def testRemoveVirtualPackage(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": ["anypackage"]},
                "pkg2": {"install": {"fullName":"pkg2-1"},
                         "virtualpackage":"anypackage"}}
        installProgress1 = {"install-progress":
                            {"pkg1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                             "pkg2-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}}
        repository = MockObjects.MockRepository(data)
        self.filesystem.status = installProgress1
        self.config.repository = repository
        self.bombardier.repository = repository
        shouldBeRemoved = sets.Set(["pkg1", "pkg2"])

        packages = self.bombardier.getPackagesToRemove(["pkg2"])

        flaggedForRemoval = sets.Set(packages.keys())
        assert shouldBeRemoved == flaggedForRemoval, "Uninstalling a virtual package has "\
               "failed. %s != %s" % (shouldBeRemoved, flaggedForRemoval)

        scalls = self.server.getAllCalls()
        assert len(scalls) == 0, `scalls`

        fcalls = self.filesystem.getAllCalls()
        assert len(fcalls) == 1, `fcalls`
        assert `fcalls[0]` == "getProgressData(True)", `fcalls[0]`

        wcalls = self.windows.getAllCalls()
        assert len(wcalls) == 0

        rcalls = repository.getAllCalls()
        assert len(rcalls) == 2, len(rcalls)


    def testCheckBom(self):
        installProgress2 = {"install-progress":
                            {"pkg1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                             "pkg3-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}}
        self.filesystem.status = installProgress2
        shouldBeInstalled, shouldntBeInstalled = self.bombardier.checkBom(["pkg1", "pkg2"])
        assert ["pkg2"] == shouldBeInstalled, shouldBeInstalled
        assert ["pkg3"] == shouldntBeInstalled, shouldntBeInstalled
        fcalls = self.filesystem.getAllCalls()
        assert len(fcalls) == 2, `fcalls`
        assert `fcalls[0]` == "getProgressData(True)", `fcalls[0]`
        assert `fcalls[1]`.startswith("open")
        
    def testDependenciesInstalled(self):
        data = """[pkg2]\ndep0=pkg1-1\n"""
        self.filesystem.readFiles = [StringIO.StringIO(data)]
        installedDependencies = self.bombardier.dependenciesInstalled(["pkg2"])
        assert installedDependencies == ["pkg1-1"]
        fcalls = self.filesystem.getAllCalls()
        assert len(fcalls) == 1, `fcalls`
        assert `fcalls[0]`.startswith("open")
        
    def testAddToDependencyErrors(self):
        self.filesystem.readFiles = [StringIO.StringIO()]
        self.filesystem.writeFiles = [StringIO.StringIO()]
        pkg1 = MockObjects.MockPackage()

        pkgChain = bombardier.BombardierClass.PackageChain(100, "pkg1", {"pkg1": pkg1}, ["pkg3"],[],
                                                           self.repository, self.config, 
                                                           self.filesystem, self.server, self.windows)
        status = pkgChain.addToDependencyErrors("pkg1", "pkg2")
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        wcalls = self.windows.getAllCalls()
        rcalls = self.repository.getAllCalls()
        assert status == OK
        depErrorData = self.filesystem.writeFiles[0].buflist
        assert depErrorData == ['[pkg1]\n', 'dep0 = pkg2\n', '\n']
        assert len(scalls) == 0
        assert len(fcalls) == 3, `fcalls`
        assert `fcalls[0]` == "warningLog('BOM file is incomplete: should contain pkg2', <UNPRINTABLE>)"
        assert `fcalls[1]`.startswith("open"), `fcalls[1]`
        assert `fcalls[2]`.startswith("open"), `fcalls[1]`

    def testPackageDep(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": ["pkg5"]},
                "pkg5": {"install": {"fullName":"pkg5-1"}}}

        repository = MockObjects.MockRepository(data)
        pkg1 = MockObjects.MockPackage()
        pkg1.dependencies = ["pkg5"]
        packages = {"pkg1": pkg1}
        brokenPackages = []
        newChain = bombardier.BombardierClass.PackageChain(100, "pkg1", packages, ["pkg5"], brokenPackages,
                                                           repository, self.config, self.filesystem,
                                                           self.server, self.windows)
        assert newChain.chain == ["pkg1"], newChain.chain 

    def testPackageChain(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": ["pkg5"]},
                "pkg5": {"install": {"fullName":"pkg5-1"},
                         "dependencies": ["pkg6"]},
                "pkg6": {"install": {"fullName":"pkg6-1"}}}

        repository = MockObjects.MockRepository(data)
        pkg1 = MockObjects.MockPackage()
        pkg1.dependencies = ["pkg5"]
        packages = {"pkg1": pkg1}
        brokenPackages = []
        newChain = bombardier.BombardierClass.PackageChain(100, "pkg1", packages, ["pkg2", "pkg3"],
                                                           brokenPackages, repository, self.config,
                                                           self.filesystem,
                                                           self.server, self.windows)
        assert newChain.chain == ["pkg6", "pkg5", "pkg1"], newChain.chain
        pcalls = pkg1.getAllCalls()
        fcalls = self.filesystem.getAllCalls()
        rcalls = repository.getAllCalls()
        scalls = self.server.getAllCalls()
        assert len(pcalls) == 0
        assert len(scalls) == 0, `scalls`
        assert len(rcalls) == 2
        assert len(fcalls) == 6, len(fcalls)

    def testPackageChainWithBroken(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1",
                                     "priority": '20'},
                         "dependencies": ["pkg2"]},
                "pkg2": {"install": {"fullName":"pkg2-1",
                                     "priority": '70'},
                         "dependencies": ["pkg3"]},
                "pkg3": {"install": {"fullName":"pkg3-1",
                                     "priority": '20'},
                         "dependencies": ["pkg4"]},
                "pkg4": {"install": {"fullName":"pkg4-1",
                                     "priority": '20'},
                         "dependencies": ["pkg5"]},
                "pkg5": {"install": {"fullName":"pkg5-1",
                                     "priority": '30'},
                         "dependencies": ["pkg6"]},
                "pkg6": {"install": {"fullName":"pkg6-1",
                                     "priority": '20'}}}

        repository = MockObjects.MockRepository(data)
        pkg1 = MockObjects.MockPackage()
        pkg1.dependencies = ["pkg2"]
        pkg1.priority = 20
        pkg2 = MockObjects.MockPackage()
        pkg2.dependencies = ["pkg3"]
        pkg2.priority = 70
        pkg3 = MockObjects.MockPackage()
        pkg3.dependencies = ["pkg4"]
        pkg3.priority = 20
        pkg4 = MockObjects.MockPackage()
        pkg4.dependencies = ["pkg5"]
        pkg4.priority = 20
        pkg5 = MockObjects.MockPackage()
        pkg5.dependencies = ["pkg6"]
        pkg5.priority = 30
        pkg6 = MockObjects.MockPackage()
        pkg6.priority = 20
        packages = {"pkg1": pkg1,"pkg2": pkg2,"pkg3": pkg3,"pkg4": pkg4,"pkg5": pkg5,"pkg6": pkg6}
        brokenPackages = ["pkg3"]
        newChain = bombardier.BombardierClass.PackageChain(0, "pkg1", packages, [],
                                                           brokenPackages, repository, self.config,
                                                           self.filesystem,
                                                           self.server, self.windows)
        assert newChain.chain == ["pkg6", "pkg5", "pkg4"], newChain.chain
        assert newChain.priority == 30

    def testCreatePackageChains(self):
        data = {"pkg1": {"install": {"fullName":"pkg1-1"},
                         "dependencies": ["pkg5"]},
                "pkg5": {"install": {"fullName":"pkg5-1"},
                         "dependencies": ["pkg6"]}}
        installProgress2 = {"install-progress":
                            {"pkg1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                             "pkg3-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}}
        repository = MockObjects.MockRepository(data)
        self.filesystem.status = installProgress2
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
        assert len(fcalls) == 1, `fcalls`
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
                         "dependencies": ["pkg5"]},
                "pkg5": {"install": {"fullName":"pkg5-1"},
                         "dependencies": ["pkg6"]}}
        installProgress2 = {"install-progress":
                            {"pkg1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                             "pkg3-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}}
        repository = MockObjects.MockRepository(data)
        self.filesystem.status = installProgress2
        pkg1 = MockObjects.MockPackage()
        pkg1.dependencies = ["pkg5"]
        pkg1.priority = 200
        pkg5 = MockObjects.MockPackage()
        pkg6 = MockObjects.MockPackage()
        pkg6.priority = 300
        packages = {"pkg1": pkg1, "pkg5": pkg5, "pkg6": pkg6}
        self.filesystem.status = {}
        installOrder  = self.bombardier.installList(packages)
        assert installOrder == ["pkg6", "pkg5", "pkg1"], "bad install order %s" % installOrder

    def testHandleConsole1(self):
        package = MockObjects.MockPackage()
        self.windows.testConsoleValue = FAIL
        try:
            self.bombardier.handleConsole(package)
        except SystemExit, e:
            assert e.code == REBOOT
        scalls = self.server.getAllCalls()
        assert len(scalls) == 0, `scalls`
        fcalls = self.filesystem.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert len(fcalls) == 1, len(fcalls)
        assert `fcalls[0]` == "clearLock()", `fcalls[1]`
        assert len(wcalls) == 3, wcalls
        assert `wcalls[0]` == "testConsole()"
        assert `wcalls[1]` == "autoLogin(MOCK-CONFIG)"
        assert `wcalls[2]` == "restartOnLogon()"
        
    def testHandleConsole2(self):
        package = MockObjects.MockPackage()
        status = self.bombardier.handleConsole(package)
        assert self.server.getAllCalls() == []
        wcalls = self.windows.getAllCalls()
        assert len(wcalls) == 1, wcalls
        assert `wcalls[0]` == "testConsole()", `wcalls[0]`
        assert status == OK

    def testInstallPackages(self):
        pkg1 = MockObjects.MockPackage()
        bombardier.addPackages = {"pkg1": pkg1}
        self.filesystem.status = {}
        status = self.bombardier.installPackages()
        assert status == OK, "Perfectly good package failed to install"
        assert self.server.getAllCalls() == []
        assert self.windows.getAllCalls() == []
        assert self.repository.getAllCalls() == []
        fcalls = self.filesystem.getAllCalls()
        assert len(fcalls) == 2, `fcalls`
        assert `fcalls[0]` == "getProgressData(True)", `fcalls[0]`
        assert `fcalls[1]` == "getProgressData(True)", `fcalls[1]`
        pcalls = pkg1.getAllCalls()
        assert len(pcalls) == 0, `pcalls`

    def testInstallPackagesOneBroken(self):
        pkg1 = MockObjects.MockPackage()
        pkg2 = MockObjects.MockPackage()
        pkg2.priority = 200
        pkg2.processResults = FAIL
        bombardier.addPackages = {"pkg1": pkg1, "pkg2": pkg2}
        status = self.bombardier.installPackages()
        assert status == OK, "Perfectly good package failed to install"

    def testInstallPackagesNeedingConsole(self):
        pkg1 = MockObjects.MockPackage()
        pkg1.console = True
        self.bombardier.addPackages = {"pkg1": pkg1}
        self.windows.testConsoleValue = FAIL
        self.filesystem.status = {}
        try:
            self.bombardier.installPackages()
        except SystemExit, e:
            assert e.code == REBOOT
        assert self.repository.getAllCalls() == []
        assert self.server.getAllCalls() == []
        fcalls = self.filesystem.getAllCalls()
        assert len(fcalls) == 3, `fcalls`
        assert `fcalls[2]` == "clearLock()"
        pcalls = pkg1.getAllCalls()
        assert len(pcalls) == 1, `pcalls`
        wcalls = self.windows.getAllCalls()
        assert len(wcalls) == 3, wcalls
        assert `wcalls[0]` == "testConsole()"
        assert `wcalls[1]` == "autoLogin(MOCK-CONFIG)"
        assert `wcalls[2]` == "restartOnLogon()"

    def testInstallPackagesNeedingPreboot(self):
        pkg1 = MockObjects.MockPackage()
        pkg1.preboot = True
        pkg1.name = "pkg1"
        self.bombardier.addPackages = {"pkg1": pkg1}
        self.config.freshStart = False
        self.filesystem.status = {}
        try:
            self.bombardier.installPackages()
        except SystemExit, e:
            assert e.code == REBOOT
        assert self.repository.getAllCalls() == []
        assert self.server.getAllCalls() == []
        fcalls = self.filesystem.getAllCalls()
        assert len(fcalls) == 3, fcalls
        assert `fcalls[2]` == "clearLock()"
        pcalls = pkg1.getAllCalls()
        assert len(pcalls) == 0, len(pcalls)
        wcalls = self.windows.getAllCalls()
        assert len(wcalls) == 2, wcalls
        assert `wcalls[0]` == "autoLogin(MOCK-CONFIG)"
        assert `wcalls[1]` == "restartOnLogon()"


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
        packagesData = {"pkg1": {"install": {"fullName":"pkg1-1"},
                                "dependencies": ["pkg5"]},
                       "pkg2": {"install": {"fullName":"pkg2-1"},
                                "dependencies": ["pkg6"]}}
        installProgress1 = {"install-progress":
                            {"pkg1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                             "pkg2-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}}
        repository = MockObjects.MockRepository(packagesData)
        self.config.repository = repository
        self.filesystem.status = installProgress1
        self.server.yamlRequests = [{"status":OK}, {"status":OK}, {"status":OK}]
        base = os.path.join(os.getcwd(), "packages", "pkg1-1")
        self.filesystem.directories = [os.path.join(base, "scripts"),
                                       os.path.join(base, "injector")]
        self.filesystem.files = [os.path.join(base, "scripts", "verify.py")]
        self.bombardier.repository = repository
        self.windows.verifiablePackages = ["pkg1-1"]
        testResults = self.bombardier.verifySystem()
        assert testResults.has_key("pkg1"), testResults
        assert testResults['pkg1'] == OK, testResults
        assert testResults['pkg2'] == FAIL, testResults
        assert len(testResults.keys()) == 2, "Returned more than 2 results."
        scalls = self.server.getAllCalls()
        assert len(scalls) == 1, `scalls`
        fcalls = self.filesystem.getAllCalls()
        assert len(fcalls) == 15, len(fcalls)
        #for i in range(0,len(fcalls)):
            #print i, fcalls[i]
        assert `fcalls[1]`.startswith("isdir"), `fcalls[0]`
        assert `fcalls[2]`.startswith("isfile")
        assert `fcalls[14]`.startswith("open")

    def testVerifySystem2(self):
        installProgress = {"install-progress":
                            {"pkg1-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                             "pkg2-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}}
        packagesDat = {"pkg1": {"install": {"fullName":"pkg1-1"},
                                "dependencies": ["pkg5"]},
                       "pkg2": {"install": {"fullName":"pkg2-1"},
                                "dependencies": ["pkg6"]}}
        repository = MockObjects.MockRepository(packagesDat)
        self.config.repository = repository
        self.filesystem.status = installProgress # don't want this getting clobbered
        base1 = os.path.join(os.getcwd(), "packages", "pkg1-1")
        base2 = os.path.join(os.getcwd(), "packages", "pkg2-1")
        self.filesystem.directories = [os.path.join(base1, "scripts"),os.path.join(base1, "injector"),
                                       os.path.join(base2, "scripts"),os.path.join(base2, "injector")]
        self.filesystem.files = [os.path.join(base1, "scripts", "verify.py"),
                                 os.path.join(base2, "scripts", "verify.py")]
        self.bombardier.repository = repository
        self.windows.verifiablePackages = ["pkg1-1", "pkg2-1"]
        testResults = self.bombardier.verifySystem()
        assert testResults == {"pkg1": OK, "pkg2":OK}, testResults

    def testVerifySystem3(self): # it's not time for this package to be verified
        packagesDat = {"pkg1": {"install": {"fullName":"pkg1-1"},
                                "dependencies": ["pkg5"]}}
        installProgress3 = {"install-progress":
                           {"pkg1-1": {"INSTALLED": time.ctime(),
                                       "UNINSTALLED": 'NA',
                                       "VERIFIED": time.ctime()}}}
        repository = MockObjects.MockRepository(packagesDat)
        self.config.repository = repository
        self.filesystem.status = installProgress3
        self.server.yamlRequests = [{"status":OK}, {"status":OK}, {"status":OK}]
        self.bombardier.repository = repository
        testResults = self.bombardier.verifySystem()
        assert testResults == {}, `testResults`

    def testVerifySystem4(self): # No packages installed, error in verify.
        packagesDat = {"pkg1": {"install": {"fullName":"pkg1-1"},
                                "dependencies": ["pkg5"]}}
        installProgress3 = {"install-progress":
                           {"pkg1-1": {"INSTALLED": time.ctime(),
                                       "UNINSTALLED": 'NA',
                                       "VERIFIED": time.ctime()}}}
        repository = MockObjects.MockRepository(packagesDat)
        self.config.repository = repository
        self.filesystem.progressData = {}
        self.server.yamlRequests = [{"status":OK}, {"status":OK}, {"status":OK}]
        self.filesystem.status = installProgress3
        self.bombardier.repository = repository

        testResults = self.bombardier.verifySystem()
        assert testResults == {}, `testResults`

    def testCheckInstallationStatus(self):
        self.config.data = {"bom": ["base"], "packages": ["pkg3"]}
        self.server.bom["base"] = ["pkg1", "pkg2"]
        packagesDat = {"pkg1": {"install": {"fullName":"pkg1-1", "priority":"100"}},
                       "pkg2": {"install": {"fullName":"pkg2-1", "priority":"50"},
                                "dependencies": ["pkg1"]},
                       "pkg3": {"install": {"fullName":"pkg3-1", "priority":"3"}}}
        repository = MockObjects.MockRepository(packagesDat)
        self.filesystem.status = {"install-progress":
                                  {"pkg4-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                              "UNINSTALLED": 'NA',
                                              "VERIFIED": 'Mon Apr 18 01:01:01 2005'},
                                   "pkg2-1": {"INSTALLED": 'Mon Apr 18 01:01:01 2005',
                                              "UNINSTALLED": 'NA',
                                              "VERIFIED": 'Mon Apr 18 01:01:01 2005'}}}
##         self.config.repository = repository
##         inMaintenance(self.config)
##         self.config.repository = repository
        bombardierClass = bombardier.BombardierClass.Bombardier(repository, self.config,
                                                           self.filesystem, self.server,
                                                           self.windows)
        try:
            self.bombardier.checkInstallationStatus()
        except SystemExit, e:
            assert e.code == OK

    def testReconcileSystem1(self):
        self.config.data = {"bom": ["base"], "packages": ["pkg3"]}
        self.server.bom["base"] = ["pkg1", "pkg2"]
        packagesDat = {"pkg1": {"install": {"fullName":"pkg1-1", "priority":"100"}},
                       "pkg2": {"install": {"fullName":"pkg2-1", "priority":"50"},
                                "dependencies": [ "pkg1"]},
                       "pkg3": {"install": {"fullName":"pkg3-1", "priority":"3"}}}
        repository = MockObjects.MockRepository(packagesDat)
        self.config.repository = repository
        inMaintenance(self.config)
        self.filesystem.status = {}
        base1 = os.path.join(os.getcwd(), "packages", "pkg1-1")
        base2 = os.path.join(os.getcwd(), "packages", "pkg2-1")
        base3 = os.path.join(os.getcwd(), "packages", "pkg3-1")
        self.filesystem.directories = [os.path.join(base1, "scripts"),os.path.join(base1, "injector"),
                                       os.path.join(base2, "scripts"),os.path.join(base2, "injector"),
                                       os.path.join(base3, "scripts"),os.path.join(base3, "injector")]
        self.filesystem.files = [os.path.join(base1, "scripts", "installer.py"),
                                 os.path.join(base1, "scripts", "verify.py"),
                                 os.path.join(base2, "scripts", "installer.py"),
                                 os.path.join(base2, "scripts", "verify.py"),
                                 os.path.join(base3, "scripts", "installer.py"),
                                 os.path.join(base3, "scripts", "verify.py")]

        self.bombardier.repository = repository
        self.windows.installablePackages = ["pkg1-1", "pkg2-1", "pkg3-1"]
        self.windows.verifiablePackages = ["pkg1-1", "pkg2-1", "pkg3-1"]
        
        try:
            self.bombardier.reconcileSystem()
        except SystemExit, e:
            assert e.code == OK
        
        scalls = self.server.getAllCalls()
        assert len(scalls) == 2, scalls
        assert `scalls[0]` == "clearCache()", `scalls[0]`
        fcalls = self.filesystem.getAllCalls()
        #for i in range(0,len(fcalls)):
            #print i, fcalls[i]
        assert len(fcalls) == 32, len(fcalls)
        assert `fcalls[0]`.startswith("chdir("), `fcalls[0]`
        assert `fcalls[31]` == "clearLock()", `fcalls[34]`
        wcalls = self.windows.getAllCalls()
        assert len(wcalls) == 8, len(wcalls)
        assert `wcalls[0]`.startswith('run'), `wcalls[0]`
        assert `wcalls[1]`.startswith('run'), `wcalls[0]`
        assert `wcalls[2]`.startswith('run'), `wcalls[0]`
        assert `wcalls[3]`.startswith('run'), `wcalls[0]`
        assert `wcalls[4]`.startswith('run'), `wcalls[0]`
        assert `wcalls[5]`.startswith('run'), `wcalls[0]`
        assert `wcalls[6]` == "noRestartOnLogon()", `wcalls[0]`
        assert `wcalls[7]` == "noAutoLogin()"

    def testReconcileSystemWithDependencies(self):
        self.config.data = {"bom": ["base"]}
        self.server.yamlRequests = [["pkg2"]]
        packagesDat = {"pkg1": {"install": {"fullName":"pkg1-1"}},
                       "pkg2": {"install": {"fullName":"pkg2-1"}}}
        repository = MockObjects.MockRepository(packagesDat)
        self.config.repository = repository
        inMaintenance(self.config)
        self.filesystem.status = {"install-progress": {}}
        self.filesystem.getProgressData()
        base1 = os.path.join(os.getcwd(), "packages", "pkg1-1")
        self.filesystem.directories = [os.path.join(base1, "scripts"),os.path.join(base1, "injector")]
        self.filesystem.files = [os.path.join(base1, "scripts", "uninstaller.py")]
        self.bombardier.repository = repository


        self.config.data = {"bom": ["base"]}
        self.server.bom["base"] = ["pkg2"]
        packagesDat = {"pkg1": {"install": {"fullName":"pkg1-1"}},
                       "pkg2": {"install": {"fullName":"pkg2-1"},
                                "dependencies": [ "pkg1"]}}
        repository = MockObjects.MockRepository(packagesDat)
        self.config.repository = repository
        inMaintenance(self.config)
        base1 = os.path.join(os.getcwd(), "packages", "pkg1-1")
        base2 = os.path.join(os.getcwd(), "packages", "pkg2-1")
        self.filesystem.directories = [os.path.join(base1, "scripts"),os.path.join(base1, "injector"),
                                       os.path.join(base2, "scripts"),os.path.join(base2, "injector")]
        self.filesystem.files = [os.path.join(base1, "scripts", "installer.py"),
                                 os.path.join(base1, "scripts", "verify.py"),
                                 os.path.join(base2, "scripts", "installer.py"),
                                 os.path.join(base2, "scripts", "verify.py")]
        self.filesystem.readFiles = [StringIO.StringIO("[pkg2]\ndep0=pkg1")]
        self.bombardier.repository = repository

        self.windows.installablePackages = ["pkg1-1", "pkg2-1"]
        self.windows.verifiablePackages = ["pkg1-1", "pkg2-1"]
        try:
            self.bombardier.reconcileSystem()
        except SystemExit, e:
            assert e.code == OK
        wcalls = self.windows.getAllCalls()
        assert len(wcalls) == 6, len(wcalls)
        assert `wcalls[0]`.startswith('run'), `wcalls[0]`
        assert `wcalls[0]`.find('installer')>0, `wcalls[0]`
        assert `wcalls[1]`.startswith('run'), `wcalls[1]`
        assert `wcalls[1]`.find('verify')>0, `wcalls[1]`
        assert `wcalls[2]`.startswith('run'), `wcalls[2]`
        assert `wcalls[2]`.find('installer')>0, `wcalls[2]`
        assert `wcalls[3]`.startswith('run'), `wcalls[3]`
        assert `wcalls[3]`.find('verify')>0, `wcalls[3]`
        assert `wcalls[4]`.startswith('noRestartOnLogon'), `wcalls[4]`
        assert `wcalls[5]`.startswith('noAutoLogin'), `wcalls[5]`
        fcalls = self.filesystem.getAllCalls()
        assert len(fcalls) == 26, len(fcalls)

    def testReconcileSystemBogus(self):
        self.config.data = {"bom": ["base"]}
        installProgress3 = {"install-progress":
                           {"pkg1-1": {"INSTALLED": time.ctime(),
                                       "UNINSTALLED": 'NA',
                                       "VERIFIED": time.ctime()}}}
        self.server.bom["base"] = ["pkg1"]
        packagesDat = {"pkg1": {"install": {"fullName":"pkg1-1"}}}
        repository = MockObjects.MockRepository(packagesDat)
        self.config.repository = repository
        inMaintenance(self.config)
        self.filesystem.status = installProgress3
        base1 = os.path.join(os.getcwd(), "packages", "pkg1-1")
        self.filesystem.directories = [os.path.join(base1, "scripts"),os.path.join(base1, "injector")]
        self.filesystem.files = [os.path.join(base1, "scripts", "uninstaller.py")]
        self.bombardier.repository = repository
        try:
            self.bombardier.reconcileSystem()
        except SystemExit, e:
            assert e.code == OK

    def testCheckSystem(self):
        #^ get the system to believe that it has some package installed
        now = time.ctime()
        installProgress = {"install-progress":
                           {"reconfig-1": {"INSTALLED": now,
                                           "UNINSTALLED": 'NA',
                                           "VERIFIED": now},
                           "stable-1": {"INSTALLED": now,
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": now}}}
        self.filesystem.status = installProgress
        #^ Let the system know that the packages care about config data
        packages = {"reconfig": {"install": {"fullName":"reconfig-1"},
                                 "configuration": {"section1":"option1"}},
                    "stable": {"install": {"fullName":"stable-1"},
                               "configuration": {"section2":"option2"}}}
        repository = MockObjects.MockRepository(packages)

        #^ get the system to believe it has a configuration fingerprint saved
        oldConfigData = {"section1": {"option1": "spam"}, "section2": {"option2": "eggs"}}
        self.config.savedYamlData["reconfig"] = oldConfigData
        self.config.savedYamlData["stable"] = oldConfigData

        #^ set the current fingerprint to something different
        self.config.data = {"section1": {"option1": "foo"}, "section2": {"option2": "eggs"}}
        self.config.data["packages"] = ["reconfig", "stable"]

        #^ have the system report that the package with different config data needs attention
        self.config.repository = repository
        self.bombardier.repository = repository
        packageData = self.bombardier.checkSystem()
        assert packageData["reconfigure"] == {'reconfig': ['/section1']}, packageData
        assert packageData["ok"] == ["stable"], packageData

    def testCheckSystem2(self):
        now = time.ctime()
        installProgress = {"install-progress":
                           {"reconfig-1": {"INSTALLED": now,
                                           "UNINSTALLED": 'NA',
                                           "VERIFIED": now},
                           "stable-1": {"INSTALLED": now,
                                        "UNINSTALLED": 'NA',
                                        "VERIFIED": now}}}
        self.filesystem.status = installProgress
        packages = {"reconfig": {"install": {"fullName":"reconfig-1"},
                                 "configuration": {}},
                    "stable": {"install": {"fullName":"stable-1"},
                               "configuration": {"section2":"option2"}}}
        repository = MockObjects.MockRepository(packages)
        oldConfigData = {"section1": {"option1": "spam"}, "section2": {"option2": "eggs"}}
        self.config.savedYamlData["reconfig"] = oldConfigData
        self.config.savedYamlData["stable"] = oldConfigData
        self.config.data = {"section1": {"option1": "foo"}, "section2": {"option2": "eggs"}}
        self.config.data["packages"] = ["reconfig", "stable"]
        self.config.repository = repository
        self.bombardier.repository = repository
        packageData = self.bombardier.checkSystem()
        assert packageData["reconfigure"] == {}, packageData
        assert "reconfig" in packageData["ok"], packageData
        
        
if __name__ == "__main__":
    tcommon = Tcommon.Tcommon()
    tcommon.setForTest()
    suite = unittest.TestSuite()
    #suite.addTest(BombardierTest("testRemoveVirtualPackage"))
    #suite.addTest(BombardierTest("testReconcileSystemBogus"))
    #suite.addTest(BombardierTest("testGetPackagesToAdd1"))
    #suite.addTest(BombardierTest("testAddToDependencyErrors"))
    #suite.addTest(BombardierTest("testAddToDependencyErrors"))
##     suite.addTest(BombardierTest("testAddToDependencyErrors"))
##     suite.addTest(BombardierTest("testBogusDependency"))
##     suite.addTest(BombardierTest("testCheckBom"))
##     suite.addTest(BombardierTest("testCreatePackageChains"))
##     suite.addTest(BombardierTest("testDependenciesInstalled"))
##     suite.addTest(BombardierTest("testDownloadBomBadgroup"))
##     suite.addTest(BombardierTest("testDownloadBomEmpty"))
##     suite.addTest(BombardierTest("testDownloadBomGoofy1"))
##     suite.addTest(BombardierTest("testDownloadBomGoofy2"))
##     suite.addTest(BombardierTest("testDownloadBomMultiple"))
##     suite.addTest(BombardierTest("testDownloadBomPathological"))
    #suite.addTest(BombardierTest("testDownloadBomSimple"))
##     suite.addTest(BombardierTest("testGetActualPkgName"))
##     suite.addTest(BombardierTest("testGetPackagesToAdd1"))
##     suite.addTest(BombardierTest("testGetPackagesToAdd2"))
##     suite.addTest(BombardierTest("testGetPackagesToRemove"))
##     suite.addTest(BombardierTest("testGetPkgNameListFromVPkgName"))
##     suite.addTest(BombardierTest("testGetTopPriority"))
##     suite.addTest(BombardierTest("testGetVPkgNameFromPkgName"))
##     suite.addTest(BombardierTest("testHandleConsole1"))
##     suite.addTest(BombardierTest("testHandleConsole2"))
##     suite.addTest(BombardierTest("testInMaintenanceWindow"))
##     suite.addTest(BombardierTest("testInstallList"))
    ## suite.addTest(BombardierTest("testInstallPackagesNeedingConsole"))
    ## suite.addTest(BombardierTest("testInstallPackagesNeedingPreboot"))
##     suite.addTest(BombardierTest("testPackageChainWithBroken"))
##     suite.addTest(BombardierTest("testPackageDep"))
    #suite.addTest(BombardierTest("testReconcileSystem1"))
    #suite.addTest(BombardierTest("testReconcileSystemWithDependencies"))
    #suite.addTest(BombardierTest("testGetPackagesToRemove1"))
    #suite.addTest(BombardierTest("testVerifySystem1"))
    #suite.addTest(BombardierTest("testCheckConfig uration"))
    #suite.addTest(BombardierTest("testCheckSystem2"))
    suite.addTest(unittest.makeSuite(BombardierTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
    tcommon.unsetForTest()
