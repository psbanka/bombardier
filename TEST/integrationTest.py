#!c:\Python24\python.exe

import os, sys, unittest, shutil, string, yaml, sets, threading, time, tarfile
sys.path = [os.path.join("..", "client"), os.path.join('..', 'spkgDir')] + sys.path

import MockObjects, Tcommon

import bombardier.Exceptions as Exceptions
import bombardier.Package as Package
import bombardier.Config as Config
import bombardier.Logger as Logger
import bombardier.Repository as Repository
import bombardier.CommSocket as CommSocket
from bombardier.staticData import *

import bombardier.Server as Server
import bombardier.Filesystem as Filesystem

if sys.platform == "linux2":
    import bombardier.Linux as Linux
    operatingSystem = Linux.Linux()
else:
    import bombardier.Windows as Windows
    operatingSystem = Windows.Windows()

import bombardier.miniUtility as miniUtility
import bombardier.BombardierClass as BombardierClass

TEST_PATH    = os.getcwd()
NORMAL_PATH  = miniUtility.getSpkgPath()

logger     = Logger.Logger()
filesystem = Filesystem.Filesystem()
server     = Server.Server(filesystem)
config     = Config.Config(filesystem, server, operatingSystem)
repo       = Repository.Repository(config, filesystem, server)
server.getServerData()
repo.getPackageData()
config.freshen()

class CommandThread(threading.Thread):
    def __init__(self, command, sleepTime):
        threading.Thread.__init__(self)
        self.sleepTime = sleepTime
        self.command = command

    def run(self):
        #print "STARTING!!"
        counter = 0
        while counter < self.sleepTime:
            #print "waiting...", counter, self.sleepTime
            counter += 1
            time.sleep(1)
        #print "RUNNING COMMAND"
        self.command()

def generateInstallProgress(condition):
    packages = ['testbadverifypackage1-1','testokpackage1-1']
    installDateString = 'Mon Apr 18 01:01:01 2005'
    doVerifyDateString = 'Mon Apr 18 01:01:01 2005'
    # conditions will be 
    # 1 - Both packages will run through verify
    # 2 - first package will run through verify
    # 3 - second package will run through verify
    # 4 - No packages will run through verify
    tmp = {}
    for pkg in packages:
        tmp[pkg] = { 'INSTALLED': installDateString,
                     'UNINSTALLED': 'NA',
                     'VERIFIED': doVerifyDateString }
    if condition == 2:
        tmp[packages[1]]['VERIFIED'] = time.ctime()
    elif condition == 3:
        tmp[packages[0]]['VERIFIED'] = time.ctime()
    elif condition == 4:
        for pkg in packages:
          tmp[pkg]['VERIFIED'] = time.ctime()

    fh = open(STATUS_FILE, 'w')
    yaml.dumpToFile(fh, {"install-progress": tmp,
                         "status": {"action": "None", "main":"None",
                                    "overall": "None", "package": "None",
                                    "percentage":0},
                         "timestamp":0})
    fh.close()
    
installProgressYml1 = """
---
testbadverifypackage1-1:
  INSTALLED: Mon Apr 18 01:01:01 2005
  UNINSTALLED: NA
  VERIFIED: 
testokpackage1-1:
  INSTALLED: Mon Apr 18 01:01:01 2005
  UNINSTALLED: NA
  VERIFIED: """

installProgressYml2 = """
---
testbadverifypackage1-1:
  INSTALLED: Mon Apr 18 01:01:01 2005
  UNINSTALLED: NA
  VERIFIED: 
testokpackage1-1:
  INSTALLED: Mon Apr 18 01:01:01 2005
  UNINSTALLED: NA
  VERIFIED: 
"""

configData2 = """
packageGroups:
  - base
  - test
  - cheeze

include:
  - margaret
  - bill
"""

def inMaintenance():
    day     = time.strftime("%a")
    clock   = time.strftime("%H:%M")
    config.set("system", "maintenanceWindow", "%s %s 10" % (day, clock))

def outOfMaintenance():
    day     = time.strftime("%a")
    clock   = time.strftime("%H:%M")
    config.set("system", "maintenanceWindow", "%s %s 0" % (day, clock))

class BombardierTest(unittest.TestCase):

    def setUp(self):
        self.startPath = os.getcwd()
        self.bombardier = BombardierClass.Bombardier(repo, config,
                                                     filesystem, server, operatingSystem)
        if os.path.isfile(INSTALL_LOCK):
            os.unlink(INSTALL_LOCK)
            
    def tearDown(self):
        os.chdir(self.startPath)
        purgeFiles = ["utility", "Config", "staticData", "spkg"]
        for inode in purgeFiles:
            if os.path.isfile(inode+".py"):
                os.unlink(inode+".py")
            if os.path.isfile(inode+".pyc"):
                os.unlink(inode+".pyc")
        assert not os.path.isfile("localConfig.ini"), "that damn file showed up again"

    def testGetPackageGroups(self):
        config.data = yaml.load(configData2).next()
        packageGroups, packages = config.getPackageGroups()
        assert ["base", "test", "cheeze"] == packageGroups, "Invalid packages "\
               "In current package group: %s" % packageGroups
        exceptionCaught = False
        bom = self.bombardier.downloadBom(packageGroups)
        assert "system-setup" in bom, bom
        config.freshen()

    # WORKING
    def testGetPackagesToAdd(self):
        Logger.critical("=======================GETPACKAGESTOADD")
        open(miniUtility.getProgressPath(), 'w').write("---\nfoomanchoo-1: INSTALLED\n")
        progressPath = miniUtility.getProgressPath()
        packages = self.bombardier.getPackagesToAdd(["testdependencies1"])
        assert packages.keys() == ["testdependencies1"], packages.keys()
        packages = self.bombardier.getPackagesToAdd(["testokpackage1"])
        assert packages.keys() == ["testokpackage1"], packages.keys()
        packages = self.bombardier.getPackagesToAdd(["fugazi"])
        assert packages.keys() == [], packages.keys()

    # WORKING
    def testGetPackagesToRemove(self):
        Logger.critical("=======================GETPACKAGESTOREMOVE")
        currentBom = setBom([])
        setInstallProgress(["testokpackage1-1", "testdependencies1-1"])
        packages = self.bombardier.getPackagesToRemove(["testdependencies1"])
        assert packages.keys() == ["testdependencies1"], "Expected that removing 'testdependencies1'"\
               "would result in no further packages to be removed. However, this was listed: %s"\
               % packages.keys()
        packages = self.bombardier.getPackagesToRemove(["testokpackage1"])
        test = ["testokpackage1", "testdependencies1"]
        for item in test:
            assert item in packages.keys(), "Dependency %s was expected in "\
                   "calculated dependency list %s" % (item, `packages.keys()`)
        status = FAIL
        packages = self.bombardier.getPackagesToRemove(["fugazi"])
        assert packages["fugazi"].status == FAIL
        setBom(currentBom)

    def testVirtualPackages(self):
        vp = BombardierClass.VirtualPackages(repo.packages)
        vPkgName = vp.getVPkgNameFromPkgName("testdb-structure-1")
        assert vPkgName == "testdb-structure",\
               "Got back a weird virtual name for testdb-structure-1: %s" % vPkgName
        vPkgName2 = vp.getVPkgNameFromPkgName("testokpackage1")
        assert vPkgName2 == "testokpackage1",\
               "Got back a weird virtual name for testokpackage1: %s" % vPkgName2
        vPkgName3 = vp.getVPkgNameFromPkgName("shrubbery")
        assert vPkgName3 == "shrubbery", \
               "Got back a weird virtual name for nonsense package: %s" % vPkgName3
        pkgNameSet = set(vp.getPkgNameListFromVPkgName("testdb-structure"))
        assert pkgNameSet == set(["testdb-structure-1", "testdb-structure-2"]), \
               "Got back some weird set of real package names: %s" % pkgNameSet
        pkgNameSet = set(vp.resolveVPkgList(["testdb-structure-1",
                                                  "testdb-data-initial"]))
        assert pkgNameSet == set(["testdb-structure", "testdb-data-initial"]), \
               "Error in resolveVPkgList: %s" % pkgNameSet
        packageName = vp.getActualPkgName( "testdb-structure",
                                           ["testdb-structure-1", "testdb-data-initial"] )
        assert packageName == "testdb-structure-1", "Bad actual package name from "\
               "getActualPackageName: %s" % packageName
        
    def testRemoveVirtualPackage(self):
        Logger.critical("=======================REMOVEVIRTUALPACKAGE")
        currentBom = setBom(["testdb-data"])
        # testdb-structure-1-1 is a testdb-structure virtual package.
        setInstallProgress(["testdb-structure-1-1", "testdb-data-initial-1"])
        packages = self.bombardier.getPackagesToRemove(["testdb-structure-1"])
        testSet = set(["testdb-structure-1", "testdb-data-initial"])
        packageSet = set(packages.keys())
        assert testSet == packageSet, "Uninstalling a virtual "\
               "package has failed. %s != %s" % (testSet, packageSet)
        setBom(currentBom)

    def testCheckBom(self):
        setInstallProgress(["ipsettings-1", "testuninstalledpackage1-1"])
        shouldBeInstalled, shouldntBeInstalled = self.bombardier.checkBom(["ipsettings","bginfo"])
        testShould = "bginfo"
        testShouldnt = "testuninstalledpackage1"
        testIgnored  = "ipsettings"
        assert testShould in shouldBeInstalled, `testShould, ':::', shouldBeInstalled`
        assert testShouldnt in shouldntBeInstalled, `testShouldnt, ':::', shouldntBeInstalled`
        assert testIgnored not in shouldntBeInstalled, `testIgnored, ':::', shouldntBeInstalled`

    # WORKING
    def testAddToDependencyErrors(self):
        Logger.critical("=======================ADDTOBOM")
        open("dependency-errors.ini", 'w').write('')
        if os.path.isfile("BOM.txt"):
            os.unlink("BOM.txt")
        pkg1 = MockObjects.MockPackage()
        pkgChain = BombardierClass.PackageChain(100, "pkg1", {"pkg1": pkg1}, ["pkg3"],
                                           [], repo, config, 
                                           filesystem, server, operatingSystem)
        status = pkgChain.addToDependencyErrors("Hotfix-manager", "foomanchoo")
        assert status == OK, "Unable to add entry to BOM"
        data = open("dependency-errors.ini", 'r').readlines()
        assert data[0].strip() == "[Hotfix-manager]", "invalid data: (%s)" % data[0]
        assert data[1].strip() == "dep0 = foomanchoo"
        assert len(data) == 3, len(data)
        status = pkgChain.addToDependencyErrors("Hotfix-manager", "foomanchoo")
        assert status == OK, "Added an entry twice to the BOM"

    # WORKING
    def testInstallList(self):
        Logger.critical("=======================INSTALLLIST")
        config2 = Config.Config(filesystem, server, operatingSystem)
        server2 = Server.Server(filesystem, {"address": "http://localhost:123"} )
        setInstallProgress(['testbadverifypackage1-1'])
        status = FAIL
        try:
            repo2 = Repository.Repository(config2, filesystem, server2)
            repo2.getPackageData()
        except Exceptions.ServerUnavailable:
            status = OK
        assert status == OK, "Failed to throw an exception when server was not available"
        config.console = False
        package0 = Package.Package("testokpackage1", repo, config, filesystem, server, operatingSystem)
        package0.initialize()
        package1 = Package.Package("testdependencies1", repo, config, filesystem, server, operatingSystem)
        package1.initialize()
        package2 = Package.Package("testbadpackage1", repo, config, filesystem, server, operatingSystem)
        package2.initialize()
        status = FAIL
        try:
            package3 = Package.Package("cheese", repo, config,
                                       filesystem, server, operatingSystem)
            package3.initialize()
        except Exceptions.BadPackage, e:
            assert e.errmsg == "No metadata found for this package"
            status = OK
        assert status == OK
        package5 = Package.Package("testconfigpkg1", repo, config, filesystem, server, operatingSystem)
        package5.initialize()
        assert package0.status == OK, package0.status
        assert package1.status == OK, package1.status
        assert package2.status == OK, package2.status
        assert package5.status == OK, package5.status
        assert package1.priority == 150, "package1.priority (%d) != %d" %(`package1.priority`, 150)
        assert package5.priority == 50, package5.priority
        packages = {"testconfigpkg1":package5, "testdependencies1":package1, "testokpackage1":package0}
        chains = self.bombardier.createPackageChains(packages)
        assert len(chains) == 3, chains
        installOrder  = self.bombardier.installList(packages)
        assert installOrder == ["testokpackage1", "testdependencies1", "testconfigpkg1"], installOrder
                                

    # WORKING
    def testDownloadBom(self):
        Logger.critical("=======================DOWNLOADBOM")
        config.console = False
        exceptionCaught = False
        try:
            status = self.bombardier.downloadBom(["foo"])
        except Exceptions.BadBillOfMaterials:
            exceptionCaught = True
        assert exceptionCaught == True, "Bad system type returned success"
        exceptionCaught = False
        try:
            status = self.bombardier.downloadBom([""])
        except Exceptions.BadBillOfMaterials:
            exceptionCaught = True
        assert exceptionCaught == True, "Empty system type returned OK"
        exceptionCaught = False
        exceptionCaught = False
        try:
            status = self.bombardier.downloadBom(12)
        except Exceptions.BadBillOfMaterials:
            exceptionCaught = True
        assert exceptionCaught == True, "Bad pkgGroup returned success"        

    # WORKING FIXME: Fails on Shawn's machine
    def testZProcessPackages(self):
        Logger.critical("=======================PROCESSPACKAGES")
        operatingSystem.DEBUG = True
        config.console = False
        progressPath = miniUtility.getProgressPath()
        package = Package.Package("testokpackage1", repo, config,
                                  filesystem, server, operatingSystem)
        package.initialize()
        assert package.status == OK, package.status
        packages = {"testokpackage1":package}
        setInstallProgress(["testanotherpackage1-1"], [])
        status = self.bombardier.installPackages(packages)
        assert status == OK, "Perfectly good package failed to install"
        installProgress = yaml.loadFile(progressPath).next().get("install-progress")
        matches = installProgress.get("testokpackage1-1")
        assert matches["INSTALLED"], "didn't find what we wanted in %s. "\
               "We wanted ['testokpackage1-1: INSTALLED'], "\
               "but we got %s" % (progressPath, matches)
        matches = installProgress.get("testanotherpackage1-1")
 
        package2 = Package.Package("testconsolepackage1", repo,
                                   config, filesystem, server, operatingSystem)
        package2.initialize()
        packages = {"testconsolepackage1":package2}
        outOfMaintenance()
        assert package2.status == OK, "Package status "\
               "indicated failure to process"
        status = self.bombardier.installPackages(packages)
        installProgress = yaml.loadFile(progressPath).next().get("install-progress")
        matches = installProgress.get("testconsolepackage1-13")
        assert matches["INSTALLED"] != "NA", "Console package installed "\
               "outside of a maintenance window. (%s)" % matches

    # WORKING
    def testInMaintenanceWindow(self):
        Logger.critical("=======================INMAINTENANCEWINDOW")
        day = time.strftime("%a")
        clock = time.strftime("%H:%M")
        inMaintenance()
        status = self.bombardier.inMaintenanceWindow()
        assert status == True, "System should indicate "\
               "that we are in a maintenance window"
        outOfMaintenance()
        status = self.bombardier.inMaintenanceWindow()
        assert status == False, "System should indicate "\
               "that we are not in a maintenance window"
        config.set("system", "maintenanceWindow", "%s 99:00 10" % (day))
        status = self.bombardier.inMaintenanceWindow()
        assert status == False, "System should indicate "\
               "that we are not in a maintenance window"
        if day == "Sun":
            day = "Mon"
        else:
            day = "Sun"
        config.set("system", "maintenanceWindow", "%s %s 10" % (day, clock))
        status = self.bombardier.inMaintenanceWindow()
        assert status == False, "System should indicate that "\
               "we are not in a maintenance window"
        config.set("system", "maintenanceWindow", "%s 99:00 10" % (day))
        status = self.bombardier.inMaintenanceWindow()
        assert status == False, "System should indicate that we "\
               "are not in a maintenance window"
        config.set("system", "maintenanceWindow", "Foo 00:00 10")
        status = self.bombardier.inMaintenanceWindow()
        assert status == False, "System should indicate that we "\
               "are not in a maintenance window"
        config.set("system", "maintenanceWindow", "Sun happy:00 10")
        status = self.bombardier.inMaintenanceWindow()
        assert status == False, "System should indicate that we "\
               "are not in a maintenance window"
        config.set("system", "maintenanceWindow", "cheese")
        status = self.bombardier.inMaintenanceWindow()
        assert status == False, "System should indicate that we are "\
               "not in a maintenance window"


    def testVerifySystem1(self):
        generateInstallProgress(1)
        commSocket = CommSocket.CommSocket()
        messages   = self.bombardier.verifySystem(commSocket.testStop)
        assert FAIL in messages.values(), "testbadverifypackage1 should have failed"
        assert messages.has_key("testokpackage1")
        assert messages['testokpackage1'] == OK, \
               'messages dict is corrupt for testokpackage1'
        assert messages['testbadverifypackage1'] == FAIL, \
               'messages dict is corrupt for testbadverifypackage1'
        assert len(messages.keys()) == 2, "Returned more then 2 results."
        #shutil.copyfile("install-progress.bak","install-progress.yml")

    def testVerifySystem2(self):
        generateInstallProgress(2)
        commSocket = CommSocket.CommSocket()
        messages   = self.bombardier.verifySystem(commSocket.testStop)
        assert FAIL in messages.values(),"testbadverifypackage should have failed"
        assert len(messages.keys()) == 1
        #shutil.copyfile("install-progress.bak","install-progress.yml")

    def testVerifySystem3(self):
        generateInstallProgress(3)
        commSocket = CommSocket.CommSocket()
        messages   = self.bombardier.verifySystem(commSocket.testStop)
        assert not FAIL in messages.values(), "testokpackage1 should have passed"
        assert len(messages.keys()) == 1
        #shutil.copyfile("install-progress.bak","install-progress.yml")

    def testVerifySystem4(self):
        generateInstallProgress(4)
        commSocket = CommSocket.CommSocket()
        messages   = self.bombardier.verifySystem(commSocket.testStop)
        assert not FAIL in messages.values(), "no packages to verify, returns ok"
        assert len(messages.keys()) == 0
        #shutil.copyfile("install-progress.bak","install-progress.yml")


    def testReconcileSystem(self):
        Logger.critical("=======================TESTRECONCILESYSTEM")
        commSocket = CommSocket.CommSocket()
        inMaintenance()
        assert self.bombardier.inMaintenanceWindow() == True, \
               "NOT in Maintenance window; cannot install"
        open("dependency-errors.ini", 'w').write('')
        currentBom = setBom(["testokpackage1"])
        setInstallProgress([])
        status = self.bombardier.reconcileSystem(commSocket.testStop, packageNames = ["testokpackage1"])
        assert status == OK, "Simple package installation failed"
        checkInstallProgress(["testokpackage1"])
        setBom([])
        setInstallProgress(["testokpackage1-1"])
        Logger.critical("===ROUND 2===")
        setBom([])
        status = self.bombardier.reconcileSystem(commSocket.testStop, packageNames = [])
        assert status == OK, "Simple package un-installation failed"
        checkInstallProgress([])
        packageNames = ["testokpackage1", "testokpackage1"]
        setInstallProgress([])
        status = self.bombardier.reconcileSystem(commSocket.testStop, packageNames = packageNames)
        checkInstallProgress(["testokpackage1"])
        setBom(["nonsense"])
        setInstallProgress([])
        status = self.bombardier.reconcileSystem(commSocket.testStop, packageNames = ["nonsense"])
        checkInstallProgress([])
        assert status == OK, "System said it installed a nonexistant package"

        if os.path.isfile(INSTALL_LOCK):
            os.unlink(INSTALL_LOCK)
        setBom(["testdependencies1"])
        setInstallProgress([])
        Logger.critical("===ROUND 3===")
        status = self.bombardier.reconcileSystem(commSocket.testStop, packageNames = ["testdependencies1"])
        assert status == OK, "system could not install two packages"
        checkInstallProgress(["testokpackage1", "testdependencies1"])
        depNames = self.bombardier.dependenciesInstalled(["testdependencies1"])
        assert depNames == ["testokpackage1"], "Incorrect depencencies identified: %s" % depNames
        Logger.critical("==========")
        # KNOWN TO FAIL:
        setBom(currentBom)

    def testAbort(self):
        Logger.critical("=======================ABORT")
        operatingSystem.DEBUG = True
        setInstallProgress([])
        package = Package.Package("testdelaypackage-1", repo, config, filesystem, server, operatingSystem)
        package.initialize()
        assert package.status == OK, package.status
        packages = {"testdelaypackage-1":package}
        commSocket = CommSocket.CommSocket()
        self.bombardier.testStop = commSocket.testStop
        killThread = CommandThread(commSocket.sendStop, 3)
        killThread.start()
        throwCheck = False
        try:
            status = self.bombardier.installPackages(packages)
        except Exceptions.StoppedExecution:
            throwCheck = True
        assert throwCheck == True

    def testPackageIntegration(self):
        logger.info("=======================testInstallUninstallVerify")
        commSocket = CommSocket.CommSocket()
        startPath = os.getcwd()
        # INSTALL
        status = FAIL
        try:
            package = Package.Package("foo", repo, config,
                                      filesystem, server, operatingSystem)
            package.initialize()
        except Exceptions.BadPackage, e:
            assert e.errmsg == "No metadata found for this package"
            status = OK
        assert status == OK
        package = Package.Package("testokpackage1", repo, config, filesystem, server, operatingSystem)
        package.initialize()
        status = package.install(["testokpackage1"], commSocket.testStop)
        assert status == OK, "Legitimate package installation failed."

        assert startPath != os.getcwd, "Installer did not change path"
        os.chdir(startPath)
        package = Package.Package("testbadpackage1", repo, config, filesystem, server, operatingSystem)
        package.initialize()
        status = package.install(["testbadpackage1"], commSocket.testStop)
        assert status == FAIL, "Package that returns an error succeeded."
        os.chdir(startPath)
        # UNINSTALL
        package = Package.Package("testbaduninstallpackage1", repo, config, filesystem, server, operatingSystem)
        package.initialize()
        status = package.uninstall(commSocket.testStop)
        assert status == FAIL, "uninstallation of a bogus package succeeded"
        os.chdir(startPath)
        package = Package.Package("testokpackage1", repo, config, filesystem, server, operatingSystem)
        package.initialize()
        status = package.uninstall(commSocket.testStop)
        assert status == OK, "Legitimate package uninstallation failed"
        os.chdir(startPath)
        package = Package.Package("testbaduninstallpackage1", repo, config, filesystem, server, operatingSystem)
        package.initialize()
        status = package.uninstall(commSocket.testStop)
        assert status == FAIL, "Uninstallation of a package that returns an error succeeded"
        # VERIFY
        package = Package.Package("testnoverifypackage1", repo, config, filesystem, server, operatingSystem)
        package.initialize()
        status = package.verify(commSocket.testStop)
        assert status == FAIL, "verification of a bogus package succeeded"
        os.chdir(startPath)
        package = Package.Package("testokpackage1", repo, config, filesystem, server, operatingSystem)
        package.initialize()
        status = package.verify(commSocket.testStop)
        assert status == OK, "Legitimate package verification failed"
        os.chdir(startPath)
        package = Package.Package("testbadverifypackage1", repo, config, filesystem, server, operatingSystem)
        package.initialize()
        status = package.verify(commSocket.testStop)
        assert status == FAIL, "Verification of a package that returns an error succeeded"
        logger.info("=======================testInstallUninstallVerify")

    def testBackupUninstall(self):
        if sys.platform == "linux2":
            return
        Logger.critical("=======================TESTBACKUPUNINSTALL")
        packageNames = ["testdb-structure-1", "testdb-data-initial"]
        commSocket = CommSocket.CommSocket()
        currentBom = setBom(packageNames)
        setInstallProgress([])
        status = self.bombardier.reconcileSystem(commSocket.testStop, packageNames)
        assert status == OK, "Installation of databases failed" 

        setInstallProgress(["testdb-structure-1-1", "testdb-data-initial-1"])
        Logger.critical("=============Remove structure out from under")
        packageNames = ["testdb-structure-2", "testdb-data-initial"]
        setBom(packageNames)
        status = self.bombardier.reconcileSystem(commSocket.testStop, packageNames)
        assert status == OK, "Upgrading the structure package failed." 
        checkInstallProgress(["testdb-structure-2", "testdb-data-initial"])
        setBom(currentBom)

    def testGetDetailedTodolist(self):
        # basic test case, no surprises
        conf = MockObjects.MockConfig()
        conf.data = {"packageGroups": ["base"]}
        installList = ["ipsettings", "system-setup"]
        self.bombardier.config = conf
        setBom(["ipsettings", "system-setup"])
        source = self.bombardier.getSources(["ipsettings", "system-setup"])
        assert source["ipsettings"] == ["base"], source["ipsettings"]
        assert source["system-setup"] == ["base"]

        todolist = self.bombardier.getDetailedTodolist(installList)
        assert len(todolist) ==2
        assert todolist[0] == "ipsettings,base", todolist
        filesystem.updateProgress({"todo": todolist}, server, True)
        statusData = yaml.loadFile(os.path.join(miniUtility.getSpkgPath(), STATUS_FILE)).next()
        assert statusData["todo"] == ["ipsettings,base","system-setup,base"], statusData["todo"]

        # coming from more than one source
        conf.data = {"packageGroups": ["base", "test"]}
        todolist = self.bombardier.getDetailedTodolist(installList)
        assert todolist[1] == "system-setup,base/test", todolist
        filesystem.updateProgress({"todo": todolist}, server, True)

        # coming from no sources
        installList.append("package3")
        todolist = self.bombardier.getDetailedTodolist(installList)
        assert "package3,<<dependency>>" in todolist, todolist
        filesystem.updateProgress({"todo": todolist}, server, True)
        
    
def checkServerTarball(fileName):
    status = server.wget("deploy", fileName, config)
    assert status == OK, "Unable to download the new uploaded backup package %s" % fileName
    tar = tarfile.open( fileName, "r:gz" )
    tarMembers = tar.getmembers()
    assert len(tarMembers) > 1, "Empty tarfile (%s)" % tarMembers
    tar.close()
    os.unlink(fileName)

def checkInstallProgress(checkList):
    progressData  = filesystem.getProgressData(True)
    installedList, brokenList = miniUtility.getInstalled(progressData)
    for packageName in checkList:
        assert packageName in installedList, "%s not installed (%s) | path: %s" % \
               (packageName, installedList, miniUtility.getProgressPath())
        installedList.remove(packageName)
    assert installedList == [], "Extra packages %s installed" % installedList
    return

def setInstallProgress(installList, uninstallList=[]):
    installProgress = {}
    for item in installList:
        installProgress[item] = {"INSTALLED":time.ctime(),
                                 "VERIFIED":time.ctime(),
                                 "UNINSTALLED": 'NA'}
    for item in uninstallList:
        installProgress[item] = {"INSTALLED":"NA",
                                 "VERIFIED":time.ctime(),
                                 "UNINSTALLED": time.ctime()}
    fh = open(miniUtility.getProgressPath(), 'w')
    yaml.dumpToFile(fh, {"install-progress": installProgress,
                         "status": {"action": "None", "main":"None",
                                    "overall": "None", "package": "None",
                                    "percentage":0},
                         "timestamp":0})
    fh.flush()
    fh.close()

def setBom(bomList):
    currentBom = server.serviceYamlRequest("deploy/bom/base.yml", legacyPathFix=False)
    status = server.serviceYamlRequest("deploy/bom/base.yml", putData=bomList, legacyPathFix=False)
    del server.cache["deploy/bom/base.yml"]
    newBom = "error"
    if status == "OK":
        newBom = server.serviceYamlRequest("deploy/bom/base.yml", legacyPathFix=False)
    if newBom != bomList:
        print "UNABLE TO SET BOM"
        print "currentBom:",currentBom
        print "tried to set to:",bomList
        print "but it is:",newBom
        sys.exit(1)
    return currentBom

if __name__ == "__main__":
    tcommon = Tcommon.Tcommon()
    tcommon.setForTest()
    operatingSystem.DEBUG = True
    suite = unittest.TestSuite()
    #suite.addTest(BombardierTest("testBackupUninstall"))
    #suite.addTest(BombardierTest("testAbort"))
    #suite.addTest(BombardierTest("testRemoveVirtualPackage"))
    #suite.addTest(BombardierTest("testAddToDependencyErrors"))
    #suite.addTest(BombardierTest("testVirtualPackages"))
    #suite.addTest(BombardierTest("testReconcileSystem"))
    #suite.addTest(BombardierTest("testZProcessPackages"))
    #suite.addTest(BombardierTest("testInstallList"))
    #suite.addTest(BombardierTest("testGetPackagesToRemove"))
    #suite.addTest(BombardierTest("testPackageIntegration"))
    #suite.addTest(BombardierTest("testBackupUninstall"))
    #suite.addTest(BombardierTest("testVerifySystem1"))
    #suite.addTest(BombardierTest("testCheckBom"))
    #suite.addTest(BombardierTest("testGetPackageGroups"))
    #suite.addTest(BombardierTest("testDownloadBom"))
    #suite.addTest(BombardierTest("testGetDetailedTodolist"))
    suite.addTest(unittest.makeSuite(BombardierTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
    tcommon.unsetForTest()
