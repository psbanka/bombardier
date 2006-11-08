#!c:\Python24\python.exe

import sys, os
sys.path = [os.path.join("..", "client"), os.path.join('..', 'client', 'spkgDir')] + sys.path

import os, unittest, yaml, re
import bombardier
import bombardier.Package as Package
import bombardier.Exceptions as Exceptions
import bombardier.miniUtility as miniUtility
from bombardier.staticData import *
import mock
import MockObjects
import StringIO
import Tcommon
import testdata

class PackageTest(unittest.TestCase):

    def setUp(self):
        data = {"testokpackage1":{"install": {"fullName": None}}}
        self.commSocket = MockObjects.MockCommSocket()
        self.repository = MockObjects.MockRepository(data)
        self.filesystem = MockObjects.MockFilesystem()
        self.windows    = MockObjects.MockWindows()
        self.filesystem.directories = []
        self.filesystem.files = []
        self.server = MockObjects.MockServer()
        self.config = MockObjects.MockConfig()
    
    def tearDown(self):
        pass

    def testInjectorBadPackage(self):
        self.repository.packages = {"foo": {"install": {"fullName": "foo-1"}}}
        package = Package.Package("foo", self.repository, self.config,
                                  self.filesystem, self.server, self.windows)
        package.initialize()
        calls = self.repository.getAllCalls()
        assert len(calls) == 1, calls
        assert `calls[0]` == "getMetaData('foo')"
        status = package.injector()
        assert status == FAIL, 'injector should not have succeeded:'+`status`
        calls = self.filesystem.getAllCalls()
        assert len(calls) == 2, calls
        assert `calls[0]`.startswith("isdir")
        assert `calls[1]` == "updateCurrentAction('Package is corrupt or missing.', 0, <UNPRINTABLE>)"

    def testInjectorOkPackage(self):
        self.repository.packages = {"foo": {"install": {"fullName": "foo-1"}}}
        base = os.path.join(os.getcwd(), 'packages', 'foo-1')
        self.filesystem.directories = [os.path.join(base, 'scripts'),
                                   os.path.join(base, 'injector')]
        package = Package.Package("foo", self.repository, self.config, 
                                  self.filesystem, self.server, self.windows)
        package.initialize()
        status = package.injector()
        assert status == OK, 'injector failed: (%s) when we expected it to work' % status
        pkgDir = os.path.join(miniUtility.getSpkgPath(), "packages", "foo-1", "injector")
        assert package.workingDir == pkgDir, "Injector failed (%s) != (%s)" % (package.workingDir, pkgDir)
        calls = self.filesystem.getAllCalls()
        assert len(calls) == 2, calls
        assert `calls[0]`.split(os.path.sep)[-1] == "scripts')", `calls[0]`
        assert `calls[1]`.split(os.path.sep)[-1] == "injector')", `calls[1]`

    def testGetDateString(self):
        self.repository.packages = {"pkg1":{"install": {"fullName": "testdb-data-initial"}}}
        package = Package.Package("pkg1", self.repository, self.config,
                                  self.filesystem, self.server, self.windows)
        package.initialize()
        dateString = package.getDateString()
        c1 = re.compile("(\d{4})\-(\d+)\-(\d+)\-(\d+)\-(\d+)\-(\d+)")
        m = c1.findall(dateString)
        assert len(m[0]) == 6, "Invalid datestring generated: %s" % dateString

    #^ TEST: Upload malformed tarball to the web service and see what happens

    def testInstall(self):
        self.repository.packages = {"pkg1":{"install": {"fullName": "testdb-data-initial"}}}
        base = os.path.join(os.getcwd(), "packages", "testdb-data-initial")
        self.filesystem.directories = [os.path.join(base, "scripts"),
                                   os.path.join(base, "injector")]
        package = Package.Package("pkg1", self.repository, self.config,
                                  self.filesystem, self.server, self.windows)
        self.filesystem.files = [os.path.join(base, "scripts", "installer.py")]
        self.windows.installablePackages = ["testdb-data-initial"]
        package.initialize()
        package.install(["pkg1"], self.commSocket.testStop)
        assert package.status == OK
        calls = self.filesystem.getAllCalls()
        assert len(calls) == 5, len(calls)
        assert `calls[2]`.startswith("isdir")
        assert `calls[3]` == "updateCurrentAction('Installing...', 50, <UNPRINTABLE>)"
        assert `calls[4]`.startswith("isfile"), `calls[5]`
        calls = self.windows.getAllCalls()
        assert `calls[0]`.startswith("run")

    def testUninstallMalformedPackage(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "testbaduninstallpackage1-1"}}}
        package = Package.Package("pkg1", self.repository, self.config,
                                  self.filesystem, self.server, self.windows)
        package.initialize()
        package.uninstall(self.commSocket.testStop)
        assert package.status == FAIL, "uninstallation of a bogus package succeeded"
        calls = self.filesystem.getAllCalls()
        assert len(calls) == 3, len(calls)

    def testUninstallNoScript(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "testbaduninstallpackage1-1"}}}
        base = os.path.join(os.getcwd(), "packages", "testbaduninstallpackage1-1")
        self.filesystem.directories = [os.path.join(base, "scripts"),
                                       os.path.join(base, "injector")]
        self.filesystem.files = [os.path.join(base, "scripts", "installer.py")]
        package = Package.Package("pkg1", self.repository, self.config,
                                  self.filesystem, self.server, self.windows)
        package.initialize()
        package.uninstall(self.commSocket.testStop)
        assert package.status == FAIL, "uninstallation of a bogus package succeeded"
        calls = self.filesystem.getAllCalls()
        assert len(calls) == 12, len(calls)
        assert `calls[3]` == "updateCurrentAction('Uninstalling...', 70, <UNPRINTABLE>)"
        assert `calls[4]`.startswith("isfile"), `calls[6]`
        assert `calls[5]`.startswith("isfile"), `calls[7]`
        assert `calls[6]`.startswith("isfile"), `calls[8]`
        assert `calls[10]`.startswith("updateProgress")

    def testUninstallOkPackage(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "testokpackage1-1"}}}
        base = os.path.join(os.getcwd(), "packages", "testokpackage1-1")
        self.filesystem.directories = [os.path.join(base, "scripts"), os.path.join(base, "injector")]
        self.filesystem.files = [os.path.join(base, "scripts", "uninstaller.py")]
        package = Package.Package("pkg1", self.repository, self.config, 
                                  self.filesystem, self.server, self.windows)
        package.initialize()
        self.windows.uninstallablePackages = ["testokpackage1-1"]
        package.uninstall(self.commSocket.testStop)
        assert package.status == OK, "Legitimate package uninstallation failed"
        calls = self.server.getAllCalls()
        assert len(calls) == 0, calls
        calls = self.filesystem.getAllCalls()
        assert len(calls) == 9, len(calls)
        assert `calls[3]` == "updateCurrentAction('Uninstalling...', 70, <UNPRINTABLE>)"
        assert `calls[6]` == "getProgressData(False)", `calls[9]`
        assert `calls[8]`.startswith("open"), `calls[11]`
        calls = self.windows.getAllCalls()
        assert `calls[0]`.startswith("run"), `calls[0]`

    def testUninstallErrorScript(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "testbaduninstallpackage1-1"}}}
        base = os.path.join(os.getcwd(), "packages", "testbaduninstallpackage1-1")
        self.filesystem.directories = [os.path.join(base, "scripts"), os.path.join(base, "injector")]
        self.filesystem.files = [os.path.join(base, "scripts", "uninstaller.py")]
        self.filesystem.badcmds = [os.path.join(base, "scripts", "uninstaller.py")]
        package = Package.Package("pkg1", self.repository, self.config,
                                  self.filesystem, self.server, self.windows)
        package.initialize()
        package.uninstall(self.commSocket.testStop)
        assert package.status == FAIL, "Uninstallation of a package that returns an error succeeded"
        calls = self.filesystem.getAllCalls()

        assert len(calls) == 9, len(calls)
        assert `calls[7]`.startswith("updateProgress({'install-progress': {'testbaduninstallpackage1-1': {'UNINSTALLED': 'NA', 'VERIFIED': 'NA', 'INSTALLED': 'BROKEN'}}}, <UNPRINTABLE>, True)")
        calls = self.windows.getAllCalls()
        assert `calls[0]`.startswith("run")

    def testVerifyNoScript(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "testnoverifypackage1-1"}}}
        base = os.path.join(os.getcwd(), "packages", "testnoverifypackage1-1")
        self.filesystem.directories = [os.path.join(base, "scripts"), os.path.join(base, "injector")]
        package = Package.Package("pkg1", self.repository, self.config,
                                  self.filesystem, self.server, self.windows)
        package.initialize()
        package.action = VERIFY
        package.verify(self.commSocket.testStop)
        assert package.status == FAIL, "verification of a bogus package succeeded"
        calls = self.filesystem.getAllCalls()
        assert len(calls) == 12, len(calls)
        assert `calls[3]` == "updateCurrentAction('Verifying...', 90, <UNPRINTABLE>)"
        assert `calls[4]`.startswith("isfile"), `calls[5]`
        assert `calls[4]`.endswith("verify.py')"), `calls[5]`
        assert `calls[5]`.startswith("isfile"), `calls[6]`
        assert `calls[5]`.endswith("verify.bat')"), `calls[6]`
        assert `calls[6]`.startswith("isfile"), `calls[7]`
        assert `calls[6]`.endswith("verify.pl')"), `calls[7]`
        assert `calls[7]`.startswith("isfile"), `calls[7]`
        assert `calls[7]`.endswith("verify.sh')"), `calls[7]`
        assert `calls[10]`.startswith("updateProgress"), calls[10]

    def testVerifyOkPackage(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "testokpackage-1"}}}
        base = os.path.join(os.getcwd(), "packages", "testokpackage-1")
        self.filesystem.directories = [os.path.join(base, "scripts"), os.path.join(base, "injector")]
        self.filesystem.files = [os.path.join(base, "scripts", "verify.py")]
        package = Package.Package("pkg1", self.repository, self.config,
                                  self.filesystem, self.server, self.windows)
        package.initialize()
        self.windows.verifiablePackages = ["testokpackage-1"]
        package.verify(self.commSocket.testStop)
        assert package.status == OK, "Legitimate package verification failed"
        calls = self.filesystem.getAllCalls()
        assert len(calls) == 5, len(calls)
        calls = self.windows.getAllCalls()
        assert `calls[0]`.startswith("run")

    def testConsolePackage(self):
        if sys.platform == "linux2":
            return
        self.repository.packages = {"pkg1": {"install": {"fullName": "testconsolepkg-1", "console":"TRUE"}}}
        base = os.path.join(os.getcwd(), "packages", "testconsolepkg-1")
        self.filesystem.directories = [os.path.join(base, "scripts"), os.path.join(base, "injector")]
        self.filesystem.files = [os.path.join(base, "scripts", "verify.py"),
                                 os.path.join(base, "scripts", "installer.py")]
        package = Package.Package("pkg1", self.repository, self.config,
                                  self.filesystem, self.server, self.windows)
        package.initialize()
        self.windows.installablePackages = ["testconsolepkg-1"]
        package.install(["pkg1"], self.commSocket.testStop)
        assert package.status == OK, "Console package verification failed"
        wcalls = self.windows.getAllCalls()
        fcalls = self.filesystem.getAllCalls()
        
        assert len(fcalls) == 5, len(fcalls)

    def testVerifyBadScript(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "testbadverifypackage-1"}}}
        base = os.path.join(os.getcwd(), "packages", "testbadverifypackage-1")
        self.filesystem.directories = [os.path.join(base, "scripts"), os.path.join(base, "injector")]
        self.filesystem.files = [os.path.join(base, "scripts", "verify.py")]
        self.filesystem.badcmds = [os.path.join(base, "scripts", "verify.py")]
        package = Package.Package("pkg1", self.repository, self.config,
                                  self.filesystem, self.server, self.windows)
        package.initialize()
        package.action = VERIFY
        package.verify(self.commSocket.testStop)
        assert package.status == FAIL, "Verification of a package that returns an error succeeded"
        calls = self.filesystem.getAllCalls()
        assert len(calls) == 9, len(calls)
        assert `calls[7]`.startswith("updateProgress")
        calls = self.windows.getAllCalls()
        assert `calls[0]`.startswith("run")

    def testDownload(self):
        assert 1 == 1 # ^^^ FIXME

    def testProcess(self):
        assert 1 == 1 # ^^^ FIXME

    def testDownloadMetaData(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "testokpackage-1",
                                                     "console": "FALSE"}}}
        package = Package.Package("pkg1", self.repository, self.config,
                                  self.filesystem, self.server, self.windows)
        package.initialize()
        assert package.status == OK
        console = package.metaData.get("install", "console")
        assert console == "FALSE", console

    def testWriteProgressBasic(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "testokpackage1-1"}}}
        self.filesystem.status = yaml.load(testdata.basicProgress).next()
        package = Package.Package("pkg1", self.repository, self.config,
                                  self.filesystem, self.server, self.windows)
        package.initialize()
        package.action = INSTALL
        status = package.writeProgress()
        assert status == OK
        statusData = self.filesystem.status
        assert "timestamp" in statusData.keys()
        installProgress = statusData["install-progress"]
        matches = installProgress.get("testokpackage1-1")
        calls = self.server.getAllCalls()
        assert len(calls) == 0, calls
        calls = self.filesystem.getAllCalls()
        assert len(calls) == 4, len(calls)
        assert `calls[1]` == "getProgressData(False)"
        assert `calls[2]`.startswith("updateProgress({'install-progress': {'testokpackage1-1': {'UNINSTALLED': 'NA', 'VERIFIED':"), `calls[2]`
        assert `calls[3]`.startswith("open"), `calls[3]`
        progress = self.filesystem.status['install-progress']['testokpackage1-1']['INSTALLED']
        assert progress != "BROKEN" and progress != 'NA'

        package.status = FAIL
        status = package.writeProgress()
        assert status == OK
        progress = self.filesystem.status['install-progress']['testokpackage1-1']['INSTALLED']
        assert progress == "BROKEN", progress
        

    def testWriteProgressRepeat(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "testokpackage1-1"}}}
        self.filesystem.status = {"install-progress":{"testokpackge1-1": {"INSTALLED":"today"}}}
        package = Package.Package("pkg1", self.repository, self.config,
                                  self.filesystem, self.server, self.windows)
        package.initialize()
        package.action = INSTALL
        status = package.writeProgress()
        assert status == OK
        status = package.writeProgress()
        assert status == OK
        statusData = self.filesystem.status.get("install-progress")
        matches = statusData.get("testokpackage1-1")
        assert matches.get("INSTALLED") != None, "didn't find what we wanted. "\
               "We wanted 'INSTALLED', but we got %s" % (matches)
        calls = self.server.getAllCalls()
        assert len(calls) == 0, calls
        calls = self.filesystem.getAllCalls()
        assert len(calls) == 8, len(calls)
        assert `calls[1]` == "getProgressData(False)"
        assert `calls[2]`.startswith("updateProgress({'install-progress': {'testokpackage1-1': {'UNINSTALLED': 'NA', 'VERIFIED'")
        assert `calls[3]`.startswith("open"), `calls[3]`

    def testWriteProgressEmpty(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "testokpackage1-1"}}}
        self.filesystem.status = {}
        package = Package.Package("pkg1", self.repository, self.config,
                                  self.filesystem, self.server, self.windows)
        package.initialize()
        package.action = INSTALL
        status = package.writeProgress()
        assert status == OK
        statusData = self.filesystem.status["install-progress"]
        matches = statusData.get("testokpackage1-1")
        assert matches["INSTALLED"] != None, matches
 
    def testBadPackageCreation(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": None}}}
        status = FAIL
        try:
            package = Package.Package("pkg1", self.repository, self.config,
                                      self.filesystem, self.server, self.windows)
            package.initialize()
        except Exceptions.BadPackage, e:
            status = OK
            assert e.errmsg == "Package does not exist on the server"
        assert status == OK
        calls = self.filesystem.getAllCalls()
        assert len(calls) == 0, calls
        calls = self.repository.getAllCalls()
        assert `calls[0]` == "getMetaData('pkg1')"

    def testPackageCreation(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "testokpackage1-1"}}}
        package = Package.Package("pkg1", self.repository, self.config,
                                  self.filesystem, self.server, self.windows)
        package.initialize()
        assert package.status == OK

if __name__ == "__main__":
    tcommon = Tcommon.Tcommon()
    tcommon.setForTest()
    suite = unittest.TestSuite()
    #suite.addTest(PackageTest("testBadPackageCreation"))
    #suite.addTest(PackageTest("testWriteProgressRepeat"))
    #suite.addTest(PackageTest("testWriteProgressBasic"))
    #suite.addTest(PackageTest("testPackageCreation"))
    #suite.addTest(PackageTest("testInstall"))
    #suite.addTest(PackageTest("testInjectorBadPackage"))
    #suite.addTest(PackageTest("testVerifyBadScript"))
    #suite.addTest(PackageTest("testUninstallErrorScript"))
    suite.addTest(unittest.makeSuite(PackageTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
    tcommon.unsetForTest()
