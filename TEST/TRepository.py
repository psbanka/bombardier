#!/cygdrive/c/Python24/python.exe
import os, unittest, StringIO, sys
import Tcommon

sys.path = ["..\\client"] + sys.path
import bombardier.Repository as Repository
import MockObjects
from bombardier.staticData import *
from testdata import *

class RepositoryTest(unittest.TestCase):

    def setUp(self):
        self.server     = MockObjects.MockServer()
        self.filesystem = MockObjects.MockFilesystem()
        self.server.yamlRequests = [packageData]
        self.config     = MockObjects.MockConfig()
        self.commSocket = MockObjects.MockCommSocket()
        self.repository = Repository.Repository(self.config, 
                                                self.filesystem, self.server)
        self.repository.getPackageData()
        
    def tearDown(self):
        pass

    def testGetPackageData(self):
        pkgCount = len(self.repository.packages.keys())
        calls = self.server.getAllCalls()
        assert len(calls) == 1, calls
        assert `calls[0]`.startswith("serviceYamlRequest"), calls
        assert pkgCount == 3, self.repository.packages
        packageNames = self.repository.getFullPackageNames()
        assert len(packageNames) == pkgCount
        data = self.repository.getMetaData("testbaduninstallpackage1")
        assert data.get("install", "console") == "FALSE", data

    def testGetAndUnpack(self):
        self.filesystem.gzipfile = StringIO.StringIO("data")
        self.filesystem.tarObject.data = {"item1": "data"}
        self.server.output["/deploy/packages/"] = "data"
        base = os.path.join(os.getcwd(), "packages")
        self.filesystem.files = [os.path.join(base, "testbaduninstallpackage1-1.spkg")]
        self.filesystem.directories = [os.path.join(base, "testbaduninstallpackage1-1")]
        self.server.serverData = {"address": "127.0.0.1:123"}
        status = self.repository.getAndUnpack("testbaduninstallpackage1-1",
                                              checksum="1a8d144af5488d46b051786a64b2f681",
                                              abortIfTold=self.commSocket.testStop)
        scalls = self.server.getAllCalls()
        fcalls = self.filesystem.getAllCalls()
        assert len(fcalls) == 9, len(fcalls)
        assert `fcalls[0]`.startswith('isfile'), `fcalls[0]`
        assert `fcalls[1]`.startswith('gzipOpen'), `fcalls[1]`
        assert `fcalls[2]`.startswith('open'), `fcalls[2]`
        assert `fcalls[3]`.startswith('unlink'), `fcalls[3]`
        assert `fcalls[4]`.startswith('tarOpen'), `fcalls[4]`
        assert `fcalls[5]`.startswith('chdir'), `fcalls[5]`
        assert `fcalls[6]`.startswith('isdir'), `fcalls[6]`
        assert `fcalls[7]`.startswith('chdir'), `fcalls[7]`
        assert `fcalls[8]`.startswith('unlink'), `fcalls[8]`
        assert len(scalls) == 2, len(scalls)
        assert `scalls[0]`.startswith("serviceYamlRequest"), `scalls[0]`
        assert status == OK

    def testGetPackage(self):
        self.filesystem.gzipfile = StringIO.StringIO("data")
        self.filesystem.tarObject.data = {"item1": "data"}
        self.server.output["deploy/packages/"] = "data"
        base = os.path.join(os.getcwd(), "packages")
        self.filesystem.files = [os.path.join(base, "testbaduninstallpackage1-1.spkg")]
        self.filesystem.directories = [os.path.join(base, "testbaduninstallpackage1-1")]
        status = self.repository.getPackage("testbaduninstallpackage1", self.commSocket.testStop)
        scalls = self.server.getAllCalls()
        fcalls = self.filesystem.getAllCalls()
        assert len(scalls) == 1
        assert len(fcalls) == 1
        assert `scalls[0]`.startswith("serviceYamlRequest"), `scalls[0]`
        assert `fcalls[0]`.startswith("isdir"), `fcalls[0]`
        assert status == OK

    def testGetFullPackageNames(self):
        fullPackageNames = self.repository.getFullPackageNames()
        assert len(fullPackageNames) == 3
        assert fullPackageNames[0] == "testbaduninstallpackage1-1", fullPackageNames[0]

    def testGetMetaData(self):
        metaData = self.repository.getMetaData("testbaduninstallpackage1")
        assert metaData["install"]["fullName"] == "testbaduninstallpackage1-1"

    def testUnzip(self):
        self.filesystem.gzipfile = StringIO.StringIO("data")
        status = self.repository.unzip("c:\\spkg\\packages\\testbaduninstallpackage1.spkg",
                                       "testbaduninstallpackage1",
                                       abortIfTold=self.commSocket.testStop)
        scalls = self.server.getAllCalls()
        fcalls = self.filesystem.getAllCalls()
        assert status == OK
        assert len(scalls) == 1
        assert `scalls[0]`.startswith('serviceYamlRequest'), `scalls[0]`
        assert len(fcalls) == 3
        assert `fcalls[0]`.startswith('gzipOpen'), `fcalls[0]`
        assert `fcalls[1]`.startswith('open'), `fcalls[1]`
        assert `fcalls[2]`.startswith('unlink'), `fcalls[2]`

if __name__ == "__main__":
    tcommon = Tcommon.Tcommon()
    tcommon.setForTest()
    suite = unittest.TestSuite()
    #suite.addTest(RepositoryTest("testGetMetaData"))
    #suite.addTest(RepositoryTest("testGetAndUnpack"))
    suite.addTest(unittest.makeSuite(RepositoryTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
    tcommon.unsetForTest()
