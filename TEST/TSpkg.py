#!/cygdrive/c/Python25/python.exe

import os, sys, unittest, shutil, Tcommon
sys.path = ["../client"] + sys.path

from bombardier.staticData import *
from bombardier.Spkg import Spkg
import MockObjects

template = """
<?xml version="1.0" encoding="utf-8"?>
<configuration>
    <appSettings>
      <add key="WebServiceURL" value="%(webServiceUrl)s"/>
      <add key="StringResourceFile" value="%(homeDirectory)s\strings.resources"/>
      <add key="ReturnEmailAddress" value="%(emailSender)s"/>
      <add key="contract_DocumentRoot" value="%(contractDocumentRoot)s" />
      <add key="DbConnect" value="Data Source=%(connectionString)s;UID=%(username)s;PWD=%(password)s;Initial Catalog=%(database)s"/>
</configuration>
"""

class TestPackage(Spkg):
    
    def __init__(self, config, logger, filesystem):
        self.database         = config.get("database", "database")
        self.server           = config.get("database", "server")
        self.username         = config.get("database", "username")
        self.password         = config.get("database", "password")
        self.webServiceUrl    = config.get("testpackage", "webServiceUrl")
        self.connectionString = self.makeConnectionString()
        self.homeDirectory    = config.get("testpackage", "homeDirectory")
        self.emailSender      = config.get("testpackage", "emailSender")
        self.contractDocumentRoot = config.get("testpackage", "contractDocumentRoot")
        Spkg.__init__(self, config, logger, filesystem)

    def installer(self):
        self.filesystem.readFiles[0].write(template)
        self.filesystem.readFiles[0].seek(0)
        #print self.filesystem.readFiles[0].read()
        status = self.modifyTemplate("inputfile", "outputfile")
        output = self.filesystem.writeFiles[0]
        output.seek(0)
        print output.read()
        return status
        

class SpkgTest(unittest.TestCase):

    def setUp(self):
        pass
    def tearDown(self):
        pass
    def testInstaller(self):
        mockConfig  = MockObjects.MockConfig()
        mockConfig.data  = {"database": {"database": "adventureWorks",
                                         "server": "bigdb01",
                                         "username": "advadmin",
                                         "password": "password"},
                            "testpackage": {"webServiceUrl": "http://www.google.com/service/",
                                            "homeDirectory": "c:\\application",
                                            "emailSender": "root",
                                            "contractDocumentRoot": "c:\\contracts"}}
        mockLogger = MockObjects.MockLogger() 
        mockFilesystem = MockObjects.MockFilesystem()
        testPackage = TestPackage(mockConfig, mockLogger, mockFilesystem)
        status = testPackage.installer()
        assert status == OK
        mockLogger.dump()
        

if __name__ == "__main__":
    tcommon = Tcommon.Tcommon()
    tcommon.setForTest()
    suite = unittest.TestSuite()
    #suite.addTest(FilesystemTest("testupdateCurrentStatus"))
    suite.addTest(unittest.makeSuite(SpkgTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
    tcommon.unsetForTest()

