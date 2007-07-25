#!/cygdrive/c/Python25/python.exe

import os, sys, unittest, shutil, md5
sys.path = ["../client"] + sys.path
Tcommon.checkImportPath("dbutils.py")

import bombardier.dbutils as dbutils
import bombardier.Config as Config
import bombardier.Package as Package
import bombardier.Repository as Repository
from bombardier.staticData import *
import bombardier.Logger as Logger

TEST_PATH    = os.getcwd()
NORMAL_PATH  = Config.getSpkgPath()
config  = Config.Config()
repo    = Repository.Repository(config)

class DbUtilsTest(unittest.TestCase):

    def setUp(self):
        self.startPath = os.getcwd()
        Tcommon.setSpkgPath(TEST_PATH)
        Tcommon.cleanup()
            
    def tearDown(self):
        Tcommon.setSpkgPath(NORMAL_PATH)
        os.chdir(self.startPath)

    def testDbInstallStructure(self):
        Logger.info("=======================testDbinstallStructure")
        package = Package.Package("testrealdb-structure-1", repo, config)
        # download the package and get to the injector directory
        startDir = os.getcwd()
        package.download()
        package.injector()
        package.chdir()
        assert os.path.isdir("injector"), "Injector directory did not get created: "\
               "problem with testreadb-structure"
        os.chdir("injector")
        databaseNames, role = dbutils.dbOwnership()
        assert databaseNames == ["ReaderDB", "LoginDB"], "Incorrect database names %s" % databaseNames
        assert role == STRUCTURE, "Incorrect role type %s" % role
        try:
            dbutils.uninstallDbPackage(config)
        except SystemExit, e:
            assert e.code == OK, "Uninstallation of database package failed (%s!=%s) %s." % (e.code, OK, e.code==OK)
        try:
            dbutils.installDbPackage(config)
        except SystemExit, e:
            assert e.code == OK, "Installation of database package failed (%s)." % e.code
        os.chdir(startDir)

    def testZDbInstallData(self):
        Logger.info("=======================testDbInstallData")
        package = Package.Package("testrealdb-data-initial", repo, config)
        # download the package and get to the injector directory
        startDir = os.getcwd()
        status = package.download()
        assert status == OK
        status = package.injector()
        assert status == OK
        status = package.chdir()
        assert status == OK
        assert os.path.isdir("injector"), "Injector directory did not get created: "\
               "problem with testreadb-data-initial package"
        os.chdir("injector")
        databaseNames, role = dbutils.dbOwnership()
        assert databaseNames == ["ReaderDB", "LoginDB"], "Incorrect database names %s" % databaseNames
        assert role == DATA, "Incorrect role type (%s)" % `role`
        try:
            dbutils.uninstallDbPackage(config)
        except SystemExit, e:
            assert e.code == OK, "Uninstallation of database package failed (%s!=%s) %s." % (e.code, OK, e.code==OK)
        try:
            dbutils.installDbPackage(config)
        except SystemExit, e:
            assert e.code == OK, "Installation of database package failed (%s)." % e.code
        os.chdir(startDir)


if __name__ == "__main__":
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(DbUtilsTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
