#!/cygdrive/c/Python25/python.exe

import os, sys, unittest, shutil, md5, time
import Tcommon

if os.environ.has_key("PYTHONPATH"):
    pp = os.environ["PYTHONPATH"]
else:
    pp = ""
pp += ";..\\client"
os.environ["PYTHONPATH"] = pp

Tcommon.checkImportPath("commonUtil.py")
from staticData import *
import utility, shutil
import Config, Logger

FAIL = Tcommon.FAIL
OK   = Tcommon.OK

TEST_PATH    = os.getcwd()
NORMAL_PATH  = Config.getSpkgPath()

logger = Logger.Logger()
config = Config.Config(logger)

repo = config.repository["address"]

class UtilityTest(unittest.TestCase):

    def setUp(self):
        pass
    
    def tearDown(self):
        Tcommon.setSpkgPath(NORMAL_PATH)

    def testGetInstallablePackages(self):
        bomPath = Config.getBomPath()
        repo = config.repository()["address"]
        packages = utility.getInstallablePackages(repo, bomPath)
        assert "testnoinstaller1-1" in packages, "`testnoinstaller1-1, not in %s" % packages


    ### FROM BOMBARDIER'S CREDENTIALS CLASS, now part of config

    def testGetRunKey(self):
        logger=Logger.Logger()
        utility.restartOnLogon()
        status = utility.checkRunKey()
        assert status == 1, "Expected to find checkRun = 1, instead == %s" % status
        utility.noRestartOnLogon(logger)
        status = utility.checkRunKey()
        assert status == 0, "Expected to find checkRun = 0, instead = %s" % status

    def testWget(self):
        status = utility.wget("http://localhost:23", "foomanchoo", config, logger)
        assert status == FAIL, "wget from a nonexistent server returned success"
        status = utility.wget(repo+"deploy/", "foomanchoo", config, logger)
        assert status == FAIL, "wget a nonexistant file returned success"
        status = utility.wget(repo+"deploy/", "bomsetup.ini", config, logger,
                              destDir = "kdkdkd")
        assert status == FAIL, "wget to a nonexistent directory returns success"
        curDir = os.getcwd()
        assert os.path.isfile(os.path.join("testScratch", "bomsetup.ini")) == False, "scratch "\
               "directory was not cleaned properly"
        status = utility.wget(repo+"deploy/", "bomsetup.ini", config, logger,
                              destDir = "kdkdkd")
        assert status == FAIL, "wget to a nonexistent directory returns success"
        status = utility.wget(repo+"deploy/", "bomsetup.ini", config, logger,
                              destDir = "")
        assert status == OK, "wget did not report that it was successful downloading."
        assert os.path.isfile("bomsetup.ini"), "wget failed to download the file"
        os.unlink("bomsetup.ini")
        status = utility.wget(repo+"deploy/", "bomsetup.ini", config, logger,
                              destDir = "testScratch")
        assert status == OK, "wget did not report that it was successful downloading."
        assert os.path.isfile(os.path.join("testScratch", "bomsetup.ini")) == True, "file %s was "\
               "not downloaded properly" % "bomsetup.ini"
        assert os.getcwd() == curDir

    def testWgetCaseless(self):
        repo = config.repository()["address"]
        curDir = os.getcwd()
        logger=Logger.Logger()
        config = Config.Config(logger)
        status = utility.wgetCaseless("http://localhost:23", "foomanchoo", config, logger)
        assert status == FAIL, "wgetCaseless from a nonexistent server returned success"
        status = utility.wgetCaseless(repo, "foomanchoo", config, logger)
        assert status == FAIL, "wget a nonexistant file returned success"
        TEST_FILE = "bomsetup.ini"
        status = utility.wgetCaseless(repo+"deploy/", TEST_FILE, config, logger, destDir = "testScratch")
        assert os.path.isfile(os.path.join("testScratch", TEST_FILE)) == True, "file %s was "\
               "not downloaded properly" % TEST_FILE
        assert os.getcwd() == curDir
        os.unlink(os.path.join("testScratch", TEST_FILE))
        status = utility.wgetCaseless(repo+"deploy/", "bOMsetup.Ini", config, logger, destDir = "testScratch")
        assert os.path.isfile(os.path.join("testScratch", TEST_FILE)) == True, "file %s was "\
               "not downloaded properly" % TEST_FILE


    def testGetPackagesFromFile(self):
        okBomFile = "okbom.txt"
        badBomFile = "badbom.txt"
        okBom = """cscript-5.6
cygwin-108922887
support-tools
ie6
Hotfix-manager
Hotfix-824146
Hotfix-828028
Hotfix-828035
Hotfix-828741
Hotfix-835732
Hotfix-837001
ethereal-0.10
bginfo
radmin-2.1
active-ports-1.4
nav-1
acrobat-1
textpad-1
system-setup"""
        badBom = """cscript-5.6
cygwin-108922
support-tools



ie6
Hotfix-manage
Hotfix-824146
Hotfix-828028
Hotfix-828035
Hotfix-828741
***
Hotfix-837001
ethereal-0.10
\x088
@
22
bginfo
radmin-2.1
active-ports-
nav-1
acrobat-1
textpad-1
system-setup"""
        open(badBomFile, 'w').write(badBom)
        open(okBomFile, 'w').write(okBom)
        packages = utility.getPackagesFromFile(okBomFile)
        assert "system-setup" in packages, "expected %s in the list of packages %s "\
               "from ok BOM" % ("system-setup", `packages`)
        assert len(packages) == 19, "expected 19 packages in the ok bom file, "\
               "got %d" % len(packages)
        packages = utility.getPackagesFromFile(badBomFile)
        assert len(packages) == 20, "expected 20 packages in the bad bom file, "\
               "got %d: %s" % (len(packages), `packages`)
        packages = utility.getPackagesFromFile("bddkdkdk")
        assert len(packages) == 0, "got packages from a bad file"
        packages = utility.getPackagesFromFile(okBomFile, stripVersionFromName = 1)
        assert "textpad" in packages
        assert "system-setup" in packages

    def testStripVersion(self):
        output = utility.stripVersion("system-setup-1")
        assert output == "system-setup", output
        output = utility.stripVersion("system-setup-2323")
        assert output == "system-setup", output
        output = utility.stripVersion("system-setup")
        assert output == "system-setup", output
        output = utility.stripVersion("system-setup-21.2")
        assert output == "system-setup-21.2", output
        output = utility.stripVersion("sys")
        assert output == "sys", output
        output = utility.stripVersion("")
        assert output == "", output
        output = utility.stripVersion("----")
        assert output == "----", output
        output = utility.stripVersion("0292")
        assert output == "0292", output
        
    def testGetInstallablePackages(self):
        repo = config.repository()["address"]
        installablePackages = utility.getInstallablePackages(repo, "BOM.txt")
        assert "testnoinstaller1-1" in installablePackages, "Packages installable is wrong"
        installablePackages = utility.getInstallablePackages(repo, "BOdfsfdM.txt")
        assert len(installablePackages) > 1, "Packages from a bogus BOM is wrong: %s" % `installablePackages`
        installablePackages = utility.getInstallablePackages("http://localhost:383", "BOM.txt")
        assert len(installablePackages) == 0, "Packages from a bogus repository is wrong."

##     def testInstallManager(self):
##         logger=Logger.Logger()
##         print "\n\nNote, because windows sucks, this test only works once."
##         status = utility.installManager("d:\\dev\\spkg\\", logger)
##         assert status == OK, "InstallManager was unable to install/start the service"
##         status = utility.managerStatus(logger)
##         assert status == OK
##         status = utility.uninstallManager("d:\\dev\\spkg\\", logger)

##   #############################
##   # CONFIG CLASS
##   #############################
        
    def testAddDictionaries(self):
        a = {1: "foo", 2:"bar"}
        b = {99: 2, 3:[1,2,3], "hello":2}
        c = utility.addDictionaries(a, b)
        keys = c.keys()
        assert 1 in keys
        assert 99 in keys
        assert c[3] == [1,2,3]

    def testEvalBoolean(self):
        assert utility.evalBoolean("TRUE")  == True
        assert utility.evalBoolean("TrUE")  == True
        assert utility.evalBoolean("FALSE") == False
        assert utility.evalBoolean("fALSE") == False
        assert utility.evalBoolean("FOO")   == False
        assert utility.evalBoolean("1")     == True
        assert utility.evalBoolean("Ok")    == True
        assert utility.evalBoolean("yes")   == True
        assert utility.evalBoolean(12)      == False
        assert utility.evalBoolean(1)       == True
        assert utility.evalBoolean([1,2,3]) == False

    def testDownloadConfig(self):
        if os.path.isfile('bill.ini'):
            os.unlink('bill.ini')
        logger=Logger.Logger()
        config = Config.Config(logger)
        config.setupFile = "bill"
        status = config.downloadConfig()
        assert status == OK
        assert os.path.isfile("bill.ini") == True
        config.setupFile = "bill.ini"
        status = config.downloadConfig()
        assert status == FAIL
        os.unlink("bill.ini")
        config.setupFile = "bILL"
        status = config.downloadConfig()
        assert status == OK
        assert os.path.isfile("bill.ini") == True

    def testCheckSetLock(self):
        status = utility.setLock(config, logger)
        assert status == OK, "Lock should have been clear"
        assert config.get("system", "installLock") == "TRUE", "Lock not properly set"
        status = utility.setLock(config, logger)
        assert status == FAIL, "Lock should have been set."
        assert config.get("system", "installLock") == "TRUE", "Lock not properly set"
        utility.clearLock(config, logger)
        assert config.get("system", "installLock") == "FALSE", "Lock not properly set"

    def testPostServerMessage(self):
        import random, urlparse, os
        randint = random.randint(0,100)
        hostname = os.environ["COMPUTERNAME"]
        #testHost = "http://192.168.1.199"
        testHost = "http://localhost"
        url = urlparse.urljoin(testHost, "website/service/clientlog?client=%s" % (hostname))
        badurl1 = urlparse.urljoin(testHost, "weite/service/clientlog?client=%s" % (hostname))
        badurl2 = urlparse.urljoin("http://localhost:8833",
                                   "website/service/clientlog?client=%s" % (hostname))
        desturl = urlparse.urljoin(testHost, "client/%s" % (hostname))
        status = utility.postServerMessage("WARNING","Unable to read localConfig.ini", url, logger)
        status = utility.postServerMessage("ERROR","%s" % randint, url, logger)
        assert status == OK, "Failed to POST"
        status = utility.wget(desturl, "status.txt",
                              config, logger)
        assert status == OK, "Failed to retrieve POST file"
        lastLine = open("status.txt").readlines()[-1]
        assert `randint` in lastLine.split(), "Server did not write the file where expected "\
               "in the right way"
        status = utility.postServerMessage("ERROR","Hi there",badurl1, logger)
        assert status == FAIL
        status = utility.postServerMessage("ERROR","Hi there",badurl2, logger)
        assert status == FAIL

    def testUpdateProgress(self):
        utility.updateProgressFile({"todo": ["hello,goodbye"]})

    def testUpdateCurrentAction(self):
        current = """---
status:
    action: Verified package structure
    percentage: 10
    overall: installing
    package: testokpackage1
timestamp: 1113085453.25
todo:
    - testconsolepackage, test
    - testrebootpackage, test
"""
        open(CURRENT_FILE, 'w').write(current)
        utility.updateCurrentAction("hello")
        data = utility.loadCurrent()
        assert len(data["status"].keys()) == 4, data["status"].keys()

    def testAddDictionaries(self):
        import utility
        thing1 = {"spam":{"eggs": 1}}
        thing2 = {"spam":{"milk": 2, "eggs": 2}, "grits":1}
        combination = utility.addDictionaries(thing1, thing2)
        assert combination["spam"]["eggs"] == 1
        assert combination["spam"]["milk"] == 2
        assert combination["grits"] == 1



if __name__ == "__main__":
    suite = unittest.TestSuite()
    suite.addTest(UtilityTest("testUpdateCurrentAction"))
    #suite.addTest(unittest.makeSuite(UtilityTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
