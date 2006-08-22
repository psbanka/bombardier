#!/cygdrive/c/Python23/python.exe

import unittest, sys, os, yaml

sys.path = ["..\\server"] + sys.path
from website.service import package
from Twebcommon import *
import webUtil

class WebPackageTest(unittest.TestCase):

    def setUp(self):
        pass
            
    def tearDown(self):
        pass

    def testFindVersion(self):
        version = package.findVersion("testokpackage1")
        assert version == '2', "bad version: %s" % version
        version = package.findVersion("cheezy")
        assert version == '2', "bad version: %s" % version

    def testGet(self):
        accessLog = Logger()
        errorLog  = Logger()
        request   = Request()
        output    = package.get(request, accessLog, errorLog)
        outputlines = output.split('\n')
        assert "testokpackage1" in outputlines, "Bad output from testGet: %s" % output
        assert request.responseCode == 0, "Bad Response code from testGet: %s" % request.responseCode
        request   = Request(args={"type": ["yaml"]})
        output    = package.get(request, accessLog, errorLog)
        packages  = yaml.load(output).next()
        assert packages["testbadpackage1"]["install"]["console"] == "FALSE", "Bad package data: %s" % output

    def testPutGeneric(self):
        accessLog = Logger()
        errorLog  = Logger()
        content   = open("testTar.tar.gz", 'rb').read()

        request   = Request(content=content)
        output    = package.put(request, accessLog, errorLog)
        assert request.responseCode == 400, "Bad response code. "\
               "Expected 400, but got %s" % request.responseCode
        assert output.strip() == "Must include at least the packagename", "Bad output: %s" % output

        request   = Request(args={"packagename":["testokpackage1"]}, content=content)
        output    = package.put(request, accessLog, errorLog)
        assert request.responseCode == 0, "Bad response code %s" % request.responseCode
        assert output == NOT_FINISHED, "Bad output value: %s" % output
        request.waitToFinish()
        assert request.finished == True, "Not finished yet"
        assert request.result["status"] == FAIL, "Bad output status: %s" % request.data

        packagePath = os.path.join(webUtil.getDeployPath(), "testputgenericpackage-2.spkg")
        if os.path.isfile(packagePath):
            os.unlink(packagePath)
        request   = Request(args={"packagename":["testputgenericpackage"]}, content=content)
        output    = package.put(request, accessLog, errorLog)
        assert request.responseCode == 0, "Bad response code %s" % request.responseCode
        assert output == NOT_FINISHED, "Bad output value: %s" % output
        request.waitToFinish(7)
        assert request.finished == True, "Not finished yet"
        assert request.result["fullname"] == "testputgenericpackage-2", \
               "bad package name: %s" % request.result["fullname"]
        assert request.result["status"] == OK, "Bad output status: %s" % request.data
        assert os.path.isfile(packagePath), "no file created"
        os.unlink(packagePath)

    def testPutDbpatch(self):
        accessLog = Logger()
        errorLog  = Logger()
        content   = open("testTar.tar.gz", 'rb').read()
        request   = Request(args={"type":["dbpatch"], "packagename":["foomanchoo"]},content=content)
        output    = package.put(request, accessLog, errorLog)
        assert request.responseCode == 400, "Bad response code. %s" % request
        assert output.strip().startswith("Did not include"), "Unexpected output %s " % output

        packagePath = os.path.join(webUtil.getDeployPath(), "foomanchoo-patch-100-2.spkg")
        if os.path.isfile(packagePath):
            os.unlink(packagePath)
        request   = Request({"type":["dbpatch"], "packagename":["foomanchoo"], "version":["100"],
                             "database":["testdb"]},content=content)
        output    = package.put(request, accessLog, errorLog)
        assert output == NOT_FINISHED, "Unexpected output value: %s" % output
        request.waitToFinish()
        assert request.result["status"] == OK, "invalid status" % request.result["status"]
        assert request.result["fullname"] == "foomanchoo-patch-100-2", "bad full name: %s" % request.data
        assert os.path.isfile(packagePath), "no file created"
        os.unlink(packagePath)

    def testHotfix(self):
        url       = "http://download.microsoft.com/download/3/4/c/"\
                    "34ca2e4e-da75-4844-aa28-179ae00b1ac0/Q811114_W2K_SP4_X86_EN.exe"
        url       = "http://localhost/deploy/testbadpackage1-1.spkg"
        accessLog = Logger()
        errorLog  = Logger()

        request   = Request(args={"type":["hotfix"], "packagename":["yahoo"]},
                            content="http://www.yahoo.com")
        output    = package.put(request, accessLog, errorLog)
        assert output == NOT_FINISHED, "Bad result code %s" % output
        request.waitToFinish()
        assert request.result["status"] == FAIL, "Bad status %s" % request.data
        
        request   = Request(args={"type":["hotfix"], "packagename":["MS04-035"]}, content=url)
        packagePath = os.path.join(webUtil.getDeployPath(), "Hotfix-MS04-035-2.spkg")
        if os.path.isfile(packagePath):
            os.unlink(packagePath)
        output    = package.put(request, accessLog, errorLog)
        assert request.responseCode == 0, "Bad response code. %s/%s" % (request.responseCode, output)
        assert output == NOT_FINISHED, "Bad output result %s" % output
        request.waitToFinish(10)
        assert request.result["status"] == OK, "Bad status: %s" % request.data
        assert request.result["fullname"] == "Hotfix-MS04-035-2", "Bad filename %s" % request.data
        assert os.path.isfile(packagePath), "no file created"
        os.unlink(packagePath)

if __name__ == "__main__":
    suite = unittest.TestSuite()
    #suite.addTest(WebPackageTest("testHotfix"))
    suite.addTest(unittest.makeSuite(WebPackageTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
