#!/cygdrive/c/Python25/python.exe

import unittest, sys, os, yaml, sets

sys.path = ["..\\server"] + sys.path
from website.service import pkggroups
from Twebcommon import *
import webUtil

errorLog  = Logger()
accessLog = Logger()


class WebPackageGroupsTest(unittest.TestCase):

    def setUp(self):
        pass
            
    def tearDown(self):
        pass

    def testGetBomFiles(self):
        bomfiles = pkggroups.getBomFiles(None)
        assert len(bomfiles) > 5, "Failed to retrieve package groups %s" % bomfiles
        bomfiles = pkggroups.getBomFiles(["testdb"])
        assert len(bomfiles) == 2, "Improper data from bomfiles: %s" % bomfiles
        bomfiles = pkggroups.getBomFiles(["squeezie"])
        assert len(bomfiles) == 0, "Improper data from bomfiles: %s" % bomfiles
        
    def testGet(self):
        args = {"group":["testdb"]}
        request   = Request(args)
        output    = pkggroups.get(request, accessLog, errorLog)
        assert len(output.split()) == 2, "Invalid data returned from server: %s" % output

    def testPut(self):
        args = {"group":["testgroup"]}
        content = "testpackage1\ntestpackage2\n"
        request   = Request(args=args, content=content)
        output    = pkggroups.put(request, accessLog, errorLog)
        data      = open("../server/deploy/config/testgroup.BOM").read()
        assert len(data.strip().split()) == 2, "bad output data: %s" % data
        
if __name__ == "__main__":
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(WebPackageGroupsTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
