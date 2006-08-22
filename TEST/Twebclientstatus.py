#!/cygdrive/c/Python23/python.exe

import unittest, sys, os, yaml, sets

sys.path = ["..\\server"] + sys.path
from website.service import clientstatus
from Twebcommon import *
import webUtil

errorLog  = Logger()
accessLog = Logger()

inifile = """[body]
residence=portland

[parents]
parent1 = margaret
parent0 = bill

[packageGroups]
group0=testdb
"""

testfile = """---
acknowledged: False
client: SLECJSXH31
message: Problems downloading package foo
section: foo
severity: ERROR
time: 'Thu Mar 10 17:24:51 2005'
viewed: False

---
acknowledged: False
client: SLECJSXH31
message: 'Unable to find an appropriate package in http://127.0.0.1 for package foo.'
section: bar
severity: WARNING
time: 'Thu Mar 10 17:24:53 2005'
viewed: True

---
acknowledged: True
client: SLECJSXH31
message: 'Unable to find an appropriate package in http://127.0.0.1 for package foo.'
section: fuzz
severity: ERROR
time: 'Thu Mar 10 17:24:53 2005'
viewed: False
"""

lastdata = """
---
acknowledged: True
client: SLECJSXH31
message: 'testing 1,2,3'
section: fuzz
severity: OK
time: 'Thu Mar 10 17:24:53 2005'
viewed: False
"""

installProgress1 = """
---
testokpackage1-1:
    - INSTALLED
    - 'Thursday, Mar 03/10/05 17:25:00'
"""

installProgress2 = """
---
testokpackage1-1:
    - INSTALLED
    - 'Thursday, Mar 03/10/05 17:25:00'
testfruitshow-1: INSTALLED
anotherpackage-3: UNINSTALLED
"""

installProgress3 = """
---
testokpackage1-
"""

installProgress4 = """
---
testdb-structure-1: INSTALLED
testdb-data-1: INSTALLED
"""

class WebClientStatusTest(unittest.TestCase):

    def setUp(self):
        pass
            
    def tearDown(self):
        pass

    def testGetInstalledPackages(self):
        open("../server/client/SLECJSXH31/install-progress.yml", 'w').write(installProgress1)
        packageNames = clientstatus.getInstalledPackages("SLECJSXH31", errorLog)
        assert ["testokpackage1"] == packageNames, "Bad returned packageNames: %s" % packageNames

        open("../server/client/SLECJSXH31/install-progress.yml", 'w').write(installProgress2)
        packageNames = clientstatus.getInstalledPackages("SLECJSXH31", errorLog)
        assert sets.Set(["testokpackage1","testfruitshow"]) == sets.Set(packageNames), \
               "Bad returned packageNames: %s" % packageNames

        open("../server/client/SLECJSXH31/install-progress.yml", 'w').write(installProgress3)
        packageNames = clientstatus.getInstalledPackages("SLECJSXH31", errorLog)
        assert packageNames == [], "Bad returned packageNames: %s" % packageNames

    def testInstallStatus(self):
        open("../server/client/SLECJSXH31/install-progress.yml", 'w').write(installProgress1)
        status = clientstatus.installStatus("SLECJSXH31", errorLog)
        assert status['testdb']['installedStatus'] == 'FAIL', "invalid status %s" % status
        
        open("../server/client/SLECJSXH31/install-progress.yml", 'w').write(installProgress4)
        status = clientstatus.installStatus("SLECJSXH31", errorLog)
        assert status['testdb']['installedStatus'] == 'OK', "invalid status %s" % status

    def testUpdateStatus(self):
        open("../server/client/SLECJSXH31/last.yml", 'w').write(lastdata)
        status = clientstatus.updateStatus("SLECJSXH31")
        assert status['lastStatus'] == 'OK', "Invalid status: %s " % status
        assert status['lastMessage'] == 'testing 1,2,3', "Invalid status: %s " % status

    def testGet(self):
        open("../server/client/SLECJSXH31/status.yml", 'w').write(testfile)
        open("../server/client/SLECJSXH31/last.yml", 'w').write(lastdata)
        open("../server/client/SLECJSXH31/install-progress.yml", 'w').write(installProgress4)
        open("../server/deploy/config/SLECJSXH31.ini", 'w').write(inifile)
        
        args = {"client":["SLECJSXH31"]}
        request   = Request(args)
        output    = clientstatus.get(request, accessLog, errorLog)
        data      = yaml.load(output).next()
        assert len(data.keys()) == 4, "Invalid yaml data returned from server: %s" % data.keys()
        assert data['installation']['testdb']['status'] == 'OK', "bad data: %s" % data

    def testPut(self):
        args = {"client":["SLECJSXH31"], "message":["INSTALL"]}
        request   = Request(args=args, content=installProgress4)
        output    = clientstatus.put(request, accessLog, errorLog)
        data      = yaml.load(output).next()
        assert data.strip() == "OK", "bad output data: %s" % data
        
if __name__ == "__main__":
    suite = unittest.TestSuite()
    suite.addTest(WebClientStatusTest("testGet"))
    #suite.addTest(unittest.makeSuite(WebClientStatusTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
