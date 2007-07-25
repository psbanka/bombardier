#!/cygdrive/c/Python25/python.exe

import unittest, sys, os, yaml

sys.path = ["..\\server"] + sys.path
from website.service import clientlog
from Twebcommon import *
import webUtil

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

class WebClientLogTest(unittest.TestCase):

    def setUp(self):
        pass
            
    def tearDown(self):
        pass

    def testGet(self):
        open("../server/client/SLECJSXH31/status.yml", 'w').write(testfile)
        accessLog = Logger()
        errorLog  = Logger()

        args = {"client":["SLECJSXH31"]}
        request   = Request(args)
        output    = clientlog.get(request, accessLog, errorLog)
        records = getRecords(output)
        assert len(records) == 3, records

        args = {"client":["SLECJSXH31"], "section":["foo"]}
        request   = Request(args)
        output    = clientlog.get(request, accessLog, errorLog)
        records = getRecords(output)
        assert len(records) == 1, records

        args = {"client":["SLECJSXH31"], "viewed":["False"]}
        request   = Request(args)
        output    = clientlog.get(request, accessLog, errorLog)
        records = getRecords(output)
        assert len(records) == 2, records

        args = {"client":["SLECJSXH31"], "viewed":["False"], "acknowledged":["False"]}
        request   = Request(args)
        output    = clientlog.get(request, accessLog, errorLog)
        records = getRecords(output)
        assert len(records) == 1, records

    def testPost(self):
        open("../server/client/SLECJSXH31/status.yml", 'w').write(testfile)
        accessLog = Logger()
        errorLog  = Logger()
        args = {"client":["SLECJSXH31"], "severity":["WARNING"], "message":["Testing"]}
        request   = Request(args)
        output    = clientlog.post(request, accessLog, errorLog)
        assert output.strip() == "OK", "Bad value for output: %s" % output
        data = open("../server/client/SLECJSXH31/status.yml" , 'r').read()
        records = getRecords(data)
        assert len(records) == 4, "Bad records %s" % records
        assert records[-1]["message"] == "Testing", "Bad data from server: %s" % records[-1]
        
def getRecords(output):
    data      = yaml.load(output)
    records   = []
    newRecords = True
    while newRecords:
        try:
            records.append(data.next())
        except:
            newRecords = False
    return records

if __name__ == "__main__":
    suite = unittest.TestSuite()
    #suite.addTest(WebClientLogTest("testPost"))
    suite.addTest(unittest.makeSuite(WebClientLogTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
