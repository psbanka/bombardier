#!/cygdrive/c/Python25/python.exe

import unittest, StringIO, sets, yaml, sys
import Tcommon

sys.path = ["..\\client"] + sys.path
import bombardier.Server

from bombardier.staticData import *
from testdata import *
import MockObjects

class ServerTest(unittest.TestCase):

    def setUp(self):
        data = {"testokpackage1":{"install": {"fullName": None}}}
        self.filesystem = MockObjects.MockFilesystem()
        self.logger     = MockObjects.MockLogger()
        serverData      = {"address": "127.0.0.1"}
        self.server = bombardier.Server.Server(self.filesystem, serverData)

    def tearDown(self):
        pass

    def testServiceRequest(self):
        putData = {"thing": ["now", "is", "the", "time"]}
        output = self.server.serviceYamlRequest("website/service/putfile/client/test.yml",
                                                putData=putData)
        assert output == "OK", output
        output2 = self.server.serviceYamlRequest("deploy/client/test.yml")
        assert output2.has_key("thing")
        assert len(output2["thing"]) == 4
        output3 = self.server.serviceYamlRequest("deploy/client/test.yml")
        assert output3 == output2
        

if __name__ == "__main__":
    tcommon = Tcommon.Tcommon()
    tcommon.setForTest()
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(ServerTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
    tcommon.unsetForTest()
