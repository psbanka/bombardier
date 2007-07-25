#!/cygdrive/c/Python25/python.exe

import os, sys, unittest, shutil, md5
import sys
sys.path = ["..\\client"] + sys.path
from bombardier.staticData import *
import bombardier.Logger as Logger

class LoggerTest(unittest.TestCase):

    def setUp(self):
        pass
            
    def testInit(self):
        logger = Logger.Logger()
        logger.info("testing")
        data = logger.getLastLogLine()
        assert data.rfind("testing") != -1, "logger function is not working."
        assert data.rfind("INFO") != -1, "logger function is not logging at the proper levels."


    ## THESE ARE PROBABLY BROKE AND SHOULD BE FIXED - pbanka
    def testCheckLogSize(self):
        self.filesystem.files = [os.path.join(bombardier.miniUtility.getSpkgPath(), LOG_FILE)]
        self.filesystem.stats = [(33279, 43065671436770020L, 2834126225L, 1,
                                  400, 401, 2291984L, 1114140250, 1114140250, 1108358052)]
        status = self.bombardierClient.checkLogSize()
        assert status == FAIL
        self.filesystem.files = [os.path.join(bombardier.getSpkgPath(), LOG_FILE)]
        self.filesystem.stats = [(33261, 1970324837012162L, 2834126225L, 1, 544, 401,
                                  3879L, 1113971328, 1113778888, 1107913169)]
        status = self.bombardierClient.checkLogSize()
        assert status == OK

    def testCycleLog(self):
        base = os.path.join(bombardier.getSpkgPath(), LOG_FILE)
        for i in range(0,5):
            self.filesystem.files.append("%s.%d" % (base, i))
        status = self.bombardierClient.cycleLog()
        fcalls = self.filesystem.getAllCalls()

        assert `fcalls[0]`.startswith("isfile")
        assert `fcalls[0]`.endswith("log.5')")

        assert `fcalls[1]`.startswith("isfile")
        assert `fcalls[1]`.endswith("log.4')")

        assert `fcalls[2]`.startswith("copyfile")
        assert `fcalls[2]`.rfind("log.4") != -1
        assert `fcalls[2]`.endswith("log.5')")

        assert `fcalls[3]`.startswith("unlink")
        assert `fcalls[3]`.endswith("log.4')")

        assert `fcalls[4]`.startswith("isfile")
        assert `fcalls[4]`.endswith("log.3')")

        assert `fcalls[5]`.startswith("copyfile")
        assert `fcalls[5]`.rfind("log.3") != -1
        assert `fcalls[5]`.endswith("log.4')")

        assert `fcalls[6]`.startswith("unlink")
        assert `fcalls[6]`.endswith("log.3')")

        assert `fcalls[7]`.startswith("isfile")
        assert `fcalls[7]`.endswith("log.2')")

        assert `fcalls[8]`.startswith("copyfile")
        assert `fcalls[8]`.rfind("log.2") != -1
        assert `fcalls[8]`.endswith("log.3')")

        assert `fcalls[9]`.startswith("unlink")
        assert `fcalls[9]`.endswith("log.2')")

        assert `fcalls[10]`.startswith("isfile")
        assert `fcalls[10]`.endswith("log.1')")

        assert `fcalls[11]`.startswith("copyfile")
        assert `fcalls[11]`.rfind("log.1") != -1
        assert `fcalls[11]`.endswith("log.2')")
                         
        assert `fcalls[12]`.startswith("unlink")
        assert `fcalls[12]`.endswith("log.1')")
                         
        assert `fcalls[13]`.startswith("copyfile")
        assert `fcalls[13]`.rfind("log") != -1
        assert `fcalls[13]`.endswith("log.1')")
                         
        assert `fcalls[14]`.startswith("unlink")
        assert `fcalls[14]`.endswith("log')")


        
if __name__ == "__main__":
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(LoggerTest))
    unittest.TextTestRunner(verbosity=2).run(suite)

