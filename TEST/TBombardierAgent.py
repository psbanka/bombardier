#!/cygdrive/c/Python25/python.exe

import sys, Tcommon, os

import os, unittest
sys.path = [os.path.join("..", "client"), os.path.join('..', 'spkgDir')] + sys.path

from bombardier.staticData import *
import bombardier.BombardierAgent
import bombardier.miniUtility
import MockObjects

class BombardierAgentTest(unittest.TestCase):

    def setUp(self):
        self.server     = MockObjects.MockServer()
        self.config     = MockObjects.MockConfig()
        self.filesystem = MockObjects.MockFilesystem()
        self.windows    = MockObjects.MockWindows()
        self.bombardierAgent = bombardier.BombardierAgent.BombardierAgent(['a'], 
                                                                          self.windows,
                                                                          self.filesystem)
        wcalls = self.windows.getAllCalls()
        assert len(wcalls) == 2, `wcalls`
        assert `wcalls[0]`.startswith("ServiceFrameworkInit"), `wcalls[0]`
        assert `wcalls[1]`.startswith("getPipe("), `wcalls[1]`

    def tearDown(self):
        pass
    
    def testSvcDoRun(self):
        bombardier.BombardierAgent.SLEEP_INTERVAL=0
        if sys.platform == "linux2":
            return
        import win32event
        self.windows.multipleObjects = ["a", win32event.WAIT_OBJECT_0]
        self.windows.serviceStatusResults = [FAIL, OK]
        self.filesystem.files = [os.path.join(bombardier.miniUtility.getSpkgPath(), LOG_FILE)]
        self.filesystem.stats = [(33279, 43065671436770020L, 2834126225L, 1,
                                  400, 401, 2305596L, 1114301150, 1114301150,
                                  1108358052)]
        self.bombardierAgent.SvcDoRun()
        wcalls = self.windows.getAllCalls()
        fcalls = self.filesystem.getAllCalls()
        scalls = self.server.getAllCalls()
        assert len(wcalls) == 5, len(wcalls)
        assert `wcalls[0]`.startswith("ServiceFrameworkInit(")
        assert `wcalls[1]` == r"getPipe('\\\\.\\pipe\\BombardierAgent')", `wcalls[1]`
        assert `wcalls[2]`.startswith("LogMsg")
        assert `wcalls[3]`.startswith("ReadPipe(")
        assert `wcalls[4]`.startswith("LogMsg")
        assert len(fcalls) == 0, len(fcalls)
        assert len(scalls) == 0

if __name__ == "__main__":
    tcommon = Tcommon.Tcommon()
    tcommon.setForTest()
    suite = unittest.TestSuite()
    #suite.addTest(BombardierAgentTest("testSvcDoRun"))
    suite.addTest(unittest.makeSuite(BombardierAgentTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
    tcommon.unsetForTest()

