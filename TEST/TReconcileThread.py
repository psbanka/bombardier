#!/cygdrive/c/Python24/python.exe
import os, unittest, StringIO, sys
import Tcommon
sys.path = ["..\\client"] + sys.path
import bombardier.ReconcileThread
import bombardier.CommSocket

import MockObjects
from bombardier.staticData import *

class ReconcileThreadTest(unittest.TestCase):

    def setUp(self):
        self.server     = MockObjects.MockServer()
        self.config     = MockObjects.MockConfig()
        self.windows    = MockObjects.MockWindows()
        self.filesystem = MockObjects.MockFilesystem()
        self.commSocketToThread = bombardier.CommSocket.CommSocket()
        self.commSocketFromThread  = bombardier.CommSocket.CommSocket()
        self.bombardier = MockObjects.MockBombardier()
        self.rt = bombardier.ReconcileThread.ReconcileThread(CHECK, self.commSocketToThread,
                                                             self.commSocketFromThread,
                                                             self.config, self.server,
                                                             self.windows, self.filesystem,
                                                             self.bombardier)
        
    def tearDown(self):
        pass

    def testReconcileThread(self):
        assert self.rt.id >= 0
        assert self.rt.id <= 10000

    def testCheck(self):
        self.rt.run()
        assert self.commSocketFromThread.testStop() == True
        bcalls = self.bombardier.getAllCalls()
        scalls = self.server.getAllCalls()
        assert len(bcalls) == 1, bcalls
        assert `bcalls[0]`.startswith("reconcileSystem(<bound method CommSocket.testStop"), `bcalls[0]`
        assert len(scalls) == 0
        #assert `scalls[0]` == "serverLog('INFO', 'Finished installing', 'GENERAL')",`scalls[0]`

    def testAutomated(self):
        self.rt.command = AUTOMATED
        self.rt.run()
        assert self.commSocketFromThread.testStop() == True
        bcalls = self.bombardier.getAllCalls()
        scalls = self.server.getAllCalls()
        assert len(bcalls) == 1
        assert `bcalls[0]`.startswith("reconcileSystem(<bound method CommSocket.testStop"), `bcalls[0]`
        for i in range(len(scalls)):
            print "\n%d: %s" % (i, `scalls[i]`)
        #assert len(scalls) == 0, `scalls`
        #assert `scalls[0]` == "serverLog('INFO', 'Finished installing', 'GENERAL')", `scalls[0]`

    def testVerify1(self):
        self.rt.command = VERIFY
        self.bombardier.verifyStatus = None
        self.rt.run()
        assert self.commSocketFromThread.testStop() == True
        bcalls = self.bombardier.getAllCalls()
        scalls = self.server.getAllCalls()
        assert len(bcalls) == 1
        assert `bcalls[0]`.startswith("verifySystem(<bound method CommSocket"), `bcalls[0]`
        assert `scalls[0]` == "nagiosLog(1, {'OVERALL': 'Error in package system'})"
        
    def testVerify2(self):
        self.rt.command = VERIFY
        self.rt.run()
        assert self.commSocketFromThread.testStop() == True
        bcalls = self.bombardier.getAllCalls()
        scalls = self.server.getAllCalls()
        assert len(bcalls) == 1
        assert `bcalls[0]`.startswith("verifySystem(<bound method CommSocket"), `bcalls[0]`
        assert `scalls[0]` == "nagiosLog(0, {'pkg1': 0})", `scalls[0]`


if __name__ == "__main__":
    tcommon = Tcommon.Tcommon()
    tcommon.setForTest()
    suite = unittest.TestSuite()
    #suite.addTest(RepositoryTest("testGetMetaData"))
    #suite.addTest(ReconcileThreadTest("testGetAndUnpack"))
    #suite.addTest(ReconcileThreadTest("testAutomated"))
    suite.addTest(unittest.makeSuite(ReconcileThreadTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
    tcommon.unsetForTest()
