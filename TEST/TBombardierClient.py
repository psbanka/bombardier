#!c:\Python24\python.exe

import unittest, win32event
import sys, Tcommon, time

sys.path = ["..\\client"] + sys.path

import MockObjects
from bombardier.staticData import *
import bombardier.BombardierClientService as Bcs
import bombardier.miniUtility
Bcs.DEBUG = 2


class BombardierClientTest(unittest.TestCase):

    def setUp(self):
        self.server     = MockObjects.MockServer()
        self.config     = MockObjects.MockConfig()
        self.filesystem = MockObjects.MockFilesystem()
        self.windows    = MockObjects.MockWindows()
        self.repository = MockObjects.MockRepository({})
        self.bombardierClient = Bcs.BombardierClientService([], self.repository, self.config,
                                                            self.filesystem, self.server, self.windows)
        wcalls = self.windows.getAllCalls()
        assert len(wcalls) == 1, `wcalls`
        assert `wcalls[0]`.startswith("ServiceFrameworkInit"), `wcalls[0]`

    def tearDown(self):
        pass

    def testSvcDoRunCmd(self):
        self.windows.multipleObjects = ["a", win32event.WAIT_OBJECT_0]
        self.windows.readPipe = [CHECK]
        exited = False
        try:
            self.bombardierClient.SvcDoRun()
        except SystemExit, e:
            exited = True
        assert exited == True
        scalls = self.server.getAllCalls()
        fcalls = self.filesystem.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert len(wcalls) == 7, len(wcalls)
        assert `wcalls[1]`.startswith("LogMsg"), `wcalls[1]`
        assert `wcalls[2]`.startswith("ReadPipe("), `wcalls[2]`
        assert `wcalls[3]` == "noAutoLogin()"
        assert `wcalls[4]` == "noRestartOnLogon()", `wcalls[4]`
        assert `wcalls[5]`.startswith("ReadPipe("), `wcalls[5]`
        assert `wcalls[6]`.startswith("LogMsg(4, 1073745924, "\
                                       "('BombardierClient', ''))"), `wcalls[6]`
        assert len(fcalls) == 2, len(fcalls)
        assert `fcalls[0]` == "clearLock()", `fcalls[0]`
        assert `fcalls[1]` == "runReconcileThread('5')", `fcalls[1]`
        assert len(scalls) == 1, scalls
        assert `scalls[0]` == "getServerData()"

    def testSvcDoRunAutocheck(self):
        self.windows.multipleObjects = [None, win32event.WAIT_OBJECT_0]
        self.windows.readPipe = ['']
        Bcs.CMD_SLEEP = 0
        self.bombardierClient.checkTime = time.time() - 10
        exited = False
        try:
            self.bombardierClient.SvcDoRun()
        except SystemExit, e:
            exited = True
        scalls = self.server.getAllCalls()
        fcalls = self.filesystem.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert len(wcalls) == 7, len(wcalls)
        assert len(fcalls) == 2, fcalls
        assert `fcalls[0]` == "clearLock()", `fcalls[0]`
        assert `fcalls[1]` == "runReconcileThread('5')", `fcalls[2]`
    def testSvcDoRunAutoverify(self):
        self.windows.multipleObjects = [None, win32event.WAIT_OBJECT_0]
        self.windows.readPipe = ['']
        self.config.data["system"] = {}
        self.config.data["system"]["runVerify"] = "YES"
        Bcs.CMD_SLEEP = 0
        self.bombardierClient.verifyTime = time.time() - 10
        exited = False
        try:
            self.bombardierClient.SvcDoRun()
        except SystemExit, e:
            exited = True
        scalls = self.server.getAllCalls()
        fcalls = self.filesystem.getAllCalls()
        wcalls = self.windows.getAllCalls()
        assert len(wcalls) == 7, len(wcalls)
        assert len(fcalls) == 2, fcalls
        assert `fcalls[0]` == "clearLock()", `fcalls[1]`
        assert `fcalls[1]` == "runReconcileThread('3')", `fcalls[2]`

if __name__ == "__main__":
    tcommon = Tcommon.Tcommon()
    tcommon.setForTest()
    suite = unittest.TestSuite()
    #suite.addTest(BombardierClientTest("testSvcDoRunCmd"))
    #suite.addTest(BombardierClientTest("testSvcDoRunAutoverify"))
    suite.addTest(unittest.makeSuite(BombardierClientTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
    tcommon.unsetForTest()
