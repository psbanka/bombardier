#!/cygdrive/c/Python25/python.exe

import unittest, sys, os

sys.path = [os.path.join("..", "client"), os.path.join('..', 'client', 'spkgDir')] + sys.path

from bombardier.staticData import *
import bombardier.miniUtility as miniUtility
from testdata import *
import StatusThread
import bombardier.CommSocket as CommSocket
import bombardier.Filesystem as Filesystem
import mock
import MockObjects

class TestStatusThread(unittest.TestCase):
    def setUp(self):
        self.overall = mock.Mock()
        self.application = mock.Mock()
        self.main = mock.Mock()
        self.sub = mock.Mock()
        self.currentAction = mock.Mock()
        self.setLightColor = mock.Mock()
        self.iconControl = mock.Mock()
        self.todos = mock.Mock()
        self.commSocket = CommSocket.CommSocket()
        self.messageThread = None
        self.filesystem = Filesystem.Filesystem()

    def tearDown(self):
        self.commSocket.sendStop()
        self.statusThread.join()

    def testScenario1(self):
        filesystem = MockObjects.MockFilesystem()
        sPath = os.path.join(miniUtility.getSpkgPath(), STATUS_FILE)
        filesystem.yamlData = {sPath: statusFile1}
        self.statusThread = StatusThread.StatusThread(self.overall,
                                                      self.application,
                                                      self.main,
                                                      self.sub,
                                                      self.currentAction,
                                                      self.setLightColor,
                                                      self.iconControl,
                                                      self.todos,
                                                      self.commSocket,
                                                      filesystem)
        self.statusThread.start()
        call = `self.overall.getNamedCalls("__call__")[0]`
        assert call.startswith("__call__(50.0"), call
        call = `self.application.getNamedCalls("__call__")[0]`
        assert call.startswith("__call__(10"), call
        call = `self.main.getNamedCalls("__call__")[0]`
        assert call.startswith("__call__('Installing Packages'"), call
        call = `self.currentAction.getNamedCalls("__call__")[0]`
        assert call.startswith("__call__('downloading'"), call
        call = `self.sub.getAllCalls()[0]`
        assert call.startswith("__call__('testokpackage1')"), call
        call = `self.setLightColor.getNamedCalls("__call__")[0]`
        assert call.startswith("__call__('greenbar.png')"), call
        call = `self.iconControl.getAllCalls()[0]`
        assert call.startswith("__call__(0)")
        calls = self.todos.getAllCalls()
        assert len(calls) == 9, calls
        
    def testScenario2(self):
        filesystem = MockObjects.MockFilesystem()
        sPath = os.path.join(miniUtility.getSpkgPath(), STATUS_FILE)
        filesystem.yamlData = {sPath: statusFile2}
        self.statusThread = StatusThread.StatusThread(self.overall,
                                                      self.application,
                                                      self.main,
                                                      self.sub,
                                                      self.currentAction,
                                                      self.setLightColor,
                                                      self.iconControl,
                                                      self.todos,
                                                      self.commSocket,
                                                      filesystem)
        self.statusThread.start()
        call = `self.overall.getNamedCalls("__call__")[0]`
        assert call.startswith("__call__(66.666666666666657"), call
        call = `self.application.getNamedCalls("__call__")[0]`
        assert call.startswith("__call__(20"), call
        #call = `self.main.getNamedCalls("__call__")[0]`
        #assert call.startswith("__call__('Bombardier has stalled.'"), call
        calls = self.currentAction.getAllCalls()
        assert len(calls) != 0 and `calls[0]` == "__call__('verifying')", `calls`
        calls = self.sub.getAllCalls()
        assert len(calls) == 1, `calls`
        call = `self.setLightColor.getNamedCalls("__call__")[0]`
        assert call.startswith("__call__('redbar.png')"), call
        call = `self.iconControl.getAllCalls()[0]`
        assert call.startswith("__call__(1)")
        calls = self.todos.getAllCalls()

        assert len(calls) == 5, calls
        assert `calls[0]` == "DeleteAllItems()"
        assert `calls[1]` == "InsertStringItem(0, 'testconsolepackage')"
        assert `calls[2]` == "SetStringItem(0, 1, ' test')"
        assert `calls[3]` == "InsertStringItem(1, 'testrebootpackage')"
        assert `calls[4]` == "SetStringItem(1, 1, ' test')"

if __name__ == "__main__":
    suite = unittest.TestSuite()
    #suite.addTest(TestStatusThread("testScenario2"))
    suite.addTest(unittest.makeSuite(TestStatusThread))
    unittest.TextTestRunner(verbosity=2).run(suite)



