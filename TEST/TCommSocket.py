#!/cygdrive/c/Python24/python.exe

import os, sys, unittest, threading, time
import Tcommon
sys.path = ["../client"] + sys.path

import bombardier.CommSocket as CommSocket
from bombardier.staticData import *

class testThread(threading.Thread):
    def __init__(self, s1, s2):
        threading.Thread.__init__(self)
        self.s1 = s1
        self.s2 = s2

    def run(self):
        message = self.s1.waitForMessage()
        self.s2.sendMessage("OK")

class CommSocketTest(unittest.TestCase):

    def testSendMessage(self):
        s1 = CommSocket.CommSocket()
        s1.sendMessage("hello")
        assert s1.getMessage() == "hello"
        s2 = CommSocket.CommSocket()
        s3 = CommSocket.CommSocket()
        sockets = [s1, s2, s3]
        i = 0
        for s in sockets:
            i += 1
            assert s.testStop() == False
        s1.sendStop()
        assert s2.testStop() == False
        assert s3.testStop() == False
        assert s1.testStop() == True

    def testPause(self):
        s1 = CommSocket.CommSocket()
        s2 = CommSocket.CommSocket()
        tt = testThread(s1, s2)
        tt.start()
        time.sleep(1)
        s1.sendMessage("HELLO")
        time.sleep(0.2)
        output = s2.getMessage()
        assert output == "OK", output
        tt.join()

if __name__ == "__main__":
    tcommon = Tcommon.Tcommon()
    tcommon.setForTest()
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(CommSocketTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
    tcommon.unsetForTest()
