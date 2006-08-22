#!/cygdrive/c/Python23/python.exe

import unittest, sys, StringIO, os

sys.path = [os.path.join("..", "client"), os.path.join('..', 'client', 'spkgDir')] + sys.path

import bc
import MockObjects
import mock

class TestBc(unittest.TestCase):
    def setUp(self):
        self.filesystem = MockObjects.MockFilesystem()

    def tearDown(self):
        pass

    def testTail(self):
        self.filesystem.stats = [(33206, 0L, 3, 1, 0, 0, 152L, 1115782570, 1115782570, 1115778967)]
        mFile = MockObjects.MockFile()
        self.filesystem.readFiles = [mFile]
        logger = MockObjects.MockLogger()
        try:
            bc.tail("foobar", self.filesystem, logger.info)
        except "DoneTestingException":
            pass
        fcalls = self.filesystem.getAllCalls()
        assert len(fcalls) == 2
        assert `fcalls[0]` == "open('foobar', 'r')"
        assert `fcalls[1]` == "stat('foobar')"
        mfcalls = mFile.getAllCalls()
        assert len(mfcalls) == 3
        assert `mfcalls[0]` == "seek(152L)"
        assert `mfcalls[1]` == "tell()"
        assert `mfcalls[2]` == "readline()"

if __name__ == "__main__":
    suite = unittest.TestSuite()
    #suite.addTest(TestBc("testTail"))
    suite.addTest(unittest.makeSuite(TestBc))
    unittest.TextTestRunner(verbosity=2).run(suite)
