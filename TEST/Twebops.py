#!/cygdrive/c/Python23/python.exe

import Tcommon

Tcommon.checkImportPath("webops.py")
from staticData import *
import webops

class WebopsTest(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass



if __name__ == "__main__":
    suite = unittest.TestSuite()
    #suite.addTest(PackageTest("testDownloadMetaData"))
    suite.addTest(unittest.makeSuite(WebopTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
