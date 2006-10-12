#!/c/Python24/python.exe

import Tcommon, os, sys, unittest

sys.path = [os.path.join("..", "client")] + sys.path

import bombardier.miniUtility as miniUtility
from bombardier.staticData import *

class miniUtilityTest(unittest.TestCase):

    def testCompareLists(self):
        super = [1,2,3]
        sub = [1,2,3]
        assert miniUtility.compareLists(sub, super) == True

    def testCompareDicts(self):
        super = {"a": 1, "b": 2}
        sub   = {"a": 1}
        assert miniUtility.compareDicts(sub, super) == True
        assert miniUtility.compareDicts(super, sub) == False

        assert miniUtility.compareDicts(sub, {}) == False
        assert miniUtility.compareDicts({'section2': {'item1': 'foo'},
                                         'section1': {'item2': 'eggs', 'item1': 'spam'}},{}) == False

        super["c"] = {"spam": 1, "eggs": 2}
        sub["c"] = {"spam":1}
        assert miniUtility.compareDicts(sub, super) == True
        assert miniUtility.compareDicts(super, sub) == False

        super["c"] = {"spam": 1, "eggs": 2}
        sub["c"] = {"spam":2}
        assert miniUtility.compareDicts(sub, super) == True
        assert miniUtility.compareDicts(super, sub) == False

        super["d"] = [1,2,3]
        sub["d"] = [1,2,3]
        sub["c"] = {"spam":1}
        assert miniUtility.compareDicts(sub, super) == True
        assert miniUtility.compareDicts(super, sub) == False
        
        super["c"] = {"spam": 1, "eggs": 2}
        sub["c"] = {"foo":2}
        assert miniUtility.compareDicts(sub, super) == False
        assert miniUtility.compareDicts(super, sub) == False

if __name__ == "__main__":
    tcommon = Tcommon.Tcommon()
    tcommon.setForTest()
    suite = unittest.TestSuite()
    #suite.addTest(miniUtilityTest("testCheckConfiguration"))
    suite.addTest(unittest.makeSuite(miniUtilityTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
    tcommon.unsetForTest()
    
