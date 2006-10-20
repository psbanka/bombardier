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

    def testHashDictionary(self):
        d = {"1": "2", 3: ["a", "b", "c"], "hello": 2, "joe": {1:2}}
        h = miniUtility.hashDictionary(d)
        assert h["1"] == 'c81e728d9d4c2f636f067f89cc14862c', "Improper hash value"
        assert h[3][0] == '0cc175b9c0f1b6a831c399e269772661', "Improper hash value"

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

    def testDiffDictionaries(self):
        super = {"a": 1, "b": 2}
        sub   = {"a": 1}
        assert miniUtility.diffDicts(sub, super) == {}, miniUtility.diffDicts(sub, super)
        assert miniUtility.diffDicts(super, sub) == {"b": 2}

        assert miniUtility.diffDicts(sub, {}) == {"a": 1}
        assert miniUtility.diffDicts({'section2': {'item1': 'foo'},
                                         'section1': {'item2': 'eggs', 'item1': 'spam'}},{}) != {}

        super["c"] = {"spam": 1, "eggs": 2}
        sub["c"] = {"spam":1}
        assert miniUtility.diffDicts(sub, super) == {}
        diffs = miniUtility.diffDicts(super, sub)
        assert diffs.keys() == ['c', 'b']
        assert diffs['c'] == {'eggs': 2}
        assert diffs['b'] == 2

        super = {}
        sub = {}
        super["c"] = {"spam": 1, "eggs": 2}
        sub["c"] = {"spam":2}
        assert miniUtility.diffDicts(sub, super) == {}
        assert miniUtility.diffDicts(super, sub) == {"c": {"eggs": 2}}, miniUtility.diffDicts(super, sub)

        super = {}
        sub   = {}
        super["d"] = [1,2,3]
        sub["d"] = [1,2,3]
        super["c"] = {"spam":1}
        assert miniUtility.diffDicts(sub, super) == {}, miniUtility.diffDicts(sub, super)
        assert miniUtility.diffDicts(super, sub) == {"c": {"spam": 1}}, miniUtility.diffDicts(super, sub)
        
        super["c"] = {"spam": 1, "eggs": 2}
        sub["c"] = {"foo":2}
        assert miniUtility.diffDicts(sub, super) == {"c": {"foo": 2}}
        assert miniUtility.diffDicts(super, sub) == {"c": {"spam":1, "eggs":2}}



if __name__ == "__main__":
    tcommon = Tcommon.Tcommon()
    tcommon.setForTest()
    suite = unittest.TestSuite()
    #suite.addTest(miniUtilityTest("testDiffDictionaries"))
    suite.addTest(unittest.makeSuite(miniUtilityTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
    tcommon.unsetForTest()
    
