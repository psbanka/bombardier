#!/cygdrive/c/Python25/python.exe

import os, sys, unittest
sys.path.insert(0, "../lib")

import mini_utility as mini_utility

class MiniUtilityTest(unittest.TestCase):

    def test_hash_dictionary(self):
        d = {"1": "2", 3: ["a", "b", "c"], "hello": 2, "joe": {1:2}}
        h = mini_utility.hash_dictionary(d)
        assert h["1"] == 'c81e728d9d4c2f636f067f89cc14862c', "Improper hash value"
        assert h[3][0] == '0cc175b9c0f1b6a831c399e269772661', "Improper hash value"

    def test_compare_dicts(self):
        super = {"a": 1, "b": 2}
        sub   = {"a": 1}
        assert mini_utility.compare_dicts(sub, super) == True
        assert mini_utility.compare_dicts(super, sub) == False

        assert mini_utility.compare_dicts(sub, {}) == False
        assert mini_utility.compare_dicts({'section2': {'item1': 'foo'},
                                         'section1': {'item2': 'eggs', 'item1': 'spam'}},{}) == False

        super["c"] = {"spam": 1, "eggs": 2}
        sub["c"] = {"spam":1}
        assert mini_utility.compare_dicts(sub, super) == True
        assert mini_utility.compare_dicts(super, sub) == False

        super["c"] = {"spam": 1, "eggs": 2}
        sub["c"] = {"spam":2}
        assert mini_utility.compare_dicts(sub, super) == True
        assert mini_utility.compare_dicts(super, sub) == False

        super["d"] = [1,2,3]
        sub["d"] = [1,2,3]
        sub["c"] = {"spam":1}
        assert mini_utility.compare_dicts(sub, super) == True
        assert mini_utility.compare_dicts(super, sub) == False
        
        super["c"] = {"spam": 1, "eggs": 2}
        sub["c"] = {"foo":2}
        assert mini_utility.compare_dicts(sub, super) == False
        assert mini_utility.compare_dicts(super, sub) == False

    def test_diff_dictionaries(self):
        super = {"a": 1, "b": 2}
        sub   = {"a": 1}
        assert mini_utility.diff_dicts(sub, super) == {}, mini_utility.diff_dicts(sub, super)
        assert mini_utility.diff_dicts(super, sub) == {"b": 2}

        assert mini_utility.diff_dicts(sub, {}) == {"a": 1}
        assert mini_utility.diff_dicts({'section2': {'item1': 'foo'},
                                         'section1': {'item2': 'eggs', 'item1': 'spam'}},{}) != {}

        super["c"] = {"spam": 1, "eggs": 2}
        sub["c"] = {"spam":1}
        assert mini_utility.diff_dicts(sub, super) == {}
        diffs = mini_utility.diff_dicts(super, sub)
        assert diffs.keys() == ['c', 'b']
        assert diffs['c'] == {'eggs': 2}
        assert diffs['b'] == 2

        super = {}
        sub = {}
        super["c"] = {"spam": 1, "eggs": 2}
        sub["c"] = {"spam":2}
        assert mini_utility.diff_dicts(sub, super) == {}
        assert mini_utility.diff_dicts(super, sub) == {"c": {"eggs": 2}}, mini_utility.diff_dicts(super, sub)

        super = {}
        sub   = {}
        super["d"] = [1,2,3]
        sub["d"] = [1,2,3]
        super["c"] = {"spam":1}
        assert mini_utility.diff_dicts(sub, super) == {}, mini_utility.diff_dicts(sub, super)
        assert mini_utility.diff_dicts(super, sub) == {"c": {"spam": 1}}, mini_utility.diff_dicts(super, sub)
        
        super["c"] = {"spam": 1, "eggs": 2}
        sub["c"] = {"foo":2}
        assert mini_utility.diff_dicts(sub, super) == {"c": {"foo": 2}}
        assert mini_utility.diff_dicts(super, sub) == {"c": {"spam":1, "eggs":2}}



if __name__ == "__main__":
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(MiniUtilityTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
    
