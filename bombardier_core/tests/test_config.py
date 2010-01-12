"""Unit test for Config.py"""

import bombardier_core.Config
import unittest

class TestConfig(unittest.TestCase):
    def test_to_string(self):                          
        """Shows how string method shoudl return a string or cast it"""
        data = {"section": {"item1": "value1", "item2": 2, "item3": [1,2,3]}}
        config = bombardier_core.Config.Config("tester", data)
        output = config.string("section.item1")
        self.assertEqual("value1", output)
        output = config.string("section.item234", default = "not_defined")
        self.assertEqual("not_defined", output)
        output = config.string("section.item2")
        self.assertEqual("2", output)

    def test_to_integer(self):                          
        """Likewise should return an integer"""
        data = {"section": {"item1": "value1", "item2": 2, "item3": [1,2,3]}}
        config = bombardier_core.Config.Config("tester", data)
        output = config.integer("section.item2")
        self.assertEqual(2, output)

    def test_to_dictionary(self):                          
        """Likewise should return an integer"""
        data = {"section": {"item1": "value1", "item2": 2, "item3": [1,2,3]}}
        config = bombardier_core.Config.Config("tester", data)
        output = config.dictionary("section")
        expected = {"item1": "value1", "item2": 2, "item3": [1,2,3]}
        self.assertEqual(expected, output)
    
if __name__ == "__main__":
    unittest.main()   
