#!/usr/bin/env python

import sys
import setup_tests
setup_tests.cleanup()
setup_tests.start()
sys.path.insert(0, "../lib")

import unittest
import os
import Repository as Repository
from bombardier_core.Config import Config
from bombardier_core.static_data import OK, FAIL

INSTANCE = "TEST_INSTANCE"

class RepositoryTest(unittest.TestCase):

    def setUp(self):
        self.config = Config(INSTANCE, {})
        pkg_data = {"TestPackage-7": {"install": {"fullName": "TestPackage-7-1"}}}
        self.repository = Repository.Repository("TEST_INSTANCE", pkg_data)
        
    def tearDown(self):
        pass

    def test_get_meta_data(self):
        metaData = self.repository.get_meta_data("TestPackage-7")
        assert metaData["install"]["fullName"] == "TestPackage-7-1"

    def test_unzip(self):
        pkg_path = os.path.join("packages", "TestPackage-7")
        status = self.repository.unzip_type_4(pkg_path, "TestPackage-7-1")
        assert status == OK

if __name__ == "__main__":
    suite = unittest.TestSuite()
    #suite.addTest(RepositoryTest("test_get_meta_data"))
    suite.addTest(unittest.makeSuite(RepositoryTest))
    status = unittest.TextTestRunner(verbosity=2).run(suite)
    errors = len(status.errors) + len(status.failures)
    if not errors:
        setup_tests.cleanup()
