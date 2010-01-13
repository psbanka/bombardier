#!/usr/bin/env python

import sys, os

import setup_tests
setup_tests.cleanup()
setup_tests.start()
sys.path.insert(0, "../lib")

import os, unittest, yaml, re
from PackageV4 import PackageV4
from Exceptions import BadPackage
from bombardier_core.static_data import FAIL, OK
from bombardier_core.static_data import INSTALL, VERIFY
from bombardier_core.mini_utility import load_current_progress
import Exceptions
import mock
import MockObjects
import StringIO
import testdata

INSTANCE = "TEST_INSTANCE"

class PackageTest(unittest.TestCase):

    def setUp(self):
        data = {"testokpackage1":{"install": {"fullName": None}}}
        self.repository = MockObjects.MockRepository(data)
        self.config = MockObjects.MockConfig()

    def tearDown(self):
        pass

    def test_injector_bad_package(self):
        self.repository.packages = {"foo": {"install": {"fullName": "foo-1"}}}
        package = PackageV4("foo", self.repository, self.config, INSTANCE)
        package.initialize()
        calls = self.repository.getAllCalls()
        assert len(calls) == 1, calls
        assert `calls[0]` == "get_meta_data('foo')", calls[0]
        exception_caught = False
        try:
            status = package._download()
        except BadPackage:
            exception_caught = True
        assert exception_caught == True

    def test_install(self):
        self.repository.packages = {"pkg1":{"install": {"fullName": "TestPackage-7"}}}
        package = PackageV4("pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        package._install(["pkg1"])
        assert package.status == OK

    def test_uninstall_malformed_package(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "TestMalformed"}}}
        package = PackageV4("pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        caught_exception = False
        try:
            package.uninstall()
        except BadPackage:
            caught_exception = True
        assert caught_exception == True
        assert package.status == FAIL, "uninstallation of a bogus package succeeded"

    def test_uninstall_ok_package(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "TestPackage-7"}}}
        base = os.path.join(os.getcwd(), "packages", "TestPackage-7")
        package = PackageV4("pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        package.uninstall()
        assert package.status == OK, "Legitimate package uninstallation failed"

    def test_uninstall_error_script(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "TestBadUninstall"}}}
        base = os.path.join(os.getcwd(), "packages", "TestBadUninstall")
        package = PackageV4("pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        package.uninstall()
        assert package.status == FAIL, "Uninstallation of a package that returns an error succeeded"

    def test_verify_ok_package(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "TestPackage-7"}}}
        base = os.path.join(os.getcwd(), "packages", "TestPackage-7")
        package = PackageV4("pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        package._install([])
        package.verify()
        assert package.status == OK, "Legitimate package verification failed"

    def test_verify_bad_package(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "TestBadVerify"}}}
        base = os.path.join(os.getcwd(), "packages", "TestBadVerify")
        package = PackageV4("pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        package.action = VERIFY
        package.verify()
        assert package.status == FAIL, "Verification of a package that returns an error succeeded"

    def test_download(self):
        assert 1 == 1 # ^^^ FIXME

    def test_process(self):
        assert 1 == 1 # ^^^ FIXME

    def test_write_progress_basic(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "testokpackage1-1"}}}
        package = PackageV4("pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        package.action = INSTALL
        status = package._write_progress()
        assert status == OK

        status_data = load_current_progress(INSTANCE)
        install_progress = status_data["install-progress"]
        matches = install_progress.get("testokpackage1-1")

        package.status = FAIL
        status = package._write_progress()
        assert status == OK


    def test_write_progress_repeat(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "testokpackage1-1"}}}
        package = PackageV4("pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        package.action = INSTALL
        status = package._write_progress()
        assert status == OK
        status = package._write_progress()
        assert status == OK

    def test_write_progress_empty(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "testokpackage1-1"}}}
        package = PackageV4("pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        package.action = INSTALL
        status = package._write_progress()
        assert status == OK

    def test_bad_package_creation(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": None}}}
        status = FAIL
        try:
            package = PackageV4("pkg1", self.repository, self.config, INSTANCE)
            package.initialize()
        except Exceptions.BadPackage, e:
            status = OK
            assert e.errmsg == "Package does not exist on the server"
        assert status == OK
        calls = self.repository.getAllCalls()
        assert `calls[0]` == "get_meta_data('pkg1')"

    def test_package_creation(self):
        self.repository.packages = {"pkg1": {"install": {"fullName": "testokpackage1-1"}}}
        package = PackageV4("pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        assert package.status == OK

if __name__ == "__main__":
    suite = unittest.TestSuite()
    #suite.addTest(PackageTest("test_bad_package_creation"))
    #suite.addTest(PackageTest("test_write_progress_repeat"))
    #suite.addTest(PackageTest("test_write_progress_basic"))
    #suite.addTest(PackageTest("test_package_creation"))
    #suite.addTest(PackageTest("test_install"))
    #suite.addTest(PackageTest("test_injector_bad_package"))
    #suite.addTest(PackageTest("test_verify_bad_package"))
    #suite.addTest(PackageTest("test_uninstall_error_script"))
    #suite.addTest(PackageTest("test_verify_ok_package"))
    suite.addTest(unittest.makeSuite(PackageTest))
    status = unittest.TextTestRunner(verbosity=2).run(suite)
    errors = len(status.errors) + len(status.failures)
    print "ERRORS:",errors
    if not errors:
        setup_tests.cleanup()
