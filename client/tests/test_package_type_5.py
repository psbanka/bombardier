#!/usr/bin/env python

import sys, os

import setup_tests
setup_tests.cleanup()
setup_tests.start()
sys.path.insert(0, "../lib")

import os, unittest, yaml, re
from PackageV5 import PackageV5
from Exceptions import BadPackage
from bombardier_core.static_data import FAIL, OK
from bombardier_core.static_data import INSTALL, VERIFY
from bombardier_core.Progress import Progress
import Exceptions
import mock
import StringIO
import testdata
import Repository
from bombardier_core.Config import Config

INSTANCE = "TEST_INSTANCE"

class PackageTest(unittest.TestCase):

    def setUp(self):
        self.repository = Repository.Repository(INSTANCE, {})
        self.config = Config(INSTANCE, {})

    def tearDown(self):
        pass

    def test_injector_bad_package(self):
        self.repository.pkg_data = {"t5_bad_pkg1": {"package-version": 5,
                                                    "release": 2,
                                                    "class_name": "TestThing.TestThing",
                                                    "libs": {"BrokenThing": {"path": "MissingTestThing.tar.gz"}},
                                                   }
                                   }
        package = PackageV5("t5_bad_pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        exception_caught = False
        try:
            status = package._download()
        except BadPackage:
            exception_caught = True
        assert exception_caught == True

    def test_install(self):
        self.repository.pkg_data = {"t5pkg1": {"package-version": 5,
                                               "release": 2,
                                               "class_name": "TestThing.TestThing",
                                               "libs": {"TestThing": {"path": "TestThing.tar.gz"}},
                                              }
                                   }
        package = PackageV5("t5pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        package._install(["t5pkg1"])
        assert package.status == OK

    def test_uninstall_malformed_package(self):
        self.repository.pkg_data = {"t5pkg1": {"package-version": 5,
                                               "release": 2,
                                               "class_name": "TestMalformed.TestMalformed",
                                               "libs": {"TestMalformed": {"path": "TestMalformed.tar.gz"}},
                                              }
                                   }
        package = PackageV5("t5pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        caught_exception = False
        try:
            package.uninstall()
        except BadPackage:
            caught_exception = True
        assert caught_exception == True
        assert package.status == FAIL, "uninstallation of a bogus package succeeded"

    def test_uninstall_ok_package(self):
        self.repository.pkg_data = {"t5_pkg1": {"package-version": 5,
                                                "release": 2,
                                                "class_name": "TestThing.TestThing",
                                                "libs": {"TestThing": {"path": "TestThing.tar.gz"}},
                                               }
                                   }
        package = PackageV5("t5_pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        package.uninstall()
        assert package.status == OK, "Legitimate package uninstallation failed"

    def test_uninstall_error_script(self):
        self.repository.pkg_data = {"t5pkg1_bad_uninstall": {"package-version": 5,
                                               "release": 2,
                                               "class_name": "TestBadUninstall.TestBadUninstall",
                                               "libs": {"TestBadUninstall": {"path": "TestBadUninstall.tar.gz"}},
                                              }
                                   }
        package = PackageV5("t5pkg1_bad_uninstall", self.repository, self.config, INSTANCE)
        package.initialize()
        package.uninstall()
        assert package.status == FAIL, "Uninstallation of a package that returns an error succeeded"

    def test_verify_ok_package(self):
        self.repository.pkg_data = {"t5_pkg1": {"package-version": 5,
                                                "release": 2,
                                                "class_name": "TestThing.TestThing",
                                                "libs": {"TestThing": {"path": "TestThing.tar.gz"}},
                                               }
                                   }
        package = PackageV5("t5_pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        package._install([])
        package.verify()
        assert package.status == OK, "Legitimate package verification failed"

    def test_verify_bad_package(self):
        self.repository.pkg_data = {"t5_pkg1": {"package-version": 5,
                                                "release": 2,
                                                "class_name": "TestBadVerify.TestBadVerify",
                                                "libs": {"TestBadVerify": {"path": "TestBadVerify.tar.gz"}},
                                               }
                                   }
        package = PackageV5("t5_pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        package.action = VERIFY
        package.verify()
        assert package.status == FAIL, "Verification of a package that returns an error succeeded"

    def test_download(self):
        assert 1 == 1 # ^^^ FIXME

    def test_process(self):
        assert 1 == 1 # ^^^ FIXME

    def test_write_progress_basic(self):
        self.repository.pkg_data = {"t5_pkg1": {"package-version": 5,
                                                "release": 2,
                                                "class_name": "TestThing.TestThing",
                                                "libs": {"TestThing": {"path": "TestThing.tar.gz"}},
                                               }
                                   }
        package = PackageV5("t5_pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        package.action = INSTALL
        status = package._write_progress()
        assert status == OK

        status_data = Progress(INSTANCE).load_current_progress()
        install_progress = status_data["install-progress"]
        matches = install_progress.get("t5_pkg1-2")

        package.status = FAIL
        status = package._write_progress()
        assert status == OK


    def test_write_progress_repeat(self):

        self.repository.pkg_data = {"t5_pkg1": {"package-version": 5,
                                                "release": 2,
                                                "class_name": "TestThing.TestThing",
                                                "libs": {"TestThing": {"path": "TestThing.tar.gz"}},
                                               }
                                   }
        package = PackageV5("t5_pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        package.action = INSTALL
        status = package._write_progress()
        assert status == OK
        status = package._write_progress()
        assert status == OK

    def test_write_progress_empty(self):
        self.repository.pkg_data = {"t5_pkg1": {"package-version": 5,
                                                "release": 2,
                                                "class_name": "TestThing.TestThing",
                                                "libs": {"TestThing": {"path": "TestThing.tar.gz"}},
                                               }
                                   }
        package = PackageV5("t5_pkg1", self.repository, self.config, INSTANCE)
        package.initialize()
        package.action = INSTALL
        status = package._write_progress()
        assert status == OK

    def test_bad_package_creation(self):
        self.repository.pkg_data = {"t5_pkg1": {"package-version": 5,
                                                "release": 2,
                                                "class_name": None,
                                                "libs": {"TestThing": {"path": "TestThing.tar.gz"}},
                                               }
                                   }
        status = FAIL
        try:
            package = PackageV5("t5_pkg1", self.repository, self.config, INSTANCE)
            package.initialize()
        except Exceptions.BadPackage, e:
            status = OK
            assert e.errmsg == "No script infomation for this package", e.errmsg
        assert status == OK

    def test_package_creation(self):
        self.repository.pkg_data = {"t5_pkg1": {"package-version": 5,
                                                "release": 2,
                                                "class_name": "TestThing.TestThing",
                                                "libs": {"TestThing": {"path": "TestThing.tar.gz"}},
                                               }
                                   }
        package = PackageV5("t5_pkg1", self.repository, self.config, INSTANCE)
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
    #suite.addTest(PackageTest("test_uninstall_malformed_package"))
    #suite.addTest(PackageTest("test_verify_ok_package"))
    suite.addTest(unittest.makeSuite(PackageTest))
    status = unittest.TextTestRunner(verbosity=2).run(suite)
    errors = len(status.errors) + len(status.failures)
    if not errors:
        setup_tests.cleanup()
