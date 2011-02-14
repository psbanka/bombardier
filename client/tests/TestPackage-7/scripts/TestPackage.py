#!/usr/local/bin/python2.4

from bombardier_core.static_data import OK, FAIL
from bombardier_core.Spkg import SpkgV4
from bombardier_core.mini_utility import make_path
import shutil, sys, os
import yaml

metadata = {
    "package-version": 4,
    "install": {"reboot": "False",
                "priority": "100"},
    "dependencies": []
    }

class TestPackage(SpkgV4):

    def __init__(self, config, logger):
        SpkgV4.__init__(self, config, logger)
        self.thing = config.get("test", "value", "foo")
        self.directory = config.get("test", "directory", "c:/test")
        self.test_path  = make_path(self.directory, "TEST.TXT")

    def configure(self):
        self.info( "configuring..." )
        if not os.path.isdir(self.directory):
            self.info("creating directory %s" % self.directory)
            os.makedirs(self.directory)
        fp = open(self.test_path, 'w')
        fp.write(self.thing)
        fp.flush()
        fp.close()
        return OK

    def installer(self):
        self.info( "installing..." )
        self.configure()
        return OK

    def uninstaller(self):
        self.info( "uninstalling..." )
        os.unlink(self.test_path)
        if os.path.isdir(self.directory):
            self.info("removing directory %s" % self.directory)
            os.rmdir(self.directory)
        return OK

    def verify(self):
        self.info( "verify..." )
        self.info( "self.test_path: %s" % self.test_path )
        if os.path.isdir(self.directory):
            data = open(self.test_path).read()
            if data == self.thing:
                return OK
            else:
                self.info("Contents of test_path invalid: %s" % data)
        else:
            self.info("self.test_path not a directory : %s" % self.test_path)
        return FAIL
