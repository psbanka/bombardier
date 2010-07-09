#!/usr/bin/env python

"sets up one of the bombardier components"

from distutils.core import Command, setup
from unittest import TextTestRunner, TestLoader
from os.path import splitext, basename, join as pjoin, walk
import os
from distutils.core import setup
from lib._version import version_info
import glob
import yaml

MANIFEST_EXTRA = "MANIFEST.in.extra"

class CleanCommand(Command):
    user_options = [ ]
    def initialize_options(self):
        self._clean_me = [ ]
        for root, dirs, files in os.walk('.'):
            for f in files:
                if f.endswith('.pyc'):
                    self._clean_me.append(pjoin(root, f))

    def finalize_options(self):
        pass

    def run(self):
        for clean_me in self._clean_me:
            try:
                os.unlink(clean_me)
                print "%s deleted." % clean_me
            except:
                pass

class TestCommand(Command):
    user_options = [ ]
    def initialize_options(self):
        self._dir = os.getcwd()

    def finalize_options(self):
        pass

    def run(self):
        '''
        Finds all the tests modules in tests/, and runs them.
        '''
        testfiles = [ ]
        for t in glob.glob(pjoin(self._dir, 'tests', '*.py')):
            if not t.endswith('__init__.py'):
                testfiles.append('.'.join(
                    ['tests', splitext(basename(t))[0]])
                )

        print "TEST FILES:",testfiles
        tests = TestLoader().loadTestsFromNames(testfiles)
        t = TextTestRunner(verbosity = 1)
        t.run(tests)

def main(package_data):
    cmdclasses = {}
    # 'test' is the parameter as it gets added to setup.py

    setup(
          cmdclass = { 'test': TestCommand, 'clean': CleanCommand },
          name = package_data["name"],
          description = package_data["description"],
          version = "%(branch_nick)s-%(revno)d" % version_info,
          packages = package_data["packages"],
          package_data = package_data.get("package_data", {}),
          package_dir = package_data["package_dir"],
          data_files = package_data.get("data_files"),
          author = package_data["author"],
          author_email = package_data["author_email"],
          long_description = package_data["long_description"],
          scripts = package_data.get("scripts"),
          install_requires = package_data["install_requires"],
          provides = package_data["provides"],
          url = package_data["url"],
          classifiers = package_data["classifiers"],
         )

if __name__ == "__main__":
    package_data = yaml.load(open("project_info.yml").read())
    component_data = yaml.load(open("component_info.yml").read())
    if os.path.isfile(MANIFEST_EXTRA):
        append_manifest_data = open(MANIFEST_EXTRA).read()
        if append_manifest_data:
            open("MANIFEST.in", 'a').write(append_manifest_data)
    package_data.update(component_data)
    main(package_data)
