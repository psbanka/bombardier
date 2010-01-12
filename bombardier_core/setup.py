#!/usr/bin/env python

"sets bombardier-core up."

from distutils.core import Command, setup
from unittest import TextTestRunner, TestLoader
from glob import glob
from os.path import splitext, basename, join as pjoin, walk
import os
from lib._version import version_info

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
        for t in glob(pjoin(self._dir, 'tests', '*.py')):
            if not t.endswith('__init__.py'):
                testfiles.append('.'.join(
                    ['tests', splitext(basename(t))[0]])
                )

        tests = TestLoader().loadTestsFromNames(testfiles)
        t = TextTestRunner(verbosity = 1)
        t.run(tests)


setup (name = "bombardier_core",
       description = "Open Source Configuration management and package delivery: shared components",
       version = str("%(branch_nick)s-%(revno)d" % version_info),
       packages = ["bombardier_core"],
       package_dir = {"bombardier_core": "lib"},
       author = "Peter Banka and Shawn Sherwood",
       author_email = "peter.banka@gmail.com",
       url = "http://bombardierinstaller.org/",
       requires = [ 'PyYAML', 'pycrypto' ],
       provides = [ 'bombardier_core' ],
       classifiers = [
           "Development Status :: 2 - Pre-Alpha",
           "Programming Language :: Python",
           "Intended Audience :: Developers",
           "Intended Audience :: Information Technology",
           "License :: OSI Approved :: BSD License",
           "Operating System :: POSIX",
           "Topic :: Software Development :: Libraries :: Python Modules",
           "Topic :: System :: Archiving :: Packaging",
       ],
       cmdclass = { 'test': TestCommand, 'clean': CleanCommand }
)
