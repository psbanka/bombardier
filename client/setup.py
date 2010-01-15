#!/usr/bin/env python

"sets bombardier_client up."

from distutils.core import Command, setup
from unittest import TextTestRunner, TestLoader
from glob import glob
from os.path import splitext, basename, join as pjoin, walk
import os
from lib._version import version_info

LONG_DESCRIPTION = """\
Bombardier is a software system that delivers visibility, control
and automation to datacenter environments. Bombardier provides a means
for changes to be rolled out to a network of servers in a highly
controlled way, providing optimum security, logging, centralized
change control.

This package is called the "Bombardier Client," and its purpose is to
provide some facilities for the Bombardier Server to make it easier
for the server to do its job.  If you're a stickler for terminology,
then, it's not really a client at all, since is not used to connect to
a server.
"""


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

setup (name = "bombardier_client",
       description = "Open Source Configuration management and package delivery: machine component",
       version = str("%(branch_nick)s-%(revno)d" % version_info),
       long_description = LONG_DESCRIPTION,
       packages = ["bombardier_client"],
       package_dir = {"bombardier_client": "lib"},
       scripts = ["scripts/bc.py"],
       author = "Peter Banka and Shawn Sherwood",
       author_email = "peter.banka@gmail.com",
       url = "http://bombardierinstaller.org/",
       install_requires = [ 'PyYAML', 'bombardier_core' ],
       provides = [ 'bombardier_client' ],
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
