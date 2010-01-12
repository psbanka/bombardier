#!/usr/bin/env python

"sets bombardier-core up."

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

What happens when systems you're responsible for go offline?

    * Do you get paged?
    * Are you part of a team that needs to respond immediately?
    * Are your products or services potentially at risk?
    * Is your job or livelihood at risk?

If the answer is yes to any of these questions, then Bombardier may be
for you. Bombardier makes application deployment, minor changes, and
routine maintenance much more reliable and secure. It also provides you
with a degree of insight into the changes that are going on within your
production datacenter that you may have never seen before.
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


setup (name = "bombardier_core",
       description = "Open Source Configuration management and package delivery: shared components",
       version = str("%(branch_nick)s-%(revno)d" % version_info),
       long_description = LONG_DESCRIPTION,
       packages = ["bombardier_core"],
       package_dir = {"bombardier_core": "lib"},
       author = "Peter Banka and Shawn Sherwood",
       author_email = "peter.banka@gmail.com",
       url = "http://bombardierinstaller.org/",
       install_requires = [ 'PyYAML', 'pycrypto' ],
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
