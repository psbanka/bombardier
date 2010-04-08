#!/usr/bin/env python

"sets bombardier_server up."

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



from distutils.core import setup
from lib._version import version_info
import glob
setup (name = "bombardier_server",
       description = "Open Source Configuration management "\
                     "and package delivery: central server components",
       version = "%(branch_nick)s-%(revno)d" % version_info,
       packages = ["bombardier_server", "bombardier_server.web", 
                   "bombardier_server.web.cnm", "bombardier_server.web.cnm.configs",
                   "django_restapi"],
       package_data = {"bombardier_server": ["web/cnm/templates/*.html",
                                             "web/cnm/templates/registration/*.html"]},
       package_dir = {"bombardier_server": "lib", "django_restapi": "lib/django_restapi"},
       data_files = [("web/cnm/templates", glob.glob("web/cnm/templates/*.html")),
                     ("web/cnm/templates/registration", glob.glob("web/cnm/templates/registration/*.html"))],
       author = "Peter Banka and Shawn Sherwood",
       author_email = "peter.banka@gmail.com",
       long_description = LONG_DESCRIPTION,
       scripts = ["scripts/bdr_server_quickstart"],
       install_requires = [ 'PyYAML', 'bombardier_core', "Pyro", "Django", "pexpect", "pysqlite" ],
       provides = [ 'bombardier_server' ],
       url = "http://bombardierinstaller.org/",
       classifiers = [
          "Development Status :: 2 - Pre-Alpha",
          "Programming Language :: Python",
          "Environment :: Web Environment",
          "Framework :: Django",
          "Intended Audience :: Developers",
          "Intended Audience :: Information Technology",
          "License :: OSI Approved :: BSD License",
          "Operating System :: POSIX",
          "Topic :: Internet :: WWW/HTTP :: Site Management",
          "Topic :: Security",
          "Topic :: Software Development :: Build Tools",
          "Topic :: Software Development :: Libraries :: Python Modules",
          "Topic :: Software Development :: Quality Assurance",
          "Topic :: Software Development :: Version Control",
          "Topic :: System :: Archiving :: Packaging",
          "Topic :: System :: Installation/Setup",
          "Topic :: System :: Logging",
          "Topic :: System :: Shells",
          "Topic :: System :: Software Distribution",
          "Topic :: System :: Systems Administration",
       ]
)
