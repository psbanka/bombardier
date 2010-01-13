# setup.py: sets bombardier up.

from distutils.core import setup
from lib._version import version_info
import glob
setup (name = "bombardier_cli",
       description = "Open Source Configuration management "\
                     "and package delivery: central server components",
       version = "%(branch_nick)s-%(revno)d" % version_info,
       packages = ["bombardier_cli"],
       package_dir = {"bombardier_cli": "lib"},
       scripts = ["scripts/bdr", "scripts/dsl", "scripts/ymlEncrypter.py"],
       author = "Peter Banka and Shawn Sherwood",
       author_email = "peter.banka@gmail.com",
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
