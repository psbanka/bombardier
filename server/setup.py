# setup.py: sets bombardier up.

from distutils.core import setup
from lib._version import version_info
import glob
setup (name = "bombardier_server",
       description = "Open Source Configuration management "\
                     "and package delivery: central server components",
       version = "%(branch_nick)s-%(revno)d" % version_info,
       packages = ["bombardier_server", "bombardier_server.cli", "bombardier_server.web", 
                   "bombardier_server.web.cnm", "bombardier_server.web.cnm.configs",
                   "django_restapi"],
       package_data = {"bombardier_server": ["web/cnm/templates/*.html",
                                             "web/cnm/templates/registration/*.html"]},
       package_dir = {"bombardier_server": "lib", "django_restapi": "lib/django_restapi"},
       data_files = [("web/cnm/templates", glob.glob("web/cnm/templates/*.html")),
                     ("web/cnm/templates/registration", glob.glob("web/cnm/templates/registration/*.html"))],
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
