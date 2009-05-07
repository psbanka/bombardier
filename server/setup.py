# setup.py: sets bombardier up.

from distutils.core import setup
from lib._version import version_info
setup (name = "bombardier_server",
       description = "Open Source Configuration management "\
                     "and package delivery: central server components",
       version = "%(branch_nick)s-%(revno)d" % version_info,
       packages = ["bombardier_server", "bombardier_server.cli", "bombardier_server.web", "django_restapi"],
       package_dir = {"bombardier_server": "lib", "django_restapi": "lib/django_restapi"},
       scripts = ["scripts/bomsh", "scripts/authorize.py", "scripts/dsl",
                  "scripts/BackupJob.py", "scripts/ymlEncrypter.py"],
       author = "Peter Banka and Shawn Sherwood",
       author_email = "peter.banka@gmail.com",
       url = "http://bombardierinstaller.org/"
)
