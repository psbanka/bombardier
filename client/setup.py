# setup.py: sets bombardier up.

from distutils.core import setup
from lib._version import version_info
setup (name = "bombardier",
       description = "Open Source Configuration management and package delivery: client components",
       version = str("%(branch_nick)s-%(revno)d" % version_info),
       packages = ["bombardier"],
       scripts = ["scripts/bc.py"],
       package_dir = {"bombardier": "lib"},
       author = "Peter Banka and Shawn Sherwood",
       author_email = "peter.banka@gmail.com",
       url = "http://bombardierinstaller.org/"
)
