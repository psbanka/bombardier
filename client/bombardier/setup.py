#!/cygdrive/c/Python24/python.exe

try:
    from bombardier.staticData import VERSION
except ImportError:
    VERSION = "(unknown)"

"""
Prequisites:
1. This thing expects to be sitting in the middle of an exploded
   bombardier directory
2. Python must be installed (obviously)
3. This thing will be run by rescue, or will be run by hand, or may be
   run by installer.py as part of a bombardier update package.
"""

from distutils.core import setup
setup (name = "bombardier",
       description = "A Pure Python Yaml Parser Dumper",
       version = VERSION,
       packages = ["bombardier"],
       author = "Peter Banka et al",
       author_email = "peter@thebankas.com",
       url = "http://www.thebankas.com/bombardier/"
)
