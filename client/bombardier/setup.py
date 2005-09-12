# setup.py: sets bombardier up.

# Copyright (C) 2005 Peter Banka

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

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
       description = "Open Source Configuration management and package delivery",
       version = VERSION,
       packages = ["bombardier"],
       author = "Peter Banka et al",
       author_email = "peter@thebankas.com",
       url = "http://bombardierinstaller.org/"
)
