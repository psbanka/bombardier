#!/cygdrive/c/Python24/python.exe

# setup.py: sets up the goodness of the weasel

# Copyright (C) 2005 Seth de l'Isle, Peter Banka

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

from distutils.core import setup

setup(name='dmoweasel',
      version='0.4', 
      packages  = ["dmoweasel"],
      author = "Seth de l'Isle, Peter Banka",
      author_email = "peter.banka@ge.com",
      scripts=[ 'db-browser2.py',
                'db-data2.py',
                'db-struct2.py',
                'db-users2.py']
      )
