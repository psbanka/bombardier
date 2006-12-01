#!/cygdrive/c/Python24/python.exe

# srcUpdate.py: Tool for running setup.py install on dmoweasel,
#               bombardier, spkg, and site-root to update just
#               the python libraries. To use this, check out 
#               the bombardier/trunk/client dirctory and run
#               this script. This is just a simple tool, and
#               will not necessarily work if the currently 
#               installed version is too old. 


# Copyright (C) 2005 Peter Banka, Shawn Sherwood

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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

import sys, os, shutil

def updatePython():
    print "Updating python libraries."
    if sys.platform == "win32":
        pythonPath = os.path.join(sys.prefix, "python.exe")
    else:
        # It may take more than this to get this working on Linux.
        pythonPath = os.path.join(sys.prefix, "bin", "python")

    installCmd = "%s setup.py install" % pythonPath 
    todos = ["dmoweasel", "spkgDir", "site-root"]
    startDir = os.getcwd()
    for todo in todos:
        os.chdir( todo )
        os.system( installCmd )
        os.chdir( startDir )
    os.system( "%s bombardier/setup.py install" %pythonPath )

if __name__ == '__main__':
    updatePython()

