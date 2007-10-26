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
from bombardier.miniUtility import getSpkgPath

def setRegistry(spkgPath):
    import _winreg
    hklm = _winreg.OpenKey(_winreg.HKEY_LOCAL_MACHINE, 'SOFTWARE')
    bomKey = _winreg.CreateKey(hklm,'GE-IT\\Bombardier')
    _winreg.SetValueEx(bomKey, 'InstallPath', 0, _winreg.REG_SZ, spkgPath)

def updateSpkgDir(startDir, pythonPath):
    print "Cleaning spkgDir..."
    try:
        spkgPath = getSpkgPath()
    except:
        if sys.platform == "win32":
            spkgPath = "c:\\spkg"
            setRegistry(spkgPath)
        else:
            spkgPath = "/opt/spkg"
            open('/etc/bombardier.yml', 'w').write('---\nspkgPath: /opt/spkg\n')
    spkgPath = getSpkgPath()
    if not os.path.isdir(spkgPath):
        os.makedirs(spkgPath)
    os.chdir( spkgPath )
    for i in os.listdir('.'):
        if os.path.isfile(i) and i != 'status.yml' and 'bombardier.log' not in i:
            os.unlink( i )
        else:
            print "SAVED: %s" %i

    os.chdir( "%s/spkgDir" %startDir )
    os.system( "%s setup.py install %s" %(pythonPath, spkgPath) )
    os.chdir( startDir )

def updateBombardier(startDir, pythonPath):
    os.chdir( startDir )
    for s in sys.path:
        if s.endswith('site-packages') and 'bombardier' in os.listdir(s):
            os.chdir(s)
            os.system('rm -rf bombardier')
            print "DELETED: bombardier"
            os.chdir(startDir)

    os.system( "%s bombardier/setup.py install" %pythonPath )

def updatePython():
    print "Updating python libraries."
    if sys.platform == "win32":
        pythonPath = os.path.join(sys.prefix, "python.exe")
    else:
        pythonPath = os.path.join(sys.prefix, "bin", "python2.4")

    startDir = os.getcwd()
    updateSpkgDir(startDir, pythonPath)
    updateBombardier(startDir, pythonPath)

if __name__ == '__main__':
    updatePython()

