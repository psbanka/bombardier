#!/cygdrive/c/Python24/python.exe

# srcUpdate.py: Tool for running setup.py install on,
#               bombardier, and spkg to update just
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

def robustGetSpkgPath():
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
    return spkgPath

def updateSpkgDir(startDir, instanceName):
    spkgPath = robustGetSpkgPath()
    os.chdir( spkgPath )

    print "Cleaning spkgDir..."

    for i in os.listdir('.'):
        if os.path.isfile(i) and not i.startswith('bombardier.log'):
            try:
                os.unlink( i )
            except:
                print "unable to delete %s" % i
        else:
            print "SAVED: %s" % i

    os.chdir( "%s/spkgDir" % startDir )
    pkgsDir = "%s/%s/packages" % (instanceName, spkgPath)
    statusFile = "%s/status.yml" % pkgsDir
    if not os.path.isdir(pkgsDir):
        os.makedirs(pkgsDir)
    if not os.path.isfile(statusFile):
        open(statusFile, 'w').write("---\nstatus:\n   newInstall: True\n")

    noFileWarningTemplate = "Warning! %s/%s does not exist. Not copying." 
    for inode in os.listdir("."):
        if os.path.isfile(inode):
            spkgFile = os.path.join(spkgPath, inode)
            if not os.path.isfile(spkgFile):
                sys.stdout.write( "copying %s -> %s\n" % (inode, spkgPath) )
                shutil.copy(inode, spkgFile)
        else:
            print noFileWarningTemplate % (os.getcwd(), inode)

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

def updatePython(instanceName):
    print "Updating python libraries."
    if sys.platform == "win32":
        pythonPath = os.path.join(sys.prefix, "python.exe")
    else:
        pythonPath = os.path.join(sys.prefix, "bin", "python2.4")
        if not os.path.isfile(pythonPath):
            pythonPath = os.path.join(sys.prefix, "bin", "python")

    startDir = os.getcwd()
    updateSpkgDir(startDir, instanceName)
    updateBombardier(startDir, pythonPath)

if __name__ == '__main__':
    instanceName = sys.argv[1]
    updatePython(instanceName)

