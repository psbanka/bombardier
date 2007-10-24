#!/cygdrive/c/Python24/python.exe

# setup.py: distutils program for installing bombardier

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

import os, shutil, sys

"""
Prequisites:
1. This thing expects to be sitting in the middle of an exploded
   spkg directory
2. Python must be installed (obviously)
3. This thing will be run by rescue, or will be run by hand, or may be
   run by installer.py as part of a bombardier update package.
"""

def install(spkgPath):
    setRegistry(spkgPath)
    if not os.path.isdir(spkgPath):
        os.makedirs(spkgPath)
    configFiles = [ 'repositoryDirectory.yml', 'status.yml' ]
    noFileWarningTemplate = "Warning! %s/%s does not exist. Not copying." 
    for inode in os.listdir("."):
        if os.path.isfile(inode):
            spkgFile = os.path.join(spkgPath, inode)
            if not inode in configFiles or not os.path.isfile(spkgFile):
                sys.stdout.write( "copying %s -> %s\n" % (inode, spkgPath) )
                shutil.copy(inode, spkgFile)
        else:
            print noFileWarningTemplate % (os.getcwd(), inode)

    if sys.platform == "win32":
        system32Path = os.path.join( os.environ['WINDIR'], "system32" )
        regSvr = os.path.join(system32Path, "regsvr32.exe")
        dlls = ["AutoItX3.dll"]
        for dll in dlls:
            os.system("%s /s %s" % (regSvr, dll))

    sys.stdout.write("Successfully updated spkg.\n")

def setRegistry(spkgPath):
    import _winreg
    hklm = _winreg.OpenKey(_winreg.HKEY_LOCAL_MACHINE, 'SOFTWARE')
    bomKey = _winreg.CreateKey(hklm,'GE-IT\\Bombardier')
    _winreg.SetValueEx(bomKey, 'InstallPath', 0, _winreg.REG_SZ, spkgPath)

if __name__ == "__main__":
    try:
        import bombardier.Config
        spkgPath = bombardier.Config.getSpkgPath()
    except:
        if sys.platform == "win32":
            spkgPath = "c:\\spkg"
        else:
            spkgPath = "/opt/spkg"
    if len(sys.argv) > 2:
        if sys.argv[2] != "install":
            spkgPath = sys.argv[2]
        elif len(sys.argv) > 3:
            spkgPath = sys.argv[3]
    print "Installing to: ",spkgPath
    install(spkgPath)
