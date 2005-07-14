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
    noFileWarningTemplate = "Warning! %s/%s does not exist. Not copying." 
    system32Path = os.path.join( os.environ['WINDIR'], "system32" )
    for inode in os.listdir("."):
        if os.path.isfile(inode):
            sys.stdout.write( "copying %s -> %s\n" % (inode, spkgPath) )
            shutil.copy(inode, os.path.join(spkgPath, inode))
        else:
            print noFileWarningTemplate % (os.getcwd(), inode)
    for f in [ "msvcr71.dll", "mfc71.dll"]:
        if os.path.isfile( f ):
            if not os.path.isfile( os.path.join( system32Path, f ) ):
                sys.stdout.write( "copying %s -> %s\n" % ( f, system32Path ) )
                shutil.copy( f, os.path.join( system32Path, f ) )
        else:
            print noFileWarningTemplate % ( os.getcwd(), f )

    startDir = os.getcwd()
    regSvr = os.path.join(system32Path, "regsvr32.exe")
    dlls = ["AutoItX3.dll", "InstallTools.dll"]
    for dll in dlls:
        os.system("%s /s %s" % (regSvr, dll))

    sys.stdout.write("Successfully updated spkg.\n")
    os.chdir(startDir)

if __name__ == "__main__":
    try:
        import bombardier.Config
        spkgPath = bombardier.Config.getSpkgPath()
    except:
        spkgPath = "c:\\spkg"
    if len(sys.argv) > 2:
        if sys.argv[2] != "install":
            spkgPath = sys.argv[2]
        elif len(sys.argv) > 3:
            spkgPath = sys.argv[3]
    print "Installing to: ",spkgPath
    install(spkgPath)
