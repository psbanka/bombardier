#!/cygdrive/c/Python24/python.exe

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
    for inode in os.listdir("."):
        if os.path.isfile(inode):
            sys.stdout.write( "copying %s -> %s\n" % (inode, spkgPath) )
            shutil.copy(inode, os.path.join(spkgPath, inode))
        else:
            print "Warning! %s/%s does not exist. Not copying." % (os.getcwd(), inode)

    startDir = os.getcwd()
    regSvr = os.path.join(os.environ["WINDIR"], "system32", "regsvr32.exe")
    dlls = ["AutoItX3.dll", "InstallTools.dll", "mfc71.dll"]
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
