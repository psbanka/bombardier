#!/cygdrive/c/Python24/python.exe

# This is the new command-line client. It will tell the bombardier
# service that either (1) there is a user who would like the system to
# be reconciled, or (2) it will signal to the BombardierClientAgent
# that the system has come online from being logged into under its own
# direction. It can now also (3) verify the system, and soon (4) abort
# a current installation

import os, sys, getopt, re
from bombardier.staticData import *
import bombardier.Package as Package
import bombardier.Filesystem as Filesystem
import bombardier.Windows as Windows
import bombardier.Server as Server
import bombardier.Config as Config
import bombardier.Logger as Logger
import bombardier.Repository as Repository

filesystem = Filesystem.Filesystem()
server     = Server.Server(filesystem)
server.getServerData()
windows    = Windows.Windows()
config     = Config.Config(filesystem, server, windows)
config.freshen()
repository = Repository.Repository(config, filesystem, server)

def displayHelp():
    print """
spkg.py [-i|-v|-u|-?] [package-name]

  -i|--install    - Install the named package.
  -v|--verify     - Verify the named package.
  -k|--uninstall  - Uninstall the named package.
  -?|-h|--help    - Prints this screen.

  [package-name]  - the name of a bombardier package file (.spkg)
"""

class BadPackageName(Exception):
    pass

def abortIfTold():
    pass

def getName(spkg):
    m = re.compile("^(.*)\-(\d+)\.spkg")
    data = m.findall(spkg)
    if len(data) != 1:
        if len(data[0]) != 2:
            raise BadPackageName
    name, version = data[0]
    return name, version

class PkgProcessor:
    def __init__(self, spkg, packageName):
        spkg = spkg
        fullName = spkg[:spkg.rfind('.spkg')]
        Logger.addStdErrLogging()
        self.package = Package.Package(packageName, repository, config,
                                       filesystem, server, windows)
        self.package.fullName = spkg[:spkg.rfind('.spkg')]
        self.package.downloaded = True
        pkgPath = os.path.join(os.getcwd(), fullName)
        repository.unpack(pkgPath, fullName, abortIfTold, removeSpkg = False)
        self.status = self.package.injector()

    def install(self):
        self.package.action = INSTALL
        return self.package.process(abortIfTold)

    def uninstall(self):
        self.package.action = UNINSTALL
        return self.package.uninstall(abortIfTold)

    def verify(self):
        self.package.action = INSTALL
        return self.package.verify(abortIfTold)

def install(spkg, packageName):
    pkgProcessor = PkgProcessor(spkg, packageName)
    if pkgProcessor.status == OK:
        sys.exit(pkgProcessor.install())

def uninstall(spkg, packageName):
    pkgProcessor = PkgProcessor(spkg, packageName)
    if pkgProcessor.status == OK:
        sys.exit(pkgProcessor.uninstall())

def verify(spkg, packageName):
    pkgProcessor = PkgProcessor(spkg, packageName)
    if pkgProcessor.status == OK:
        sys.exit(pkgProcessor.verify())

commands = {INSTALL: install, UNINSTALL: uninstall, VERIFY: verify}

if __name__ == "__main__":
    command = INSTALL
    try:
        options,args = getopt.getopt(sys.argv[1:], "i:v:u:?h",
                                     ["install", "verify", "uninstall", "help"])
    except getopt.GetoptError:
        print "ERROR: Unable to parse options."
        sys.exit(1)

    for opt,arg in options:
        if opt in ['-h','-?','--help']: 
            displayHelp()
            sys.exit(0)
        elif opt in ['-i', '--install']:
            command = INSTALL
        elif opt in ['-v', '--verify']:
            command = VERIFY
        elif opt in ['-u', '--uninstall']:
            command = UNINSTALL
        else:
            print "Unknown Option",opt

    if len(sys.argv) < 3:
        print "No package name specified."
        sys.exit(1)

    spkg = sys.argv[2]
    if not os.path.isfile(spkg):
        print "Package file %s does not exist." % spkg
        sys.exit(1)
    #try:
    if 1 == 1:
        packageName, version = getName(spkg)
    else:
    #except BadPackageName:
        print "Package file %s does not have an appropriate name -- is it a bombardier package?" % spkg
        sys.exit(1)
        
    commands[command](spkg, packageName)
