#!/cygdrive/c/Python24/python.exe
# spkg.py: command-line installer of bombardier packages
# Copyright (C) 2005  Peter Banka

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
    print """%s
    
spkg.py [-i|-v|-u|-?] [package-name]

  -i|--install    - Install the named package.
  -v|--verify     - Verify the named package.
  -k|--uninstall  - Uninstall the named package.
  -?|-h|--help    - Prints this screen.

  [package-name]  - the name of a bombardier package file (.spkg)
""" % HEADER_TEXT

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
        status = self.package.process(abortIfTold)
        if status == REBOOT:
            print "!! You should reboot now."
        return status
    
    def uninstall(self):
        self.package.action = UNINSTALL
        status = self.package.uninstall(abortIfTold)
        if status == REBOOT:
            print "!! You should reboot now."
        return status

    def verify(self):
        self.package.action = INSTALL
        return self.package.verify(abortIfTold)

def install(spkg, packageName):
    pkgProcessor = PkgProcessor(spkg, packageName)
    if pkgProcessor.status == OK:
        status = pkgProcessor.install()
        if status == REBOOT:
            print "!! You should reboot before installing more packages"
        sys.exit(REBOOT)

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
    command = None
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
    if command == None:
        print "No command specified."
        displayHelp()
        sys.exit(1)
    if len(sys.argv) < 3:
        print "No package name specified."
        sys.exit(1)

    spkg = sys.argv[2]
    if not os.path.isfile(spkg):
        print "Package file %s does not exist." % spkg
        sys.exit(1)
    try:
        packageName, version = getName(spkg)
    except BadPackageName:
        print "Package file %s does not have an appropriate name -- is it a bombardier package?" % spkg
        sys.exit(1)
        
    commands[command](spkg, packageName)
