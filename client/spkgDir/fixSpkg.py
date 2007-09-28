#!/cygdrive/c/Python25/python.exe
# Version 0.5-286

# fixSpkg.py: This module is essentially a hacked version of 
# ReconcileThread.py, and is meant to be run on a linux machine.
# It could use some refinement, but seems to work now.
# Copyright (C) 2007 Peter Banka

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

import sys, getopt, yaml

from bombardier.staticData import *
from bombardier.miniUtility import getProgressPath

NONE = 0
DISPLAY = 1
FIX = 2
REMOVE = 3

def fixPackageName(packageName, ok, bad):
    if packageName in ok or packageName in bad:
        return packageName
    for okPackageName in ok:
        if packageName in okPackageName:
            print "Note: using package name %s" % okPackageName
            return okPackageName
    for badPackageName in bad:
        if packageName in badPackageName:
            print "Note: using package name %s" % badPackageName
            return badPackageName

if __name__ == "__main__":
    import optparse

    parser = optparse.OptionParser("usage: %prog [option] [package-name]")
    parser.add_option("-d", "--display", dest="action",
                      action="store_const", const=DISPLAY,
                      help="display a package status")
    parser.add_option("-f", "--fix", dest="action",
                      action="store_const", const=FIX,
                      help="fix a package")
    parser.add_option("-r", "--remove", dest="action",
                      action="store_const", const=REMOVE,
                      help="remove a package")

    (options, targetPackageNames) = parser.parse_args()

    errors = False
    ok     = []
    bad    = []
    if options.action == None:
        print "Need to specify an action"
        parser.print_help()
        sys.exit(1)

    statusData = open(getProgressPath(), 'r').read()
    status = yaml.load(statusData)
    ip = status.get("install-progress") 
    if ip == None:
        print "WARNING: status file is empty. Starting from scratch"
        ip = {}
    
    for packageName in ip.keys():
        if ip[packageName]["INSTALLED"] == "BROKEN":
            bad.append(packageName)
        else:
            ok.append(packageName)


    if options.action == DISPLAY:
        if len(bad) > 0:
            print "BAD PACKAGES:"
            print '\n'.join(bad)
        else:
            print "No bad packages"
        sys.exit(0)

    if targetPackageNames == []:
        targetPackageNames = bad

    print "processing packages: %s" % targetPackageNames
    for packageName in targetPackageNames:
        packageName = fixPackageName(packageName, ok, bad)
        if packageName in bad:
            if options.action == FIX:
                print "fixing %s" % packageName
                ip[packageName] = {"INSTALLED": "Today", "VERIFIED": "Today", "UNINSTALLED": "Today"}
            else:
                print "removing %s" % packageName
                del ip[packageName]
        elif packageName in ok:
            if options.action == FIX:
                print "%s is not broken." % packageName
                errors = True
            else:
                print "removing %s" % packageName
                del ip[packageName]
        else:
            print "%s is not a package on the system." % packageName
            errors = True

    if errors == False:
        print "writing progress"
        status["install-progress"] = ip
        fh = open(getProgressPath(), 'w')
        fh.write(yaml.dump(status))
    else:
        print "not writing progress, due to errors."
