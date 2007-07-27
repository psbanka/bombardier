#!/cygdrive/c/Python25/python.exe

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

def displayHelp(errMsg):
    usage = """
%s: the bombardier package status fixer

USAGE:
%s [-d|-f|-r] [packageName]

 -d           display package problems
 -f           set broken packages to "fixed"
 -r           remove broken packages
 packageName  optional. specify which package to work on (otherwise, all are chosen)
 -h           this screen

    """ % (sys.argv[0], sys.argv[0])
    print usage
    print errMsg
    sys.exit(1)


if __name__ == "__main__":
    try:
        options,args = getopt.getopt(sys.argv[1:], "dfrh",
                                     ["display", "fix", "remove", "help"])
        targetPackageNames = args
    except getopt.GetoptError:
        displayHelp("ERROR: Unable to parse options.")

    errors = False
    action = NONE
    ok     = []
    bad    = []
    for opt,arg in options:
        if opt in ['-h','-?','--help']:
            displayHelp()
        elif opt in ['-d', '--display']:
            action = DISPLAY
        elif opt in ['-f', '--fix']:
            action = FIX
        elif opt in ['-r', '--remove']:
            action = REMOVE
        else:
            displayHelp("Unknown Option %s"%opt)
    if action == NONE:
        displayHelp("Need to specify an action")

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


    if action == DISPLAY:
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
        if packageName in bad:
            if action == FIX:
                print "fixing %s" % packageName
                ip[packageName] = {"INSTALLED": "Today", "VERIFIED": "Today", "UNINSTALLED": "Today"}
            else:
                print "removing %s" % packageName
                del ip[packageName]
        elif packageName in ok:
            if action == FIX:
                print "%s is not broken." % packageName
                errors = True
            else:
                print "removing %s" % packageName
                del ip[packageName]
        else:
            print "%s is not a package on the system."
            errors = True

    if errors == False:
        print "writing progress"
        status["install-progress"] = ip
        fh = open(getProgressPath(), 'w')
        fh.write(yaml.dump(status))
    else:
        print "not writing progress, due to errors."
