#!/cygdrive/c/Python24/python.exe

# bc.py: This is the generic command-line client. It will tell the
# bombardier service that either (1) there is a user who would like
# the system to be reconciled, or (2) it will signal to the
# BombardierClientAgent that the system has come online from being
# logged into under its own direction. It can now also (3) verify the
# system, and soon (4) abort a current installation. It can also work
# in stand-alone mode

# Copyright (C) 2005 Peter Banka, Shawn Sherwood, Mark Hickman

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

import time, os, sys, getopt
import bombardier, bombardier.Filesystem, bombardier.Windows
import bombardier.ReconcileThread
from bombardier.staticData import *

def displayHelp():
    print """
bc.py [-a|-v|-k|-?]
  -a|--automated    - Automatically reboot system when required.
  -v|--verify       - Verify System packages.
  -k|--kill         - Kill the current bombardier install/verify thread.
  -?|-h|--help      - Prints this screen.
"""

def tail(filename, filesystem, logFunction):
    #Set the filename and open the file
    logFunction("-- tailing %s...\n" % filename)
    fh = filesystem.open(filename,'r')

    #Find the size of the file and move to the end
    st_results = filesystem.stat(filename)
    st_size = st_results[6]
    fh.seek(st_size)

    while 1:
        where = fh.tell()
        line = fh.readline()
        if not line:
            time.sleep(0.1)
            fh.seek(where)
        else:
            logFunction(line) # already has newline

if __name__ == "__main__":
    command    = CHECK
    try:
        options,args = getopt.getopt(sys.argv[1:], "avhk?",
                                     ["automated", "verify", "kill", "help"])
    except getopt.GetoptError:
        print "ERROR: Unable to parse options."
        sys.exit(1)

    for opt,arg in options:
        if opt in ['-h','-?','--help']: 
            displayHelp()
            sys.exit(0)
        elif opt in ['-a', '--automated']:
            command = AUTOMATED
        elif opt in ['-v', '--verify']:
            command = VERIFY
        elif opt in ['-k', '--kill']:
            command = KILL
        else:
            print "Unknown Option",opt

    configPath = os.path.join(bombardier.getSpkgPath(), CONFIG_FILE)
    print HEADER_TEXT
    if os.path.isfile(configPath):
        print "=======Running in stand-alone mode."
        bombardier.ReconcileThread.runWithoutService()
        print "=======Finishing"
    else:
        print "=======Running in service mode."
        filesystem = bombardier.Filesystem.Filesystem()
        windows    = bombardier.Windows.Windows()
        status = windows.sendNpMessage(BC_PIPE_NAME, command, sys.stdout.write)
        if status == OK:
            filename = os.path.join(bombardier.getSpkgPath(), LOG_FILE)
            tail(filename, filesystem, sys.stdout.write)
