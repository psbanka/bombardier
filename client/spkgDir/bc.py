#!/cygdrive/c/Python24/python.exe

# This is the new command-line client. It will tell the bombardier
# service that either (1) there is a user who would like the system to
# be reconciled, or (2) it will signal to the BombardierClientAgent
# that the system has come online from being logged into under its own
# direction. It can now also (3) verify the system, and soon (4) abort
# a current installation

import win32pipe, pywintypes, time, os, sys, getopt
import bombardier, bombardier.Filesystem, bombardier.Windows
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
    file = filesystem.open(filename,'r')

    #Find the size of the file and move to the end
    st_results = filesystem.stat(filename)
    st_size = st_results[6]
    file.seek(st_size)

    while 1:
        where = file.tell()
        line = file.readline()
        if not line:
            time.sleep(0.1)
            file.seek(where)
        else:
            logFunction(line) # already has newline

if __name__ == "__main__":
    filesystem = bombardier.Filesystem.Filesystem()
    windows    = bombardier.Windows.Windows()
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

    status = windows.sendNpMessage(BC_PIPE_NAME, command, sys.stdout.write)

    if status == OK:
        filename = os.path.join(bombardier.getSpkgPath(), LOG_FILE)
        tail(filename, filesystem, sys.stdout.write)
