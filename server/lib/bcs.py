#!/usr/bin/env python

import sys, optparse, getpass, os
import Client
from bombardier.staticData import OK, FAIL, REBOOT, PREBOOT
from BombardierRemoteClient import *

if __name__ == "__main__":
    parser = optparse.OptionParser("usage: %prog server-name [options] [package-names]")
    parser.add_option("-s", "--status", dest="action",
                      action="store_const", const=STATUS,
                      help="display the status of the system")
    parser.add_option("-c", "--configure", dest="action",
                      action="store_const", const=CONFIGURE,
                      help="configure a package")
    parser.add_option("-v", "--verify", dest="action",
                      action="store_const", const=VERIFY,
                      help="verify a package")
    parser.add_option("-i", "--install", dest="action",
                      action="store_const", const=INSTALL,
                      help="install a package")
    parser.add_option("-r", "--reconcile", dest="action",
                      action="store_const", const=RECONCILE,
                      help="reconcile the system")
    parser.add_option("-u", "--uninstall", dest="action",
                      action="store_const", const=UNINSTALL,
                      help="uninstall a package")
    parser.add_option("-x", "--execute", dest="action",
                      action="store_const", const=EXECUTE,
                      help="execute a maintenance script.")
    parser.add_option("-f", "--fix", dest="action",
                      action="store_const", const=FIX,
                      help="set a package status to INSTALLED without doing anything")
    parser.add_option("-p", "--purge", dest="action",
                      action="store_const", const=PURGE,
                      help="Remove a package from the client's status")
    parser.add_option("-k", "--insecure", dest="insecure",
                      action="store_true", default=False,
                      help="Don't decrypt and send any sensitive data")

    (options, args) = parser.parse_args()

    scriptName = ''
    if len(args) == 0:
        print "==> Need to provide a system name"
        parser.print_help()
        sys.exit(1)

    if options.action == EXECUTE:
        if len(args) < 3:
            print "==> Need to provide a package name and a script name with this option."
            parser.print_help()
            sys.exit(1)
        packageNames = [args[1]]
        scriptName   = args[2]

    elif options.action not in [STATUS, RECONCILE]:
        print "ARGS:", args
        if len(args) < 2:
            print "==> Need to provide one or more package names with this option."
            parser.print_help()
            sys.exit(1)
        packageNames = args[1:]
    else:
        packageNames = []

    serverNames = [s for s in args[0].split(' ') if len(s) ]

    encryptedItems = 0
    for serverName in serverNames:
        client = Client.Client(serverName, '', os.getcwd())
        status = client.get()
        encryptedItems += client.checkEncryption()

    if encryptedItems:
        if options.insecure:
            configPasswd = ''
        else:
            print "==> There are %d encrypted configuration items." % encryptedItems
            configPasswd = getpass.getpass("Enter client configuration password: ")
        if configPasswd == '':
            print "==> Redacting %d sensitive configuration items." % encryptedItems
    else:
        print "==> There are no encrypted configuration items..."
        configPasswd = ''

    status = OK
    for serverName in serverNames:
        r = BombardierRemoteClient(serverName, configPasswd, os.getcwd(), 80)
        machineStatus, output = r.process(options.action, packageNames, scriptName, True)
        print output
        r.disconnect()
        if status == OK:
            status = machineStatus
    sys.exit(status)
