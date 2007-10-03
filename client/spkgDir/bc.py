#!/usr/local/bin/python2.4

# bc2.py: This module is essentially a hacked version of 
# ReconcileThread.py, and is meant to be run on a linux machine.
# It could use some refinement, but seems to work now.
# Copyright (C) 2005 Peter Banka

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

import sys, optparse, StringIO, traceback, yaml

from bombardier.staticData import *
from bombardier.Logger import logger, addStdErrLogging
import bombardier.Filesystem, bombardier.Exceptions
import bombardier.Config, bombardier.OperatingSystem
import bombardier.Server, bombardier.CommSocket
import bombardier.Repository, bombardier.BombardierClass

UNINSTALL = 0
CONFIGURE = 1
INSTALL   = 2
VERIFY    = 3
RECONCILE = 4
STATUS    = 5

if __name__ == "__main__":
    import optparse
    addStdErrLogging()

    packageName = ""    
    
    parser = optparse.OptionParser("usage: %prog [option] [package-name]")

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

    (options, args) = parser.parse_args()
    if not (options.action == RECONCILE or options.action == STATUS):
        if len(args) != 1:
            print "This command requires a package name as an argument."
            parser.print_help()
            sys.exit( 1 )
        packageName = args[0]
         
    filesystem = bombardier.Filesystem.Filesystem()
    filesystem.clearLock()
    server = bombardier.Server.Server(filesystem)
    config = bombardier.Config.Config(filesystem, server, bombardier.OperatingSystem.OperatingSystem())
    try:
        config.freshen()
    except bombardier.Exceptions.ServerUnavailable, e:
        logger.error( "Cannot connect to server (%s)" % e )
        sys.exit(1)
    if sys.platform == "linux2":
        import bombardier.Linux
        operatingSystem = bombardier.Linux.Linux()
    else:
        import bombardier.Windows
        operatingSystem = bombardier.Windows.Windows()
    cs1 = bombardier.CommSocket.CommSocket()

    repository = bombardier.Repository.Repository(config, filesystem, server)
    repository.getPackageData()
    bc = bombardier.BombardierClass.Bombardier(repository, config,
                                                       filesystem, server,
                                                       operatingSystem)
    status = FAIL
    try:
        if options.action == STATUS:
            statusDict = bc.checkSystem(lambda:False)
            if type(statusDict) == type({}):
                if statusDict["ok"]:
                    logger.info("OK PACKAGES:")
                    for packageName in statusDict["ok"]:
                        logger.info("- %s" % packageName)
                if statusDict["broken"]:
                    logger.info("BROKEN PACKAGES:")
                    for packageName in statusDict["broken"]:
                        logger.info("- %s" % packageName)
                status = OK
            else:
                status = FAIL
        elif options.action == RECONCILE:
            status = bc.reconcileSystem(cs1.testStop)
            if status == OK:
                filesystem.updateCurrentStatus(IDLE, "Finished with installation activities", server)
            else:
                filesystem.updateCurrentStatus(ERROR, "Error installing", server)
                filesystem.warningLog("Error installing", server)
        elif options.action == INSTALL:
            status = bc.installPackage(packageName)
        elif options.action == CONFIGURE:
            status = bc.configurePackage(packageName)
        elif options.action == VERIFY:
            status = bc.verifyPackage(packageName)
        elif options.action == UNINSTALL:
            status = bc.uninstallPackage(packageName)

        if status == OK:
            logger.info( "COMPLETED SUCCESSFULLY" )
        else:
            logger.error( "======================\n\n%s\n" % yaml.dump(status))
        filesystem.clearLock()
    except:
        e = StringIO.StringIO()
        traceback.print_exc(file=e)
        e.seek(0)
        data = e.read()
        ermsg = ''
        for line in data.split('\n'):
            ermsg += "\n||>>>%s" % line
        logger.error(ermsg)
    sys.exit(status)
