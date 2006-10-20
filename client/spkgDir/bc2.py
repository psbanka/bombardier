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

import sys, getopt

from bombardier.staticData import *
from bombardier.Logger import logger, addStdErrLogging
import bombardier.Filesystem, bombardier.Exceptions
import bombardier.Config, bombardier.OperatingSystem
import bombardier.Server, bombardier.CommSocket
import bombardier.Repository, bombardier.BombardierClass


def displayHelp():
    usage = """
%s: the bombardier package installation and check utility

USAGE:
%s [command] [-h|-?|--help]

    [command]   Provide a command to the bc system, which can be one of:
      check     check and report on the overall status of the system packages
      update    (default) download, install, and verify all packages required
                for this system.

    -h          this screen
    
    """ % (sys.argv[0], sys.argv[0])
    print usage
    sys.exit(1)


if __name__ == "__main__":
    try:
        options,args = getopt.getopt(sys.argv[1:], "ch?",
                                     ["check", "help"])
    except getopt.GetoptError:
        print "ERROR: Unable to parse options."
        displayHelp()

    check = False
    update = False
    for opt,arg in options:
        if opt in ['-h','-?','--help']: 
            displayHelp()
        elif opt in ['check']:
            if update:
                print "Check and update are mutually exclusive options"
                displayHelp()
            check = True
        elif opt in ['update']:
            if check:
                print "Check and update are mutually exclusive options"
                displayHelp()
            update = True
        else:
            print "Unknown Option",opt
            displayHelp()

    if not check:
        update = True # default

    addStdErrLogging()
    filesystem = bombardier.Filesystem.Filesystem()
    filesystem.clearLock()
    server = bombardier.Server.Server(filesystem)
    config = bombardier.Config.Config(filesystem, server, bombardier.OperatingSystem.OperatingSystem())
    try:
        config.freshen()
    except bombardier.Exceptions.ServerUnavailable, e:
        print "Cannot connect to server (%s)" % e
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
    if update:
        status = bc.reconcileSystem(cs1.testStop)
        if status == OK:
            filesystem.updateCurrentStatus(IDLE, "Finished with installation activities", server)
        else:
            filesystem.updateCurrentStatus(ERROR, "Error installing", server)
            filesystem.warningLog("Error installing", server)
    elif check:
        status = bc.checkSystem(cs1.testStop)
        filesystem.updateCurrentStatus(IDLE, "Check complete", server)
    filesystem.clearLock()

