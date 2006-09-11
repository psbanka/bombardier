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

import threading, sys, traceback, StringIO, random, time

from bombardier.staticData import *
from bombardier.Logger import logger, addStdErrLogging
import bombardier.Filesystem, bombardier.Exceptions

try:
    import yaml
    import pycurl
except ImportError:
    print "Error: python libraries are not installed. Need yaml and pycurl"
    sys.exit(1)

# ======================== Worker thread

class ReconcileThread(threading.Thread):
    
    def __init__(self, command, commSocketFromService, commSocketToService,
                 config, server, operatingSystem, filesystem, bombardier):
        threading.Thread.__init__(self)
        self.command    = command
        self.config     = config
        self.server     = server
        self.operatingSystem    = operatingSystem
        self.filesystem = filesystem
        self.bombardier = bombardier
        self.id      = random.randint(0,10000)
        self.commSocketToService   = commSocketToService
        self.commSocketFromService = commSocketFromService
        logger.info("========== Starting thread ID %s..." % self.id)
        self.config.automated = False
        
    def run(self):
        status = OK
        try: 
            if self.command == CHECK or self.command == AUTOMATED:
                #logger.info("In Check Thread, calling reconcileSystem")
                status = self.bombardier.reconcileSystem(self.commSocketFromService.testStop)
            elif self.command == VERIFY:
                #logger.info("In Verify Thread, calling verifySystem")
                results = self.bombardier.verifySystem(self.commSocketFromService.testStop)
                status  = OK
                if type(results) != type({}):
                    status = FAIL
                    results = {"OVERALL": "Error in package system"}
                else:
                    if FAIL in results.values():
                        status = FAIL
                self.server.nagiosLog(status, results)

            if status == OK:
                logger.info("========== ENDING thread ID %s:OK " % (self.id))
                self.filesystem.updateCurrentStatus(IDLE, "Finished with installation activities", self.server)
            else:
                ermsg = "========== ENDING thread ID %s:FAIL " % (self.id)
                self.filesystem.updateCurrentStatus(ERROR, "Error installing", self.server)
                logger.info(ermsg)
                self.filesystem.warningLog("Error installing", self.server)
        except bombardier.Exceptions.ServerUnavailable, e:
            self.commSocketToService.sendStop()
            self.filesystem.clearLock()
            self.filesystem.updateCurrentStatus(ERROR, "Cannot communicate with server", self.server)
            return
        except:
            self.commSocketToService.sendStop()
            ermsg = 'Exception in thread %s: %s' % (self.id, sys.exc_type) 
            e = StringIO.StringIO()
            traceback.print_exc(file=e)
            e.seek(0)
            data = e.read()
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            logger.critical(ermsg)
            self.filesystem.updateCurrentStatus(ERROR, "Unhandled exception encountered", self.server)
            return
        logger.info("stopped thread")
        self.commSocketToService.sendStop()

def runWithoutService():
    import bombardier.Config, bombardier.OperatingSystem, bombardier.Server, bombardier.CommSocket
    import bombardier.Repository, bombardier.BombardierClass

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
    cs2 = bombardier.CommSocket.CommSocket()

    repository = bombardier.Repository.Repository(config, filesystem, server)
    repository.getPackageData()
    bombardier = bombardier.BombardierClass.Bombardier(repository, config, filesystem, server, operatingSystem)
    reconcileThread = ReconcileThread(CHECK, cs1, cs2, config, server, operatingSystem, filesystem, bombardier)
    reconcileThread.run()
        
if __name__ == "__main__":
    runWithoutService()
