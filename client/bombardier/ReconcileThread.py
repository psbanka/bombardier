#!/cygdrive/c/Python24/python.exe

# ReconcileThread.py: This module is responsible for threading
# activity related to running a Bombardier session. This could
# probably be pulled out and placed in BombardierClass.py, but it
# seems ok where it is.

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

from staticData import *
import Logger, Filesystem, Exceptions

# ======================== Worker thread

class ReconcileThread(threading.Thread):
    
    def __init__(self, command, commSocketFromService, commSocketToService,
                 config, server, windows, filesystem, bombardier):
        threading.Thread.__init__(self)
        self.command    = command
        self.config     = config
        self.server     = server
        self.windows    = windows
        self.filesystem = filesystem
        self.bombardier = bombardier
        self.id      = random.randint(0,10000)
        self.commSocketToService   = commSocketToService
        self.commSocketFromService = commSocketFromService
        Logger.info("========== Starting thread ID %s..." % self.id)
        self.config.automated = False
        
    def run(self):
        status = OK
        try: 
            self.windows.CoInitialize()
            if self.command == CHECK or self.command == AUTOMATED:
                #Logger.info("In Check Thread, calling reconcileSystem")
                status = self.bombardier.reconcileSystem(self.commSocketFromService.testStop)
            elif self.command == VERIFY:
                #Logger.info("In Verify Thread, calling verifySystem")
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
                Logger.info("========== ENDING thread ID %s:OK " % (self.id))
                self.filesystem.updateCurrentStatus(IDLE, "Finished with installation activities", self.server)
            else:
                ermsg = "========== ENDING thread ID %s:FAIL " % (self.id)
                self.filesystem.updateCurrentStatus(ERROR, "Error installing", self.server)
                Logger.info(ermsg)
                self.filesystem.warningLog("Error installing", self.server)
        except Exceptions.ServerUnavailable, e:
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
            Logger.critical(ermsg)
            self.filesystem.updateCurrentStatus(ERROR, "Unhandled exception encountered", self.server)
            return
        Logger.info("stopped thread")
        self.commSocketToService.sendStop()

def runWithoutService():
    import Config, Windows, Server, CommSocket
    import Repository, BombardierClass, Exceptions

    Logger.addStdErrLogging()
    filesystem = Filesystem.Filesystem()
    filesystem.clearLock()
    server = Server.Server(filesystem)
    config = Config.Config(filesystem, server, Windows.Windows())
    try:
        config.freshen()
    except Exceptions.ServerUnavailable, e:
        print "Cannot connect to server (%s)" % e
        sys.exit(1)
    windows = Windows.Windows()
    cs1 = CommSocket.CommSocket()
    cs2 = CommSocket.CommSocket()

    repository = Repository.Repository(config, filesystem, server)
    repository.getPackageData()
    bombardier = BombardierClass.Bombardier(repository, config, filesystem, server, windows)
    reconcileThread = ReconcileThread(CHECK, cs1, cs2, config, server, windows, filesystem, bombardier)
    reconcileThread.run()
        
if __name__ == "__main__":
    runWithoutService()
