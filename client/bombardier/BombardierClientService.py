#!/cygdrive/c/Python24/python.exe

# BombardierClient.py: This class provides for a Windows Service which
# wakes up periodically to determine if there are packages to install
# or uninstall or if packages should be verified and reported to the
# repository. This class acts as a central dispatch location for all
# activities in a completely centralized model of system
# administration.

# Copyright (C) 2005 Peter Banka, Mark Hickman

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

import win32serviceutil, win32service, win32event, pywintypes, servicemanager
import time

import Config, CommSocket
import ReconcileThread, Server, Windows
import Filesystem, Repository, Exceptions
import BombardierClass
import Logger
from staticData import *

DEBUG = 0

class BombardierClientService(win32serviceutil.ServiceFramework):
    _svc_name_ = "BombardierClient"
    _svc_display_name_ = "Bombardier Client system"

    def __init__(self, args, repository=None, config=None, 
                 filesystem=Filesystem.Filesystem(), server=Server.Server(),
                 windows=Windows.Windows()):
      
        self.repository = repository
        self.server     = server
        self.windows    = windows
        self.filesystem = filesystem
        self.windows.ServiceFrameworkInit(self, args)
        self.server.filesystem = self.filesystem
        if config:
            self.config = config
        else:
            self.config = Config.Config(self.filesystem, self.server, self.windows)
        Logger.info("Bombardier Client Service installing.")

        self.hWaitStop = win32event.CreateEvent(None, 0, 0, None)
        self.overlapped = pywintypes.OVERLAPPED()
        self.overlapped.hEvent = win32event.CreateEvent(None,0,0,None)
        self.waitHandles = self.hWaitStop, self.overlapped.hEvent

        self.checkTime  = time.time() + STARTUP_DELAY
        self.verifyTime = 0

        self.initialized          = False
        self.commSocketToThread   = None
        self.commSocketFromThread = None
        self.reconcileThread      = None

    ## Not going to test
    def SvcStop(self):
        Logger.info("Stopping Bombardier client.")
        self.ReportServiceStatus(win32service.SERVICE_STOP_PENDING)
        win32event.SetEvent(self.hWaitStop)

    def stopBombardier(self):
        if self.reconcileThread != None:
            self.stopThreads()
            
    ## TESTED (indirectly)
    def runBombardier(self, command):
        self.windows.noAutoLogin()
        self.windows.noRestartOnLogon()
        if not self.initialized:
            errmsg = "Cannot run system checks because the repository has not been contacted"
            self.filesystem.updateCurrentStatus(ERROR, "Cannot contact the repository.")
            Logger.error(errmsg)
            return
        if self.reconcileThread != None:
            message = self.commSocketFromThread.getMessage()
            if message == STOP:
                self.reconcileThread.join()
            else:
                msg = "Cannot run a new reconcileThread because the last one did not return"
                Logger.info(msg)
                return
        try:
            self.config.freshen()
            self.repository.getPackageData()
        except Exceptions.ServerUnavailable, e:
            erstr = "Unable to download configuration "\
                    "( %s ). Aborting operations." % e
            Logger.warning(erstr)
            self.reconcileThread = None
            return
        self.commSocketToThread = CommSocket.CommSocket()
        self.commSocketFromThread = CommSocket.CommSocket()
        Logger.info("Creating reconcilethread...")
        self.reconcileThread = ReconcileThread.ReconcileThread(command,
                                                               self.commSocketToThread,
                                                               self.commSocketFromThread,
                                                               self.config, self.server,
                                                               self.windows, self.b)
        if command == AUTOMATED:
            Logger.info("The bombardier user has reached the console...")
            self.reconcileThread.config.automated = True

        if DEBUG > 1:
            self.filesystem.runReconcileThread(command)
            self.reconcileThread = None
            Logger.error("NOT RUNNING BOMBARDIER, BECAUSE WE'RE IN DEBUG MODE")
            return

        self.reconcileThread.start()

    def stopThreads(self):
        if self.reconcileThread != None: # NO TESTING COVERAGE
            Logger.info("Telling last thread to abort...")
            self.commSocketToThread.sendStop()
            message = ""
            while message != STOP:
                time.sleep(3)
                message = self.commSocketFromThread.getMessage()
                Logger.info("Waiting for last reconcileThread to quit. (%s)" % message)
            self.reconcileThread.join()
            self.filesystem.updateCurrentStatus(IDLE, "Bombardier process interrupted")
            self.reconcileThread = None

    ## TESTED (indirectly)
    def cleanup(self):
        self.windows.LogMsg(servicemanager.EVENTLOG_INFORMATION_TYPE, 
                            servicemanager.PYS_SERVICE_STOPPED,
                            (self._svc_name_, ''))
        self.stopThreads()
        Logger.info("Bombardier Client service stopped.")
        if DEBUG == 2:
            sys.exit(1)

    ## TESTED (indirectly)
    def heartbeatMessage(self):
        if DEBUG:
            msg = "next installation check: %s seconds" % (self.checkTime - time.time())
            Logger.debug(msg)
            msg = "next verification check: %s seconds" % (self.checkTime - time.time())
            Logger.debug(msg)

    # CHECK_INTERVAL should ALWAYS be greater then VERIFY_INTERVAL -mhickman
    def checkTimers(self):
        self.filesystem.updateTimestampOnly()
        if time.time() > self.checkTime:
            Logger.info("Time for install/uninstall run")
            self.checkTime = time.time() + CHECK_INTERVAL
            if self.verifyTime == 0:
                self.verifyTime = time.time() + VERIFY_INTERVAL
            return CHECK
        elif self.verifyTime and time.time() > self.verifyTime:
            if self.config.get('system','runVerify',default='NO').upper() == 'YES':
                Logger.info("Time for Verification Run")
                self.verifyTime = time.time() + VERIFY_INTERVAL
                return VERIFY
        return None

    def initialize(self):
        if self.initialized:
            return
        try:
            self.server.getServerData()
            self.config.freshen()
        except Exceptions.ServerUnavailable, e:
            erstr = "Unable to download configuration "\
                    "( %s ). Aborting operations." % e
            Logger.warning(erstr)
            return
        if self.repository == None:
            try:
                self.repository = Repository.Repository(self.config,
                                                        self.filesystem,
                                                        self.server)
                self.repository.getPackageData()
            except Exceptions.ServerUnavailable, e:
                Logger.error("Unable to connect to the repository %s" % e)
                self.repository = None
                return
        self.b = BombardierClass.Bombardier(self.repository, self.config,
                                            self.filesystem, self.server,
                                            self.windows)
        self.initialized = True
    
    ## TESTED
    def SvcDoRun(self):
        Logger.info("Bombardier Client starting...")
        self.filesystem.clearLock()
        self.windows.LogMsg(servicemanager.EVENTLOG_INFORMATION_TYPE, 
                            servicemanager.PYS_SERVICE_STARTED,
                            (self._svc_name_, ''))
        while True:
            try:
                command = self.windows.ReadPipe(BC_PIPE_NAME, PIPE_READ_TIMEOUT,
                                                self.waitHandles, self.overlapped.hEvent)
                if not command:
                    command = self.checkTimers()
            except Exceptions.ServiceShutdown:
                self.cleanup()
                break
            except Exceptions.PipeNotListenable, e:
                ermsg = "Cannot listen on pipe %s" % e
                Logger.error(ermsg)
                self.cleanup()
                break
            if command:
                Logger.debug("COMMAND: (%s)" % command)
                if command == KILL:
                    self.stopBombardier()
                else:
                    self.initialize()
                    self.runBombardier(command)
            else:
                self.heartbeatMessage()
           
if __name__=='__main__':
    win32serviceutil.HandleCommandLine(BombardierClientService)
