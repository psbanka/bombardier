#!/cygdrive/c/Python23/python.exe

import win32serviceutil, win32service, win32event, pywintypes, servicemanager
import time

import logging, logging.handlers
import Config, CommSocket
import ReconcileThread, Server, Windows
import Filesystem, Repository, Exceptions
import BombardierClass, miniUtility
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
            self.config = Config.Config(self.filesystem,
                                                   self.server, self.windows)
        Logger.info("Bombardier Client Service installing.")

        self.hWaitStop = win32event.CreateEvent(None, 0, 0, None)
        self.overlapped = pywintypes.OVERLAPPED()
        self.overlapped.hEvent = win32event.CreateEvent(None,0,0,None)
        self.waitHandles = self.hWaitStop, self.overlapped.hEvent

        self.timeToCheck = 1
        self.initTime = STARTUP_DELAY

        self.commSocketToThread   = None
        self.commSocketFromThread = None
        self.reconcileThread      = None

    ## Not going to test
    def SvcStop(self):
        Logger.info("Stopping Bombardier client.")
        self.ReportServiceStatus(win32service.SERVICE_STOP_PENDING)
        win32event.SetEvent(self.hWaitStop)

    ## TESTED (indirectly)
    def runBombardier(self, command):
        if self.reconcileThread != None:
            message = self.commSocketFromThread.getMessage()
            Logger.info("===============> RECEIVED FROM THREAD: %s" % message)
            if message == STOPPED:
                self.reconcileThread.join()
            else:
                msg = "Cannot run a new reconcileThread because the last one did not return"
                Logger.info(msg)
                return
        self.commSocketToThread = CommSocket.CommSocket()
        self.commSocketFromThread = CommSocket.CommSocket()
        try:
            self.config.freshen()
        except Exceptions.ServerUnavailable, e:
            erstr = "Unable to download configuration "\
                    "( %s ). Aborting operations." % e
            Logger.warning(erstr)
            return
        self.reconcileThread = ReconcileThread.ReconcileThread(command,
                                                                          self.commSocketToThread,
                                                                          self.commSocketFromThread,
                                                                          self.config,
                                                                          self.server,
                                                                          self.windows,
                                                                          self.b)
        if command == AUTOMATED:
            Logger.info("The bombardier user has reached the console...")
            self.reconcileThread.config.automated = True

        if DEBUG > 1:
            self.filesystem.runReconcileThread(command)
            self.reconcileThread = None
            Logger.error("NOT RUNNING BOMBARDIER, BECAUSE WE'RE IN DEBUG MODE")
            return

        self.reconcileThread.start()

    ## TESTED (indirectly)
    def cleanup(self):
        self.windows.LogMsg(servicemanager.EVENTLOG_INFORMATION_TYPE, 
                            servicemanager.PYS_SERVICE_STOPPED,
                            (self._svc_name_, ''))
        Logger.info("Bombardier Client service stopped.")
        if self.reconcileThread != None: # NO TESTING COVERAGE
            Logger.info("Telling last thread to abort...")
            self.commSocketToThread.sendStop()
            message = ""
            while message != STOPPED:
                time.sleep(10)
                Logger.info("Waiting for last reconcileThread to quit.")
                message = self.commSocketFromThread.getMessage()
            self.reconcileThread.join()
        sys.exit(1)

    ## TESTED (indirectly)
    def heartbeatMessage(self):
        if DEBUG and self.timeToCheck % SLEEP_INTERVAL == 0:
            msg = "BombardierClientService will run installation "\
                  "check in %s seconds" % (self.timeToCheck)
            Logger.debug(msg)

    # CHECK_INTERVAL should ALWAYS be greater then VERIFY_INTERVAL -mhickman
    def checkTimers(self):
        if self.initTime > 0:
          self.initTime -= 1
          if self.initTime == 0:
            Logger.info("init time is up, runing check")
            self.timeToCheck = 0
            return CHECK
        if self.timeToCheck == CHECK_INTERVAL:
            Logger.info("Time for install/uninstall run")
            self.timeToCheck = 1  
            return CHECK
        elif self.timeToCheck % VERIFY_INTERVAL == 0:
            Logger.info("Time for Verification Run")
            if self.config.get('system','runVerify',default='NO').upper() == 'YES':
                return VERIFY
        return None
    
    ## TESTED
    # SvcDoRun was getting very long, so I broke it into pieces. -pbanka
    def SvcDoRun(self):
        Logger.info("Bombardier Client starting...")
        self.filesystem.clearLock()
        self.server.getServerData()
        self.windows.LogMsg(servicemanager.EVENTLOG_INFORMATION_TYPE, 
                            servicemanager.PYS_SERVICE_STARTED,
                            (self._svc_name_, ''))
        try:
            self.config.freshen()
        except Exceptions.ServerUnavailable, e:
            erstr = "Unable to download configuration "\
                    "( %s ). Aborting operations." % e
            Logger.warning(erstr)
            self.cleanup() # probably not a good thing to do.
        if self.repository == None:
            try:
                self.repository = Repository.Repository(self.config,
                                                                   self.filesystem,
                                                                   self.server)
            except Exceptions.ServerUnavailable, e:
                Logger.error("Unable to connect to the repository %s" % e)
                # FIXME: this will never get un-screwed up
                self.repository = None
        # man, this sure looks stupid:
        self.b = BombardierClass.Bombardier(self.repository,
                                                       self.config,
                                                       self.filesystem,
                                                       self.server,
                                                       self.windows)

        while 1:
            try:
                command = self.windows.ReadPipe(BC_PIPE_NAME, PIPE_READ_TIMEOUT,
                                                self.waitHandles, self.overlapped.hEvent)
            except Exceptions.ServiceShutdown:
                self.cleanup()
            except Exceptions.PipeNotListenable, e:
                ermsg = "Cannot listen on pipe %s" % e
                Logger.error(ermsg)
                self.cleanup()
            if command:
                self.runBombardier(command)
                continue
            self.heartbeatMessage()
            self.timeToCheck += 1
            command = self.checkTimers()
            if command: 
                if DEBUG:
                  Logger.info("Waking up Bombardier for "+command)
                self.runBombardier(command)
                    
if __name__=='__main__':
    win32serviceutil.HandleCommandLine(BombardierClientService)
