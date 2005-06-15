#!/cygdrive/c/Python23/python.exe

import win32serviceutil, win32service, win32event, pywintypes, servicemanager
import time

import logging, logging.handlers
import Config, CommSocket, Logger, ReconcileThread
import Server, Windows, Filesystem, Repository, Exceptions
import BombardierClass
import miniUtility

from staticData import *

DEBUG = 0

class BombardierClientService(win32serviceutil.ServiceFramework):
    _svc_name_ = "BombardierClient"
    _svc_display_name_ = "Bombardier Client system"

    def mkWlog(self):
        self.wlog   = logging.getLogger('bombardier')
        formatter   = logging.Formatter('%(asctime)s|%(levelname)s|%(message)s')
        elHandler   = logging.handlers.NTEventLogHandler("Bombardier Client",
                                                         logtype="Application")
        elHandler.setFormatter(formatter)
        self.wlog.addHandler(elHandler)
        self.wlog.setLevel(logging.INFO)

    # TESTED
    def checkLogSize(self):
        # Because we're dealing with Windows, we can't use Python's
        # fancy rolling logger effectively. We therefore have to see
        # if the log is too big and deal with it periodically. -
        # pbanka
        logPath = os.path.join(miniUtility.getSpkgPath(), LOG_FILE)
        if not self.filesystem.isfile(logPath):
            return
        size = self.filesystem.stat(logPath)[6]
        if size > LOG_MAX_SIZE:
            return FAIL
        return OK

    # TESTED
    def cycleLog(self):
        spkgPath = miniUtility.getSpkgPath()
        logPath  = os.path.join(spkgPath, LOG_FILE)
        logFile  = logPath+"."+`LOGS_TO_KEEP`
        if self.filesystem.isfile(logFile):
            try:
                self.filesystem.unlink(logFile)
            except OSError:
                self.wlog.error("Permission denied removing %s" % logFile)
                return
        for i in range(LOGS_TO_KEEP-1,0, -1):
            if self.filesystem.isfile(logPath+"."+`i`):
                self.filesystem.copyfile(logPath+"."+`i`, logPath+"."+`(i+1)`)
                self.filesystem.unlink(logPath+"."+`i`)
        self.filesystem.copyfile(logPath, logPath+".1")
        try:
            self.filesystem.unlink(logPath)
        except OSError:
            self.wlog.error("Permission denied removing %s" % logPath)
            return

    def __init__(self, args, repository=None, config=None, logger=None, 
                 filesystem=Filesystem.Filesystem(), server=Server.Server(),
                 windows=Windows.Windows()):
      
        self.repository = repository
        self.server     = server
        self.windows    = windows
        self.filesystem = filesystem
        self.windows.ServiceFrameworkInit(self, args)
        self.mkWlog()
        if logger:
            self.logger = logger
            self.wlog.info("Not adding File logging")
        else:
            if self.checkLogSize() == FAIL:
                self.cycleLog()
            self.wlog.info("Adding File logging")
            self.logger = Logger.Logger()
        self.server.filesystem = self.filesystem
        self.server.logger     = self.logger
        if config:
            self.config = config
        else:
            self.config = Config.Config(self.logger, self.filesystem,
                                        self.server, self.windows)
        self.logger.info("Bombardier Client Service installing.")

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
        self.logger.info("Stopping Bombardier client.")
        self.ReportServiceStatus(win32service.SERVICE_STOP_PENDING)
        win32event.SetEvent(self.hWaitStop)

    ## TESTED (indirectly)
    def runBombardier(self, command):
        if self.reconcileThread != None:
            message = self.commSocketFromThread.getMessage()
            self.logger.info("===============> RECEIVED FROM THREAD: %s" % message)
            if message == STOPPED:
                self.reconcileThread.join()
            else:
                msg = "Cannot run a new reconcileThread because the last one did not return"
                self.logger.info(msg)
                return
        self.commSocketToThread = CommSocket.CommSocket()
        self.commSocketFromThread = CommSocket.CommSocket()
        try:
            self.config.freshen()
        except Exceptions.ServerUnavailable, e:
            erstr = "Unable to download configuration "\
                    "( %s ). Aborting operations." % e
            self.logger.warning(erstr)
            return
        self.reconcileThread = ReconcileThread.ReconcileThread(command,
                                                               self.commSocketToThread,
                                                               self.commSocketFromThread,
                                                               self.logger, self.config,
                                                               self.server, self.windows,
                                                               self.b)
        if command == AUTOMATED:
            self.logger.info("The bombardier user has reached the console...")
            self.reconcileThread.config.automated = True

        if DEBUG > 1:
            self.filesystem.runReconcileThread(command)
            self.reconcileThread = None
            self.logger.error("NOT RUNNING BOMBARDIER, BECAUSE WE'RE IN DEBUG MODE")
            return

        self.reconcileThread.start()

    ## TESTED (indirectly)
    def cleanup(self):
        self.windows.LogMsg(servicemanager.EVENTLOG_INFORMATION_TYPE, 
                            servicemanager.PYS_SERVICE_STOPPED,
                            (self._svc_name_, ''))
        self.logger.info("Bombardier Client service stopped.")
        if self.reconcileThread != None: # NO TESTING COVERAGE
            self.logger.info("Telling last thread to abort...")
            self.commSocketToThread.sendStop()
            message = ""
            while message != STOPPED:
                time.sleep(10)
                self.logger.info("Waiting for last reconcileThread to quit.")
                message = self.commSocketFromThread.getMessage()
            self.reconcileThread.join()
        sys.exit(1)

    ## TESTED (indirectly)
    def heartbeatMessage(self):
        if DEBUG and self.timeToCheck % SLEEP_INTERVAL == 0:
            msg = "BombardierClientService will run installation "\
                  "check in %s seconds" % (self.timeToCheck)
            self.logger.debug(msg)

    # CHECK_INTERVAL should ALWAYS be greater then VERIFY_INTERVAL -mhickman
    def checkTimers(self):
        if self.initTime > 0:
          self.initTime -= 1
          if self.initTime == 0:
            self.logger.info("init time is up, runing check")
            self.timeToCheck = 0
            return CHECK
        if self.timeToCheck == CHECK_INTERVAL:
            self.logger.info("Time for install/uninstall run")
            self.timeToCheck = 1  
            return CHECK
        elif self.timeToCheck % VERIFY_INTERVAL == 0:
            self.logger.info("Time for Verification Run")
            if self.config.get('system','runVerify',default='NO').upper() == 'YES':
                return VERIFY
        return None
    
    ## TESTED
    # SvcDoRun was getting very long, so I broke it into pieces. -pbanka
    def SvcDoRun(self):
        self.wlog.info("Bombardier Client starting...")
        self.logger.info("Bombardier Client starting...")
        self.filesystem.clearLock(self.logger)
        self.server.getServerData()
        self.windows.LogMsg(servicemanager.EVENTLOG_INFORMATION_TYPE, 
                            servicemanager.PYS_SERVICE_STARTED,
                            (self._svc_name_, ''))
        try:
            self.config.freshen()
        except Exceptions.ServerUnavailable, e:
            erstr = "Unable to download configuration "\
                    "( %s ). Aborting operations." % e
            self.logger.warning(erstr)
            self.cleanup() # probably not a good thing to do.
        if self.repository == None:
            try:
                self.repository = Repository.Repository(self.config, self.logger,
                                                        self.filesystem, self.server)
            except Exceptions.ServerUnavailable, e:
                self.logger.error("Unable to connect to the repository %s" % e)
                # FIXME: this will never get un-screwed up
                self.repository = None
        # man, this sure looks stupid:
        self.b = BombardierClass.Bombardier(self.repository,
                                            self.config,
                                            self.logger,
                                            self.filesystem,
                                            self.server, self.windows)

        while 1:
            try:
                command = self.windows.ReadPipe(BC_PIPE_NAME, PIPE_READ_TIMEOUT,
                                                self.waitHandles, self.overlapped.hEvent)
            except Exceptions.ServiceShutdown:
                self.cleanup()
            except Exceptions.PipeNotListenable, e:
                ermsg = "Cannot listen on pipe %s" % e
                self.wlog(ermsg)
                self.logger(ermsg)
                self.cleanup()
            if command:
                self.runBombardier(command)
                continue
            self.heartbeatMessage()
            self.timeToCheck += 1
            command = self.checkTimers()
            if command: 
                if DEBUG:
                  self.logger.info("Waking up Bombardier for "+command)
                self.runBombardier(command)
                    
if __name__=='__main__':
    win32serviceutil.HandleCommandLine(BombardierClientService)
