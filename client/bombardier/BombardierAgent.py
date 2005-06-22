#!/cygdrive/c/Python24/python.exe

import logging, os, sys
import logging.handlers
import time
import win32serviceutil
import win32service, win32event, pywintypes, servicemanager
from staticData import *
from distutils.core import setup

import Windows, Filesystem, Exceptions

SLEEP_INTERVAL = 25
TIME_TO_CHECK  = 1000

"""
1. Run from rescue.py or started by the system on boot
2. The job of this thing is to
   a. start BombardierClient if it is not running
   b. install BombardierClient as a service if it is not installed
   b. Run rescue if BombardierClient is incorrigible
   c. listen to messages for restarting BombardierClient if necessary.

"""

## TESTED (indirectly)
def installService(logger, filesystem):
    siteLibs = os.path.join(sys.prefix, "Lib", "site-packages")
    bomPath  = os.path.join(siteLibs, "bombardier", BOMBARDIER_CLIENT_FILE)
    pPath    = os.path.join(sys.prefix, "pythonw.exe")
    cmd1     = "%s %s install > output.txt"  % (pPath, bomPath)
    cmd2     = "%s %s --startup auto update > output.txt" % (pPath, bomPath)
    cmd3     = "%s %s --interactive update > output.txt" %  (pPath, bomPath)
    logger.info("installing client(1): %s" % cmd1)
    status1 = filesystem.execute(cmd1)
    logger.info("installing client(2): %s" % cmd2)
    status2 = filesystem.execute(cmd2)
    logger.info("installing client(3): %s" % cmd3)
    status3 = filesystem.execute(cmd3)
    return status1 and status2 and status3

## TESTED
def install(logger, filesystem, windows):
    logger.info("Installing Bombardier Management Client service...")
    status = installService(logger, filesystem)
    try:
        windows.OpenService("BombardierClient")
    except:
        logger.warning("Unable to install the Bombardier client service")
        return FAIL
    logger.info("Bombardier Management Client installation successful.")
    status = windows.startService("BombardierClient")
    if status == FAIL:
        return FAIL
    logger.info("Started Bombardier Management Client service.")
    return OK

### TESTED
class BombardierAgent(win32serviceutil.ServiceFramework):
    _svc_name_ = "BombardierAgent"
    _svc_display_name_ = "Bombardier Agent system"
    def __init__(self, args, 
                 windows=Windows.Windows(),
                 filesystem=Filesystem.Filesystem(),
                 logger=None):
        self.windows    = windows
        self.filesystem = filesystem
        self.windows.ServiceFrameworkInit(self, args)
        if logger:
            self.logger = logger
        else:
            self.logger = logging.getLogger('bombardier')
            formatter   = logging.Formatter('%(asctime)s|%(levelname)s|%(message)s')
            elHandler   = logging.handlers.NTEventLogHandler("Bombardier Agent",
                                                           logtype="Application")
            elHandler.setFormatter(formatter)
            self.logger.addHandler(elHandler)
            self.logger.setLevel(logging.INFO)
        self.logger.info("Bombardier Agent installing.")
        self.hWaitStop  = win32event.CreateEvent(None, 0, 0, None)
        self.overlapped = pywintypes.OVERLAPPED()
        self.overlapped.hEvent = win32event.CreateEvent(None,0,0,None)
        self.pipeHandle = self.windows.getPipe(BA_PIPE_NAME)

    # Silly to test
    def SvcStop(self):
        self.logger.info("Stopping Bombardier Agent.")
        self.ReportServiceStatus(win32service.SERVICE_STOP_PENDING)
        win32event.SetEvent(self.hWaitStop)

    ## TESTED
    def SvcDoRun(self):
        self.logger.info("Bombardier Agent being told to start.")
        try:
            self.windows.LogMsg(
                servicemanager.EVENTLOG_INFORMATION_TYPE, 
                servicemanager.PYS_SERVICE_STARTED,
                (self._svc_name_, ''))
        except:
            self.logger.error("Event log error. Probably full.")
        self.waitHandles = self.hWaitStop, self.overlapped.hEvent
        timeToCheck = 0 # start off with a quick update
        while 1:            
            try:
                command = self.windows.ReadPipe(BA_PIPE_NAME, SLEEP_INTERVAL,
                                                self.waitHandles, self.overlapped.hEvent)
            except Exceptions.ServiceShutdown:
                break
            except Exceptions.PipeNotListenable, e:
                ermsg = "Cannot listen on pipe %s" % e
                self.logger(ermsg)
                break
            try:
                time.sleep(SLEEP_INTERVAL)
            except IOError:
                pass
            msg = "Bombardier Agent will check for updates to "\
                  "Bombardier in %s seconds" % (timeToCheck)
            self.logger.debug(msg)
            timeToCheck -= SLEEP_INTERVAL
            if timeToCheck <= 0:
                self.activities()
                timeToCheck = TIME_TO_CHECK
            if command:
                tries = 0
                while self.windows.evalStatus( "BombardierClient" ) != STOPPED:
                    self.windows.stopService(serviceName = "BombardierClient")
                    tries += 1
                    if tries > 5:
                        self.logger.warning("Could not fully stop BombardierClient")
                        break
                install(self.logger, self.filesystem, self.windows)
                self.windows.startService("BombardierClient")
        try:
            self.windows.LogMsg(
                servicemanager.EVENTLOG_INFORMATION_TYPE, 
                servicemanager.PYS_SERVICE_STOPPED,
                (self._svc_name_, ''))
        except:
            self.logger.error("Event log error. Probably full.")
        self.logger.info("Bombardier Agent stopped.")

    ## TESTED
    def activities(self):
        try:
            if self.windows.serviceStatus("BombardierClient") == FAIL:
                ermsg = "Noticed that the BombardietClient service " \
                        "was not running. Restarting it."
                self.logger.warning(ermsg)
                status1 = self.windows.startService("BombardierClient")
                if status1 == FAIL:
                    status2 = install(self.logger, self.filesystem, self.windows)
                    if status2 == OK:
                        status3 = self.windows.startService("BombardierClient")
                    if status2 == FAIL or status3 == FAIL:
                        ermsg = "Unable to start BombardierClient."
                        self.logger.critical(ermsg)
                        self.logger.critical("++++++++++NEED To run rescue.py")
        except Exceptions.ServiceNotFound:
            self.logger.info("BombardierClient not installed. Installing...")
            install(self.logger, self.filesystem, self.windows)
        self.logger.debug("...Setup finishes.")
                    
if __name__=='__main__':
    win32serviceutil.HandleCommandLine(BombardierAgent)
