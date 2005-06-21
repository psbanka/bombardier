#!/cygdrive/c/Python24/python.exe

import logging, sys, os, shutil
import logging.handlers
from staticData import *

def getSpkgPath():
    import _winreg as winreg
    keyName = r"Software\GE-IT\Bombardier"
    key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                         keyName, 0, winreg.KEY_QUERY_VALUE)
    path, dummy = winreg.QueryValueEx(key, "InstallPath")
    return path

def checkLogSize():
    # Because we're dealing with Windows, we can't use Python's
    # fancy rolling logger effectively. We therefore have to see
    # if the log is too big and deal with it periodically. -
    # pbanka
    logPath = os.path.join(getSpkgPath(), LOG_FILE)
    if not os.path.isfile(logPath):
        return
    size = os.stat(logPath)[6]
    if size > LOG_MAX_SIZE:
        return FAIL
    return OK

def cycleLog():
    path = getSpkgPath()
    logPath  = os.path.join(path, LOG_FILE)
    logFile  = logPath+"."+`LOGS_TO_KEEP`
    if os.path.isfile(logFile):
        try:
            os.unlink(logFile)
        except OSError:
            return
    for i in range(LOGS_TO_KEEP-1,0, -1):
        if os.path.isfile(logPath+"."+`i`):
            shutil.copyfile(logPath+"."+`i`, logPath+"."+`(i+1)`)
            os.unlink(logPath+"."+`i`)
    shutil.copyfile(logPath, logPath+".1")
    try:
        os.unlink(logPath)
    except OSError:
        return

if checkLogSize() == FAIL:
    cycleLog()

logger      = logging.getLogger('bombardier')
spkgPath    = getSpkgPath()
filename    = os.path.join(spkgPath, LOG_FILE)
try:
    fileHandler = logging.FileHandler(filename)
except IOError, e:
    ermsg = "Unable to open file %s for logging %s\n" % (filename , e)
    open("c:\\spkg\\logerror.txt", 'a').write( ermsg)
    fileHandler = logging.StreamHandler(sys.stderr )
formatter   = logging.Formatter('%(asctime)s|%(levelname)s|%(message)s')
fileHandler.setFormatter(formatter)
logger.addHandler(fileHandler)
logger.setLevel(logging.DEBUG)

def info(s):
    try:
        logger.info(s)
    except OSError:
        print "ERROR IN LOGGING SYSTEM"

def debug(s):
    try:
        logger.debug(s)
    except OSError:
        print "ERROR IN LOGGING SYSTEM"

def warning(s):
    try:
        logger.warning(s)
    except OSError:
        print "ERROR IN LOGGING SYSTEM"

def error(s):
    try:
        logger.error(s)
    except OSError:
        print "ERROR IN LOGGING SYSTEM"

def critical(s):
    try:
        logger.critical(s)
    except OSError:
        print "ERROR IN LOGGING SYSTEM"


# Legacy class: to be demolished. -pbanka

class Logger:

    """This class implements a simple interface that other logging
    type classes also implement. This is the most complete
    implementation. """

    def __init__(self):
        self.logger        = logger
        self.formatter     = formatter
        self.stdErrHandler = None
        self.fileHandler   = fileHandler

    def info(self, s):
        try:
            self.logger.info(s)
        except OSError:
            print "ERROR IN LOGGING SYSTEM"

    def debug(self, s):
        try:
            self.logger.debug(s)
        except OSError:
            print "ERROR IN LOGGING SYSTEM"

    def warning(self, s):
        try:
            self.logger.warning(s)
        except OSError:
            print "ERROR IN LOGGING SYSTEM"

    def error(self, s):
        try:
            self.logger.error(s)
        except OSError:
            print "ERROR IN LOGGING SYSTEM"

    def critical(self, s):
        try:
            self.logger.critical(s)
        except OSError:
            print "ERROR IN LOGGING SYSTEM"

    def getLastLogLine(self):
        data = open(os.path.join(getSpkgPath(), LOG_FILE), 'r').readlines()
        return data[-1]

    def addStdErrLogging(self):
        self.stdErrHandler = logging.StreamHandler(sys.stderr)
        self.stdErrHandler.setFormatter(self.formatter)
        self.logger.addHandler(self.stdErrHandler)
        self.logger.info("Adding logging to standard error")

    def rmFileLogging(self):
        if self.fileHandler:
            self.logger.info("Removing logging to %s" % LOG_FILE)
            self.logger.removeHandler(self.fileHandler)
        else:
            self.logger.warning("Being told to remove nonexistent file logging handler.")

    def rmStdErrLogging(self):
        if self.stdErrHandler:
            self.logger.info("Removing logging to standard error")
            self.logger.removeHandler(self.stdErrHandler)
        else:
            self.logger.warning("Being told to remove nonexistent stderr handler.")

if __name__ == "__main__":
    import time
    start = time.time()
    logger = Logger()
    testString = "now is the time for all good men to come to the aid of their country"
    for i in range(1,10000):
        logger.info(testString)
    print "elapsed 1:", time.time()-start
    start = time.time()
    for i in range(1,10000):
        Logger().info(testString)
    print "elapsed 1:", time.time()-start
    
