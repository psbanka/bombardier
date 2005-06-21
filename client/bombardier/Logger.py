#!/cygdrive/c/Python23/python.exe

import logging, sys, os
import logging.handlers
from bombardier.staticData import *

def getSpkgPath():
    import _winreg as winreg
    keyName = r"Software\GE-IT\Bombardier"
    key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                         keyName, 0, winreg.KEY_QUERY_VALUE)
    spkgPath, dummy = winreg.QueryValueEx(key, "InstallPath")
    return spkgPath

class Logger:

    """This class implements a simple interface that other logging
    type classes also implement. This is the most complete
    implementation. """

    def __init__(self):
        self.logger      = logging.getLogger('bombardier')
        spkgPath         = getSpkgPath()
        filename         = os.path.join(spkgPath, LOG_FILE)
        #self.fileHandler = logging.handlers.RotatingFileHandler(filename, 'a', 100000, 5)
        try:
            self.fileHandler = logging.FileHandler(filename)
        except IOError, e:
            ermsg = "Unable to open file %s for logging %s\n" % (filename , e)
            open("c:\\spkg\\logerror.txt", 'a').write( ermsg)
            self.fileHandler = logging.StreamHandler(sys.stderr )
        self.formatter   = logging.Formatter('%(asctime)s|%(levelname)s|%(message)s')
        self.fileHandler.setFormatter(self.formatter)
        self.logger.addHandler(self.fileHandler)
        self.logger.setLevel(logging.DEBUG)
        self.stdErrHandler = None

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

