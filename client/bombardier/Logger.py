#!/cygdrive/c/Python24/python.exe

# Logger.py: This is a simple class for wrapping functions in the
# native python logging module.

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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

import logging, sys, os, shutil, yaml
import logging.handlers
from staticData import *

def getLinuxConfig():
    data = open("/etc/bombardier.yml", 'r').read()
    config = yaml.load(data)
    return config

def putLinuxConfig(config):
    data = open("/etc/bombardier.yml", 'w')
    data.write(yaml.dump(config))

def getSpkgPath():
    spkgPath = ''
    if sys.platform == "linux2":
        config = getLinuxConfig()
        if type(config) != type({}):
            config = config.next()
        spkgPath = config.get("spkgPath")
    else:
        import _winreg as winreg
        keyName = r"Software\GE-IT\Bombardier"
        try:
            key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                                 keyName, 0, winreg.KEY_QUERY_VALUE)
            spkgPath, dummy = winreg.QueryValueEx(key, "InstallPath")
        except:
            spkgPath = r"C:\spkg"
    return spkgPath

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
    try:
        fileHandler = logging.FileHandler(filename)
    except IOError, e:
        fileHandler = logging.StreamHandler(sys.stderr)
formatter = logging.Formatter('%(asctime)s|%(levelname)s|%(message)s')
fileHandler.setFormatter(formatter)
logger.addHandler(fileHandler)
logger.setLevel(logging.DEBUG)

def addStdErrLogging():
    stdErrHandler = logging.StreamHandler(sys.stderr)
    stdErrHandler.setFormatter(formatter)
    logger.addHandler(stdErrHandler)

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
    REPS = 10
    start = time.time()
    logger = Logger()
    testString = "now is the time for all good men to come to the aid of their country"
    for i in range(1,REPS):
        logger.info(testString)
    print "elapsed 1:", time.time()-start
    start = time.time()
    for i in range(1,REPS):
        Logger().info(testString)
    print "elapsed 1:", time.time()-start
    
