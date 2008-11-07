#!/usr/bin/env python

import os, logging, libUi
import syslog
import exceptions
from commands import getstatusoutput
from staticData import *

import Mode

HISTORY_FILE = "%s/.bomsh_history" % os.environ['HOME']
TEST_CIPHER  = "The Quick Brown Fox Jumped Over The Lazy Dog"
mode = Mode.Mode(Mode.USER, '>')
mode.loadConfig()

logger      = logging.getLogger('bomsh_changes')
logPath     = os.path.join(mode.serverHome, "changes.log")
fileHandler = logging.FileHandler(logPath)
formatter   = logging.Formatter('%(asctime)-15s|%(message)s')
fileHandler.setFormatter(formatter)
logger.addHandler(fileHandler)
logger.setLevel(logging.DEBUG)
syslog.openlog("bomsh", syslog.LOG_PID, syslog.LOG_USER)

def traced(func):
    def wrapper(*__args,**__kw):
        print "entering", func
        try:
            return func(*__args,**__kw)
        finally:
            print "exiting", func
    return wrapper

def convertTokensToString(tokens, delimeter=' '):
    retVal = ''
    for token in tokens:
        retVal += token+delimeter
    return retVal[:-1]

def log(noFlag, tokens, cmdStatus, cmdOutput):
    command = ' '.join(tokens)
    if noFlag:
        command = "no %s" % command
    if type(cmdOutput) != type('string'):
        cmdOutput = str(cmdOutput)
    cmdOutput = cmdOutput.replace('\n',':')
    statusDict = {OK: "OK", FAIL:"FAIL"}
    logMessage = "%-15s|STATUS:%4s|CMD:%s|OUTPUT:%s"
    logMessage = logMessage % (mode.username, command, statusDict[cmdStatus], cmdOutput)
    syslog.syslog(logMessage)
    logger.info(logMessage)
    return command

def makeComment(comment=''):
    if not comment:
        print "\n\nCOMMANDS:"
        libUi.userOutput(mode.commentCommands, OK)
        try:
            comment = raw_input("Enter a comment for this change:\n> ")
        except exceptions.KeyboardInterrupt:
            pass
        except EOFError:
            pass
    if not comment:
        comment = "NO COMMENT ENTERED"
    logComment(comment)
    mode.commentCommands = []

def logComment(comment=None):
    syslog.syslog("%-15s|COMMENT: %s" % (mode.username, comment))
    logger.info("%-15s|COMMENT: %s" % (mode.username, comment))

if __name__ == "__main__":
    from libTest import startTest, endTest
    status = OK
    startTest()
    endTest(status)
        
