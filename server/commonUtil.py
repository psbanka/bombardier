#!/usr/bin/env python

import os, sys, re, termios, tty, time, select, popen2, logging, libUi
import exceptions
from commands import getstatusoutput

import Mode

mode = Mode.Mode(Mode.USER, '>')
mode.loadConfig()

DEBUG = 0

HISTORY_FILE = "%s/.bomsh_history" % os.environ['HOME']

# RESULT CODES
OK = 0
FAIL = 1
UNKNOWN = -1

ON = 1
OFF = 0

# PinshCmds
NO_MATCH  = 0
PARTIAL  = 1
COMPLETE = 2
INCOMPLETE = 3
DEBUG = 0

# BOOLEANS
YES = 1
NO = 0
NEUTRAL = 2

# Authorization levels
USER = 0
ADMIN = 1

# MATCH TYPES
NO_MATCH  = 0
PARTIAL  = 1
COMPLETE = 2
DEBUG = 0

def convertTokensToString(tokens, delimeter=' '):
    retVal = ''
    for token in tokens:
        retVal += token+delimeter
    return retVal[:-1]

logger      = logging.getLogger('bomsh_changes')
fileHandler = logging.FileHandler("changes.log")
formatter = logging.Formatter('%(asctime)-15s|%(message)s')
fileHandler.setFormatter(formatter)
logger.addHandler(fileHandler)
logger.setLevel(logging.DEBUG)

def log(noFlag, tokens, cmdStatus, cmdOutput):
    command = ' '.join(tokens)
    if noFlag:
        command = "no %s" % command
    if type(cmdOutput) != type('string'):
        cmdOutput = str(cmdOutput)
    outputString = ':'.join(cmdOutput)
    statusDict = {OK: "OK", FAIL:"FAIL"}
    logMessage = "%-15s|STATUS:%4s|CMD:%s|OUTPUT:%s"
    logMessage = logMessage % (os.environ["USER"], command, statusDict[cmdStatus], outputString)
    logger.info(logMessage)
    return command

def makeComment(comment=''):
    if not comment:
        print "\n\nCOMMANDS:"
        libUi.userOutput(mode.commentCommands, OK)
        try:
            comment = raw_input("Enter a comment for this change:\n> ")
        except exceptions.KeyboardInterrupt, key:
            pass
        except EOFError:
            pass
    if not comment:
        comment = "NO COMMENT ENTERED"
    logComment(comment)
    mode.commentCommands = []

def logComment(comment=None):
    logger.info("%-15s|COMMENT: %s" % (os.environ["USER"], comment))

if __name__ == "__main__":
    from libTest import *
    status = OK
    startTest()
    endTest(status)
        

