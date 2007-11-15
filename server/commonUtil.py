#!/usr/bin/env python

import os, sys, re, termios, tty, time, select, popen2, logging
from commands import getstatusoutput

import Mode

mode = Mode.Mode(Mode.USER, '>')

DEBUG = 0

PINSH = "/usr/bin/pinsh"

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
    output = ''
    for token in tokens:
        output += token+delimeter
    return output[:-1]

logger      = logging.getLogger('bomsh_changes')
fileHandler = logging.FileHandler("changes.log")
formatter = logging.Formatter('%(asctime)-15s|%(user)-15s|%(message)s')
fileHandler.setFormatter(formatter)
logger.addHandler(fileHandler)
logger.setLevel(logging.DEBUG)

def log(tokens, status, output):
    command = ' '.join(tokens)
    outputString = ':'.join(output)
    statusDict = {OK: "OK", FAIL:"FAIL"}
    logMessage = "CMD:%s|STATUS:%s|OUTPUT:%s" % (command, statusDict[status], outputString)
    logger.info(logMessage, extra={'user':os.environ["USER"]})

def logComment(comment=None):
    if not comment:
        comment = raw_input("Enter a comment for this change:\n> ")
    logger.info("COMMENT: %s" % comment, extra={'user':os.environ["USER"]})


if __name__ == "__main__":
    from libTest import *
    status = OK
    startTest()
    endTest(status)
        

