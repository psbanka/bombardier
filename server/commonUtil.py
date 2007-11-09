#!/usr/bin/env python

import os, sys, re, termios, tty, time, select, popen2
from commands import getstatusoutput

import Mode

mode = Mode.Mode(Mode.ENABLE, '(BOM) #')

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

# SYSTEM TYPES
DEBIAN = 1
REDHAT = 0

RH_ERROR = "Not supported on RedHat"

# Authorization levels
USER = 0
ADMIN = 1

# MATCH TYPES
NO_MATCH  = 0
PARTIAL  = 1
COMPLETE = 2
DEBUG = 0

# Items based on MODE
if mode.systemType == DEBIAN:
    MOTD = "/usr/lib/pinsh/motd.txt"
else:
    MOTD = "./motd.txt"

def convertTokensToString(tokens, delimeter=' '):
    output = ''
    for token in tokens:
        output += token+delimeter
    return output[:-1]

if __name__ == "__main__":
    from libTest import *
    status = OK
    startTest()
    endTest(status)
        

