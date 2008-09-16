import os
from bombardier.staticData import OK, FAIL, REBOOT, PREBOOT

# RESULT CODES
OK = 0
FAIL = 1
UNKNOWN = -1
DEBUG     = False

PACKAGES_FILE = os.path.join("packages", "packages.yml")
GLOBAL_CONFIG_FILE = "/etc/bombardier.yml"

UNINSTALL = 0
CONFIGURE = 1
INSTALL   = 2
VERIFY    = 3
RECONCILE = 4
STATUS    = 5
EXECUTE   = 6
FIX       = 7
PURGE     = 8
DRY_RUN   = 9

ACTION_DICT = {UNINSTALL: '-u', CONFIGURE:'-c', INSTALL:'-i', 
               VERIFY: '-v', RECONCILE: '-r', STATUS: '-s', 
               EXECUTE: '-x', FIX: '-f', PURGE: '-p',
               DRY_RUN: '-d'}

RETURN_DICT = {OK: 'OK', FAIL: 'FAIL', REBOOT: 'REBOOT', PREBOOT: 'PREBOOT'}

DEBUG_OUTPUT_TEMPLATE = '\n==> ' + ''.join('='*50) + "\n==> %s\n==> " + ''.join('='*50) + '\n'

COMMAND_LOG_MARKER = '^^^'

TB_CTRL_PORT = 7000
TB_RUN_JOB = "R"
TB_KILL    = "K"
TB_WAIT    = "W"
TB_ADD     = "A"
TB_DEL     = "D"
TB_SHOW    = "S"
TB_SAVE    = "V"
TB_LOAD    = "L"
TB_ENABLE  = "E"
TB_DISABLE = "X"

YAML_CHARS = [ chr(x) for x in range(ord(' '), ord('~')+1) ] + [ '\n' ]

ON = 1
OFF = 0

# PinshCmds
NO_MATCH  = 0
PARTIAL  = 1
COMPLETE = 2
INCOMPLETE = 3

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

def pyChucker(*args, **kwargs):
    pass

class ConnectionRefusedException(Exception):
    def __init__(self, explanation):
        e = Exception()
        Exception.__init__(e)
        self.explanation = explanation
    def __repr__(self):
        return self.explanation
    def __str__(self):
        return self.explanation
