#!/usr/bin/python
# BSD License
# Copyright (c) 2009, Peter Banka et al
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the name of the GE Security nor the names of its contributors may
#   be used to endorse or promote products derived from this software without
#   specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

import os, sys
from _version import version_info

# ================================================== Statics

VERSION       = str("%(branch_nick)s-%(revno)d" % version_info)
TEST_RELEASE  = False
LOGS_TO_KEEP  = 5
LOG_MAX_SIZE  = 1000000

HEADER_TEXT   = """Bombardier-%s, Copyright (C) 2005 Peter Banka et. al \n
Bombardier comes with ABSOLUTELY NO WARRANTY; This is free software,
and you are welcome to redistribute it under the terms of the GNU
General Public license version 2.""" % VERSION

LAST_REPORT = "lastReport.yml"
SERVER_CONFIG_FILE = "/etc/bombardier.yml"

VALID_PACKAGE_VERSIONS = [4,5]

SERVER=0
TRACEBACK=0
DEBUG=1
INFO=2
WARNING=3
ERROR=4
CRITICAL=5

LOG_LEVEL_LOOKUP = {"DEBUG": DEBUG, "INFO": INFO, "WARNING": WARNING,
                    "ERROR": ERROR, "CRITICAL": CRITICAL}
VERIFY_INTERVAL   = 3000
INSTALL_LOCK  = "lockfile"

CONSOLE_MONITOR = "consoleCheck.txt"

# AUTHORIZATION
USER = 0
ADMIN = 1

ADMIN_USER   = 9
SSH_USER     = 1
RDP_USER     = 2
SQL_USER     = 3
DEV_USER     = 4
CONSOLE_USER = 5

MAX_CHAIN_DEPTH = 50
STRUCTURE     = 1
DATA          = 2

TEST_TITLE = "TEST"
PACKAGE_DB = "packages.yml"

# PinshCmds
NO_MATCH  = "NO_MATCH"
PARTIAL  = "PARTIAL"
COMPLETE = "COMPLETE"
INCOMPLETE = "INCOMPLETE"

# BOOLEANS
YES = 1
NO = 0
NEUTRAL = 2

OK               = 0
FAIL             = 1
REBOOT           = 2
NO_BACKUP        = 3
UPDATED          = 4
REBOOT_AND_TRY_AGAIN = 5
PREBOOT          = 6
CRITICAL         = 99
UNKNOWN          = 97

DEFAULT          = '0'
INSTALL          = '1'
UNINSTALL        = '2'
VERIFY           = '3'
BACKUP           = '4'
CHECK            = '5'
AUTOMATED        = '6'
KILL             = '7'
CONFIGURE        = '8'
SET_PASS         = '9'
RECONCILE        = '10'
EXECUTE          = '11'
FIX              = '12'
PURGE            = '13'
STATUS           = '14'
DRY_RUN          = '15'
INIT             = '16'

ACTION_LOOKUP = { 'uninstall':UNINSTALL, 'configure':CONFIGURE, 
                'install':INSTALL, 'verify':VERIFY, 
                'reconcile':RECONCILE, 'status':STATUS, 
                'execute':EXECUTE, 'fix':FIX, 'purge':PURGE,
                'dry_run': DRY_RUN, "init": INIT } 

ACTION_REVERSE_LOOKUP = {}
for action in ACTION_LOOKUP:
    ACTION_REVERSE_LOOKUP[ACTION_LOOKUP[action]] = action

PASSWORD_LENGTH        = 10
BLOCK_SIZE    = 10000

INSTALL_KEY = r"HKLM\Software\GE-IT\Bombardier\InstallPath"

CENSORED = '=== CENSORED ==='

#Package Priorities
VERY_LOW      = 25
LOW           = 50
AVERAGE       = 100
HIGH          = 200
VERY_HIGH     = 300

LOG_FILE      = "bombardier.log"
CONFIG_FILE   = "config.yml"
HASH_FILE     = "configHash.yml"
PACKAGES      = "packages"
PYTHON        = "python.exe"

STATUS_FILE = "status.yml"

# ================================================= COMM SOCKET INFO

STOP = "STOP"
GO   = "GO"


NO_COLOR  = 'none'
DARK      = "dark"
LIGHT     = "light"

NORMAL_COLOR = {LIGHT: '[0;30m', DARK: '[1;37m'}
GOOD_COLOR = {LIGHT: '[0;32m', DARK: '[1;32m'}
WARNING_COLOR = {LIGHT: '[0;31m', DARK: '[1;31m'}
STRONG_COLOR = {LIGHT: '[0;36m', DARK: '[1;33m'}
WEAK_COLOR = {LIGHT: '[0;37m', DARK: '[0;37m'}

# RESULT CODES
OK      = 0
FAIL    = 1
REBOOT  = 2
PREBOOT = 6
UNKNOWN = -1

DEBUGGING  = False

PACKAGES_FILE = os.path.join("packages", "packages.yml")
GLOBAL_CONFIG_FILE = "/etc/bombardier.yml"

ACTION_DICT = {UNINSTALL: '-u', CONFIGURE:'-c', INSTALL:'-i',
               VERIFY: '-v', RECONCILE: '-r', STATUS: '-s',
               EXECUTE: '-x', FIX: '-f', PURGE: '-p',
               DRY_RUN: '-d', INIT: '-n'}

RETURN_DICT = {OK: 'OK', FAIL: 'FAIL', REBOOT: 'REBOOT', PREBOOT: 'PREBOOT'}
STRING_TO_RETURN_VALUE_MAPPING = {}
for item in RETURN_DICT:
    STRING_TO_RETURN_VALUE_MAPPING[RETURN_DICT[item]] = item

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

# Authorization levels
USER = 0
ADMIN = 1

# MATCH TYPES
NO_MATCH  = 0
PARTIAL  = 1
COMPLETE = 2

RETURN_DICT = {OK: 'OK', FAIL: 'FAIL', REBOOT: 'REBOOT', PREBOOT: 'PREBOOT'}
