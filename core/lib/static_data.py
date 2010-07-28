#!/usr/bin/env python

"Holds data that is used as constants through different parts of the code."

# Copyright (C) 2005-2010 Peter Banka, Shawn Sherwood

from _version import version_info

# ================================================== Statics

VERSION       = str("%(branch_nick)s-%(revno)d" % version_info)
LOGS_TO_KEEP  = 5
LOG_MAX_SIZE  = 1000000

HEADER_TEXT   = \
"""Bombardier-%s, Copyright (C) 2005-2010 Peter Bankaet. al \n
Bombardier comes with ABSOLUTELY NO WARRANTY; This is free software,
and you are welcome to redistribute it under the terms of the Simplified
BSD License. License terms can be found here: 
http://www.opensource.org/licenses/bsd-license.php""" % VERSION

SERVER_CONFIG_FILE = "/etc/bombardier.yml"

VALID_PACKAGE_VERSIONS = [4, 5]

# BASIC RETURN CODES
OK        = 0
FAIL      = 1
REBOOT    = 2
WAIT      = 3

RETURN_DICT = {OK: 'OK', FAIL: 'FAIL', REBOOT: 'REBOOT'}
STRING_TO_RETURN_VALUE_MAPPING = {}
for item in RETURN_DICT:
    STRING_TO_RETURN_VALUE_MAPPING[RETURN_DICT[item]] = item

# TYPES OF LOG MESSAGES FROM CLIENT TO SERVER

TRACEBACK = 0
DEBUG     = 1
INFO      = 2
WARNING   = 3
ERROR     = 4
CRITICAL  = 5

LOG_LEVEL_LOOKUP = {"DEBUG": DEBUG, "INFO": INFO, "WARNING": WARNING,
                    "ERROR": ERROR, "CRITICAL": CRITICAL}
LOG_REVERSE_LOOKUP = {}
for log_level in LOG_LEVEL_LOOKUP:
    LOG_REVERSE_LOOKUP[LOG_LEVEL_LOOKUP[log_level]] = log_level

# AUTHORIZATION LEVELS WITHIN THE CNM
USER      = 0
ADMIN     = 1

# PINSHCMD TOKEN MATCHING
NO_MATCH   = "NO_MATCH"
PARTIAL    = "PARTIAL"
COMPLETE   = "COMPLETE"
INCOMPLETE = "INCOMPLETE"

# PACKAGE ACTION COMMANDS
INSTALL          = 1
UNINSTALL        = 2
VERIFY           = 3
CONFIGURE        = 8
RECONCILE        = 10
EXECUTE          = 11
FIX              = 12
PURGE            = 13
CHECK_STATUS     = 14
DRY_RUN          = 15
INIT             = 16

ACTION_LOOKUP = {'uninstall': UNINSTALL, 'configure': CONFIGURE, 
                 'install': INSTALL, 'verify': VERIFY, 
                 'reconcile': RECONCILE, 'check_status': CHECK_STATUS, 
                 'execute': EXECUTE, 'fix': FIX, 'purge': PURGE,
                 'dry_run': DRY_RUN, "init": INIT } 

ACTION_REVERSE_LOOKUP = {}
for action in ACTION_LOOKUP:
    ACTION_REVERSE_LOOKUP[ACTION_LOOKUP[action]] = action

ACTION_DICT = {UNINSTALL: '-u', CONFIGURE:'-c', INSTALL:'-i',
               VERIFY: '-v', RECONCILE: '-r', CHECK_STATUS: '-s',
               EXECUTE: '-x', FIX: '-f', PURGE: '-p',
               DRY_RUN: '-d', INIT: '-n'}

# ENCRYPTION STUFF

CENSORED = '=== CENSORED ==='

# FILE NAMES

LOG_FILE      = "bombardier.log"
CONFIG_FILE   = "config.yml"
HASH_FILE     = "configHash.yml"
PACKAGES      = "packages"
STATUS_FILE   = "status.yml"

# TERMINAL COLORS

NO_COLOR  = 'none'
DARK      = "dark"
LIGHT     = "light"

NORMAL_COLOR = {LIGHT: '[0;30m', DARK: '[1;37m'}
GOOD_COLOR = {LIGHT: '[0;32m', DARK: '[1;32m'}
WARNING_COLOR = {LIGHT: '[0;31m', DARK: '[1;31m'}
STRONG_COLOR = {LIGHT: '[0;36m', DARK: '[1;33m'}
WEAK_COLOR = {LIGHT: '[0;37m', DARK: '[0;37m'}


