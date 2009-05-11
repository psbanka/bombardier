#!/cygdrive/c/Python24/python.exe

# staticData.py: Holds data that is used as constants.

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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

import os, sys
from _version import version_info

# ================================================== Statics

VERSION       = str("%(branch_nick)s-%(revno)d" % version_info)
TEST_RELEASE  = False

HEADER_TEXT   = """Bombardier-%s, Copyright (C) 2005 Peter Banka et. al \n
Bombardier comes with ABSOLUTELY NO WARRANTY; This is free software,
and you are welcome to redistribute it under the terms of the GNU
General Public license version 2.""" % VERSION

LAST_REPORT = "lastReport.yml"

# AUTHORIZATION
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

DEBUG            = 0

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

ACTION_DICT = { UNINSTALL:'uninstall', CONFIGURE:'configure', 
                INSTALL:'install', VERIFY:'verify', 
                RECONCILE:'reconcile', STATUS:'status', 
                EXECUTE:'execute', FIX:'fix', PURGE:'purge',
                DRY_RUN:'dry_run' } 

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

def pyChucker(*args, **kwargs):
    pass

