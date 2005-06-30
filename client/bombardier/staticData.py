#!/cygdrive/c/Python23/python.exe

import os, sys

# ================================================== Statics

VERSION       = "0.4-pre1"
TEST_RELEASE  = False

HEADER_TEXT   = """Bombardier-%s, Copyright (C) 2005 Peter Banka et. al \n
Bombardier comes with ABSOLUTELY NO WARRANTY; This is free software,
and you are welcome to redistribute it under the terms of the GNU
General Public license version 2.""" % VERSION


STRUCTURE     = 1
DATA          = 2

TEST_TITLE = "TEST"

OK            = 0
FAIL          = 1
DEBUG         = 0
REBOOT        = 2
NO_BACKUP     = 3
UPDATED       = 4

DEFAULT       = '0'
INSTALL       = '1'
UNINSTALL     = '2'
VERIFY        = '3'
BACKUP        = '4'
CHECK         = '5'
AUTOMATED     = '6'
KILL          = '7'

BLOCK_SIZE    = 10000

INSTALL_KEY = r"HKLM\Software\GE-IT\Bombardier\InstallPath"

# loop timer constants for bombardierClientService

BC_PIPE_NAME      = r"\\.\pipe\BombardierClientService"
BA_PIPE_NAME      = r"\\.\pipe\BombardierAgent"

PIPE_READ_TIMEOUT = 100
SLEEP_INTERVAL    = 30
CHECK_INTERVAL    = 6000
VERIFY_INTERVAL   = 3000
STARTUP_DELAY     = 5000



#Package Priorities
VERY_LOW      = 25
LOW           = 50
AVERAGE       = 100
HIGH          = 200
VERY_HIGH     = 300

DEFAULT_PORT  = 8081

RUNNING       = 1
NOT_RUNNING   = 2
PENDING       = 3
STOPPED       = 4

MEMORY_LIMIT    = 400
MEMORY_MONITOR  = "memory.dat"
CONSOLE_MONITOR = "consoleCheck.txt"

PROFESSIONAL  = 200
SERVER        = 100
UNKNOWN       = 999
WIN2000       = 44
WIN2003       = 55
WINXP         = 66
AUTOIT_V2     = "AutoIt.exe"
AUTOIT_V3     = "AutoIt3.exe"
CURL          = "curl.exe"
QFECHECK      = "qfecheck.exe"
ACTION_FILE   = "action"
LOG_FILE      = "bombardier.log"
CONFIG_FILE   = "config.yml"
LOGS_TO_KEEP  = 5
LOG_MAX_SIZE  = 1000000
LOG_INTERVAL  = 10
if sys.platform == "win32":
   WINDIR    = os.environ['windir']
   MSIEXEC   = os.path.join(WINDIR,"SYSTEM32","MSIEXEC.EXE")
   ATTRIB    = os.path.join(WINDIR, "SYSTEM32", "ATTRIB")
INSTALL_LOCK  = "lockfile"
DIRLIST       = "dirlist.txt"
OLD_PROGRESS_FILE = "install-progress.txt"
PROGRESS_FILE = "install-progress.yml"
ERRORS_FILE   = "errors.log"
BOM_FILE      = "BOM.txt"
SERVERDATA_FILE = "repositoryDirectory.yml"
BOM_DEPS      = "dependency-errors.ini"
BOMBARDIER    = "BombardierUI.py" 
PACKAGES      = "packages"
PYTHON        = "python.exe"
RUN_KEY_NAME  = 'Software\\Microsoft\\Windows\\CurrentVersion\\Run'
SYSTEM_TYPE_FILE       = "systemtype.txt"
BOMBARDIER_CLIENT      = "BombardierClient.py"
MD5_CHECKFILE          = "bombardier.md5"
TEST_MD5_CHECKFILE     = "testbombardier.md5"
UPDATE_TARBALL         = "bombardier.tar.gz"
TEST_UPDATE_TARBALL    = "testbombardier.tar.gz"

SECURITY_TARBALL       = "secure.tar.gz"
SECURITY_STATUS        = "securityStatus"
SECURITY_PATH          = "C:\\secure"
SECURE_BATCH           = "secure.bat"
UNSECURE_BATCH         = "unsecure.bat"
SECURITY_CHECKFILE     = "secure.md5"
SECURITY_NONE          = 0
SECURITY_LOW           = 3
SECURITY_HIGH          = 10
SECURITY_PARANOID      = 100

SCRATCH_DIR            = "scratch"
BOMBARDIER_CLIENT_FILE = "BombardierClientService.py"

DEFAULT_USER           = "bombardier"
PASSWORD_LENGTH        = 10
DEFAULT_DOMAIN         = "."

# ================================================== SYSTEM INFORMATION

## User interface constants

GREENLIGHT  = "greenbar.png"
OFFLIGHT    = "graybar.png"
YELLOWLIGHT = "yellowbar.png"
REDLIGHT    = "redbar.png"
CURRENT_FILE = "current.yml"

IDLE       = "idle"
INSTALLING = "installing"
ERROR      = "error"
WARNING    = "warning"
OFFLINE    = "offline"
VERIFYING  = "Verifying"

UPDATE_FREQ = 5
FLASH_MOD   = 3
DEAD_TIME  = 600

# ================================================= COMM SOCKET INFO

STOPPED = "STOP"


#  Nagios Data
