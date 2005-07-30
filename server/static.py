import ConfigParser
import sys

HEADER = '''<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"
"http://www.w3.org/TR/REC-html40/loose.dtd">
<HTML>'''

config = ConfigParser.ConfigParser()
config.read("webserver.ini")

IGNORE_FILES = ["index.yml"]

TEMPLATE      = "template.html"
FOOTER        = "footer.html"
NEVER         = 99999
SERVICE       = config.get("site", "address")
DEPLOY_DIR    = config.get("site", "deployPath")
ROOT_DIR      = config.get("site", "rootdirectory")
TMP_DIR       = config.get("site", "tmpdirectory")
TCP_PORT      = int(config.get("site", "tcpport"))
SVN_ROOT      = config.get("svn", "root")
SVN_USERNAME  = config.get("svn", "username")
SVN_PASSWORD  = config.get("svn", "password")
DEAD_TIME     = 100

SWITCH_ADDRESS    = ""
SWITCH_LOGINPASS  = ""
SWITCH_ENABLEPASS = ""
try:
    SWITCH_ADDRESS    = config.get("switch", "address")
    SWITCH_LOGINPASS  = config.get("switch", "loginpass")
    SWITCH_ENABLEPASS = config.get("switch", "enablepass")
except ConfigParser.NoSectionError:
    
    pass
except ConfigParser.NoOptionError:
    pass

PACKAGES_FILE = "packages.yml"
STATUS_FILE = "status.yml"
LAST_STATUS = "last.yml"
GENERAL     = "GENERAL SYSTEM"
OK          = 0
FAIL        = 1
INI         = 1
YML         = 0

# package types
DBPATCH  = "dbpatch"
GENERIC  = "generic"

