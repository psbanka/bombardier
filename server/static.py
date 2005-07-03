import ConfigParser
import sys

HEADER = '''<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"
"http://www.w3.org/TR/REC-html40/loose.dtd">
<HTML>'''

config = ConfigParser.ConfigParser()
config.read("webserver.ini")

SERVICE       = config.get("site", "address")
ROOT_DIR      = config.get("site", "rootdirectory")
TMP_DIR       = config.get("site", "tmpdirectory")
TCP_PORT      = int(config.get("site", "tcpport"))
SVN_ROOT      = config.get("svn", "root")
SVN_USERNAME  = config.get("svn", "username")
SVN_PASSWORD  = config.get("svn", "password")
try:
    SWITCH_ADDRESS    = config.get("switch", "address")
    SWITCH_LOGINPASS  = config.get("switch", "loginpass")
    SWITCH_ENABLEPASS = config.get("switch", "enablepass")
except ConfigParser.NoSectionError:
    pass
except ConfigParser.NoOptionError:
    pass

STATUS_FILE = "status.yml"
LAST_STATUS = "last.yml"
GENERAL     = "GENERAL SYSTEM"
OK          = 0
FAIL        = 1
INI         = 1
YML         = 0

