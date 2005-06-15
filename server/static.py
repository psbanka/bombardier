import ConfigParser
import sys

HEADER = '''<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"
"http://www.w3.org/TR/REC-html40/loose.dtd">
<HTML>'''


NEWS_FILE    = "news.html"

config = ConfigParser.ConfigParser()
config.read("webserver.ini")

SERVICE       = config.get("site", "address")
ROOT_DIR      = config.get("site", "rootdirectory")
TMP_DIR       = config.get("site", "tmpdirectory")
TCP_PORT      = int(config.get("site", "tcpport"))
SVN_ROOT      = config.get("svn", "root")
SVN_USERNAME  = config.get("svn", "username")
SVN_PASSWORD  = config.get("svn", "password")

SWITCH_ADDRESS = config.get("switch", "address")
SWITCH_LOGINPASS = config.get("switch", "loginpass")
SWITCH_ENABLEPASS = config.get("switch", "enablepass")

STATUS_FILE = "status.yml"
LAST_STATUS = "last.yml"
VIEWED           = 100
NOT_VIEWED       = 200
ACKNOWLEDGED     = 10
NOT_ACKNOWLEDGED = 20


PKGINFO_FILE = "pkginfo.db"

OK           = 0
FAIL         = 1
INI = 1
YML = 0

