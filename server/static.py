import yaml
import sys

HEADER = '''<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"
"http://www.w3.org/TR/REC-html40/loose.dtd">
<HTML>'''

configData = open("webserver.yml", 'r').read()
config = yaml.load(configData).next()

IGNORE_FILES = ["index.yml"]

TEMPLATE      = "template.html"
FOOTER        = "footer.html"
NEVER         = 99999

## Defaults:
SERVICE       = "http://localhost:8080/website/service/"
DEPLOY_DIR    = "./deploy"
ROOT_DIR      = "."
TMP_DIR       = "./tmp"
TCP_PORT      = 8080
SVN_ROOT      = "file:repos/"
SVN_USERNAME  = "foo"
SVN_PASSWORD  = "bar"
LOG_TO_SCREEN = True
LOG_FILE      = None
ENVIRONMENT   = "production"
STATUS_PATH   = "log"
DEPLOY_PATH   = "deploy"

if config.get("site"):
    SERVICE       = config["site"]["address"]
    DEPLOY_DIR    = config["site"]["deployPath"]
    ROOT_DIR      = config["site"]["rootPath"]
    TMP_DIR       = config["site"]["tmpPath"]
    TCP_PORT      = config["site"]["tcpport"]
    ENVIRONMENT   = config["site"]["environment"]
    STATUS_PATH   = config["site"]["statusPath"]
    DEPLOY_PATH   = config["site"]["deployPath"]
if config.get("svn"):
    SVN_ROOT      = config["svn"]["root"]
    SVN_USERNAME  = config["svn"]["username"]
    SVN_PASSWORD  = config["svn"]["password"]
if config.get("log"):
    LOG_TO_SCREEN = config["log"]["toscreen"]
    if config["log"].get("filename"):
        LOG_FILE  = config["log"]["filename"]
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

