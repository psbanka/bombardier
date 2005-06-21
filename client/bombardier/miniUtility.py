import os
import _winreg as winreg
from bombardier.staticData import *

### TESTED
def addDictionaries(dict1, dict2):
    """dict1 gets stuff from dict2, only if it doesn't have it"""
    #! must be deeper
    for key,value in dict2.iteritems():
        if not dict1.has_key(key):
            dict1[key] = value
        else:
            if type(value) == type(dict1[key]):
                if type(value) == type(dict()):
                    dict1[key] = addDictionaries(dict1[key], value)
    return dict1

class Logger:
    def info(self, string):
        print "info:",string
    def debug(self, string):
        print "debug:",string
    def warning(self, string):
        print "warning:",string
    def error(self, string):
        print "error:",string
    def critical(self, string):
        print "critical:",string
    def rmFileLogging(self):
        pass

def runPythonScript(scriptPath):
    pythonCmd = os.path.join(sys.prefix, "python.exe")
    return "%s %s" % (pythonCmd, scriptPath)

def getSpkgPath():
    keyName = r"Software\GE-IT\Bombardier"
    key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                         keyName, 0, winreg.KEY_QUERY_VALUE)
    spkgPath, objtype = winreg.QueryValueEx(key, "InstallPath")
    return spkgPath

# NOT WORTH TESTING
def getBomPath():
    return os.path.join(getSpkgPath(), BOM_FILE)

def getPythonPath():
    return os.path.join(sys.prefix, "python.exe")

# NOT WORTH TESTING
def getPackagePath():
    return os.path.join(getSpkgPath(), PACKAGES)

# NOT WORTH TESTING
def getBombardierPath():
    return os.path.join(getSpkgPath(), BOMBARDIER)

# TESTED
def getProgressPath():
    newPath = os.path.join(getSpkgPath(), PROGRESS_FILE)
    return newPath

#### TESTED
def evalBoolean(data):
    if not data:
        return False
    if type(data) != type('string'):
        if type(data) == type(1):
            if data == 1:
                return True
            else:
                return False
        else:
            return False
    data = data.strip().upper()
    if data in ["TRUE", "YES", "1", "OK"]:
        return True
    return False

def connectString(server, instance, port):
    dataSource = server.strip()
    instance = instance.strip()
    port=port.strip()
    if instance:
        dataSource += "\\"+instance
    if port:
        dataSource += ","+port
    return dataSource


def getConnectString(config):
    server = config.get('sql', 'server')
    instance = config.get('sql', 'instance')
    port = config.get('sql', 'port')
    return connectString(server, instance, port)
    
def consoleSync(status):
    consoleFile = os.path.join(getSpkgPath(),CONSOLE_MONITOR)
    f = open(consoleFile, 'w')
    if type(status) == type('a'):
        f.write(status)
    elif type(status) == type(1):
        f.write(`status`)
    else: # assume a zero exit code
        f.write('0')
    f.close()

