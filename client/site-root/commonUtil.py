#!/cygdrive/c/Python24/python.exe

# This class is for convenience sake for package files

from bombardier.miniUtility import *
from bombardier.utility import *
from bombardier.staticData import *
import bombardier.Exceptions as Exceptions

import bombardier.Filesystem
filesystem = bombardier.Filesystem.Filesystem()
getBinaryDataFromFilePath = filesystem.getBinaryDataFromFilePath
getStringDataFromFilepath = filesystem.getStringDataFromFilePath
getDataFromFilePath = filesystem.getDataFromFilePath
getDataFromFilePointer = filesystem.getDataFromFilePointer
beginConsole = filesystem.beginConsole
clearLock = filesystem.clearLock
setLock = filesystem.setLock
loadCurrent = filesystem.loadCurrent
updateCurrentStatus = filesystem.updateCurrentStatus
updateCurrentAction = filesystem.updateCurrentAction
getCurrentAction = filesystem.getCurrentAction
updateDict = filesystem.updateDict
updateProgressFile = filesystem.updateProgressFile
getAllFromFile = filesystem.getAllFromFile
catToLog = filesystem.catToLog
convertProgressData = filesystem.convertProgressData
getProgressData = filesystem.getProgressData
getPackagesFromFile = filesystem.getPackagesFromFile
stripVersion = bombardier.Filesystem.stripVersion
system = filesystem.execute
    
import bombardier.Windows
windows = bombardier.Windows.Windows()
getWindowsType = windows.getWindowsType
testCredentials = windows.testCredentials
findUser = windows.findUser
checkForKey = windows.checkForKey
makeKeys = windows.makeKeys
getDc = windows.getDc
AdjustPrivilege = windows.AdjustPrivilege
mkServiceUser = windows.mkServiceUser
rmServiceUser = windows.rmServiceUser
adjustPrivilege = windows.adjustPrivilege
rebootSystem = windows.rebootSystem
abortReboot = windows.abortReboot
#from serviceUtil:
startService = windows.startService
stopService = windows.stopService
restartService = windows.restartService
evalStatus = windows.evalStatus
getRunKey = windows.getRunKey
restartOnLogon = windows.restartOnLogon
noRestartOnLogon = windows.noRestartOnLogon
createScriptUser = windows.createScriptUser
removeScriptUser = windows.removeScriptUser

import bombardier.Logger as Logger
logger = Logger.Logger()

import bombardier.Server
server = bombardier.Server.Server(filesystem, logger)
serviceRequest = server.serviceRequest
serviceYamlRequest = server.serviceYamlRequest
serverLog = server.serverLog
wget = server.wget
wgetMultiple = server.wgetMultiple
nagiosLog = server.nagiosLog

from bombardier.Config import getIpAddress, Config
config = Config(logger, filesystem, server, windows)
try:
    config.freshen()
except Exceptions.ServerUnavailable, e:
    print "WARNING: Cannot update configuration from the server %s" % e
import os, sys

def getHostname():
    return os.environ["COMPUTERNAME"]

def getPythonPath():
    return sys.prefix

