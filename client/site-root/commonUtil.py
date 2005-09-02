#!/cygdrive/c/Python24/python.exe

# commonUtil.py: # This class is for convenience sake for package files

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
updateDict = miniUtility.updateDict
getAllFromFile = filesystem.getAllFromFile
catToLog = filesystem.catToLog
getProgressData = filesystem.getProgressData
stripVersion = miniUtility.stripVersion
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
server = bombardier.Server.Server(filesystem)
serviceRequest = server.serviceRequest
serviceYamlRequest = server.serviceYamlRequest
wget = server.wget
wgetMultiple = server.wgetMultiple
nagiosLog = server.nagiosLog

import bombardier.Config as Config
config = Config.Config(filesystem, server, windows)
try:
    config.freshen()
except Exceptions.ServerUnavailable, e:
    print "WARNING: Cannot update configuration from the server %s" % e
import os, sys

def getHostname():
    return os.environ["COMPUTERNAME"]

def getPythonPath():
    return sys.prefix

