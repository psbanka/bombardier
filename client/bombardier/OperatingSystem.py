#!/cygdrive/c/Python24/python.exe

# OperatingSystem.py: Abstract class to wrap all operating system
# level activity. Implemented by Windows.py and Linux.py. Also enables
# mocking of these activities.

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

import Exceptions, os

class OperatingSystem:

    """Provides capabilities of operating system-level functions."""

    def __init__(self):
        self.DEBUG = 0
        self.domain = ''
        self.username = ''
        self.password = ''
        self.testConsoleValue = False

    def runProcess( self, workingDirectory, cmd ):
        raise Exceptions.AbstractClassException, "runProcess"

    def runPython( self, file, workingDirectory=os.getcwd() ):
        raise Exceptions.AbstractClassException, "runPython"

    def runCmd( self, file, workingDirectory=os.getcwd() ):
        raise Exceptions.AbstractClassException, "runCmd"

    def serviceStatus(self, serviceName):
        raise Exceptions.AbstractClassException, "serviceStatus"

    def evalStatus(self, serviceInfo):
        raise Exceptions.AbstractClassException, "evalStatus"

    def restartService(self, serviceName):
        raise Exceptions.AbstractClassException, "restartService"

    def stopService(self, serviceName):
        raise Exceptions.AbstractClassException, "stopService"

    def startService(self, serviceName):
        raise Exceptions.AbstractClassException, "startService"

    def removeScriptUser(self, username):
        raise Exceptions.AbstractClassException, "removeScriptUser"

    def createScriptUser(self, username, password):
        raise Exceptions.AbstractClassException, "createScriptUser"

    def LogMsg(self, type, event, info):
        raise Exceptions.AbstractClassException, "LogMsg"

    def ReadPipe(self, pipeName, timeout, waitHandles, overlappedHe):
        raise Exceptions.AbstractClassException, "ReadPipe"

    def sendNpMessage(self, pipe, message, logFunction, timeout=None):
        raise Exceptions.AbstractClassException, "sendNpMessage"

    def findUser(self, username):
        raise Exceptions.AbstractClassException, "findUser"

    def mkServiceUser(self, username, password, domain='', local=False, comment=''):
        raise Exceptions.AbstractClassException, "mkServiceUser"

    def rmServiceUser(self, username, domain='', local=False):
        raise Exceptions.AbstractClassException, "rmServiceUser"

    def AdjustPrivilege(self, priv, enable = 1):
        raise Exceptions.AbstractClassException, "AdjustPrivilege"

    def testCredentials(self, username, domain, password):
        raise Exceptions.AbstractClassException, "testCredentials"

    def restartOnLogon(self):
        raise Exceptions.AbstractClassException, "restartOnLogon"

    def noRestartOnLogon(self): 
        raise Exceptions.AbstractClassException, "noRestartOnLogon"

    def adjustPrivilege(self, priv, enable = 1):
        raise Exceptions.AbstractClassException, "adjustPrivilege"

    def rebootSystem(self, message="Server Rebooting", timeout=5, bForce=1, bReboot=1):

        raise Exceptions.AbstractClassException, "rebootSystem"

    def CoInitialize(self):
        raise Exceptions.AbstractClassException, "CoInitialize"

    def testConsole(self):
        raise Exceptions.AbstractClassException, "testConsole"
    
    def checkAutoLogin(self): 
        raise Exceptions.AbstractClassException, "checkAutoLogin"

    def noAutoLogin(self): 
        raise Exceptions.AbstractClassException, "noAutoLogin"

    def autoLogin(self, config):
        raise Exceptions.AbstractClassException, "autoLogin"
