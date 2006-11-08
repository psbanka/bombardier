#!/cygdrive/c/Python24/python.exe

# Linux.py: Class to wrap all Linux OS commands.  level activity.

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

import OperatingSystem, os
import Logger
from staticData import *

class Linux(OperatingSystem.OperatingSystem):

    """Provides capabilities of operating system-level functions for Linux machines"""

    def __init__(self):
        OperatingSystem.OperatingSystem.__init__(self)

    def run(self, fullCmd, abortIfTold, workingDirectory, console = False):
        status = OK
        abortIfTold()
        if fullCmd.split(' ')[0].endswith(".py"):
            fullCmd = "%s %s" % ("/usr/local/bin/python2.4", fullCmd)
        elif fullCmd.split(' ')[0].endswith(".sh"):
            fullCmd = "%s %s" % ("bin/bash %s" % fullCmd)
        else:
            Logger.error("unknown command type %s" % `fullCmd`)
            return FAIL
        status = self.execute(fullCmd, errorString="Unable to execute %s" % fullCmd,
                              workingDirectory = workingDirectory, captureOutput = True),
        return status
    
    def noRestartOnLogon(self): 
        pass

    def testConsole(self):
        return OK

    def noAutoLogin(self): 
        pass
