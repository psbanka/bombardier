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

import Exceptions, os, Logger
from staticData import *

class OperatingSystem:

    """Provides capabilities of operating system-level functions."""

    def __init__(self):
        self.DEBUG = 0
        self.domain = ''
        self.username = ''
        self.password = ''
        self.testConsoleValue = False

    def execute(self, cmd, errorString="", debug=0, dieOnExit=False,
                workingDirectory = '.', captureOutput=False):
        capture = ''
        if captureOutput:
            capture = " 2> result1.txt > result2.txt"
        curDir = os.getcwd()
        if workingDirectory != ".":
            os.chdir(workingDirectory)
        if debug: 
            Logger.debug("EXECUTING: %s" % cmd)
        status = os.system(cmd+capture)
        if captureOutput:
            self.catToLog("result1.txt")
            self.catToLog("result2.txt")
        os.chdir(curDir)
        if status != OK:
            ermsg = "Nonzero exit code %s (executing command %s)." % (errorString, cmd)
            Logger.error(ermsg)
            if dieOnExit == 1:
                sys.exit(1)
        return status
    
    def catToLog(self, file):
        if not os.path.isfile(file):
            Logger.warning("--------------------------------")
            Logger.warning("Output file was not created.")
            Logger.warning("--------------------------------")
            return
        lines = open(file, 'r').readlines()
        if len(lines) == 0:
            return
        Logger.info("---------------------------------------")
        for line in lines:
            Logger.info("output:"+line.strip())
        Logger.info("---------------------------------------")

