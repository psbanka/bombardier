#!/cygdrive/c/Python24/python.exe

# LogThread.py: This module is responsible for updating log data into
# the log panel in the Bombardier GUI.

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
import time, os, threading

import bombardier.miniUtility as miniUtility
from bombardier.staticData import *

class LogThread(threading.Thread):
    
    def __init__(self, logObject, commSocket):
        threading.Thread.__init__(self)
        self.logObject = logObject
        self.commSocket = commSocket
        
    def run(self):
        filename = os.path.join(miniUtility.getSpkgPath(), LOG_FILE)
        fh = open(filename,'r')

        #Find the size of the file and move to the end
        st_results = os.stat(filename)
        st_size = st_results[6]
        fh.seek(st_size)
        while not self.commSocket.testStop():
            where = fh.tell()
            line = fh.readline()
            if not line:
                time.sleep(0.1)
                fh.seek(where)
            else:
                data = line.strip().split('|')
                if len(data) == 3:
                    date, level, message = data
                    self.logObject.InsertStringItem(0, level)
                    self.logObject.SetStringItem(0, 1, message)
                    self.logObject.SetStringItem(0, 2, date)
                else:
                    self.logObject.InsertStringItem(0, data[0])
