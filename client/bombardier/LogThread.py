import time
import os
import threading
import Config

import CommSocket

from staticData import *

class LogThread(threading.Thread):
    
    def __init__(self, logObject, commSocket):
        threading.Thread.__init__(self)
        self.logObject = logObject
        self.commSocket = commSocket
        
    def run(self):
        filename = os.path.join(Config.getSpkgPath(), LOG_FILE)
        file = open(filename,'r')

        #Find the size of the file and move to the end
        st_results = os.stat(filename)
        st_size = st_results[6]
        file.seek(st_size)
        while not self.commSocket.testStop():
            where = file.tell()
            line = file.readline()
            if not line:
                time.sleep(0.1)
                file.seek(where)
            else:
                data = line.strip().split('|')
                if len(data) == 3:
                    date, level, message = data
                    self.logObject.InsertStringItem(0, level)
                    self.logObject.SetStringItem(0, 1, message)
                    self.logObject.SetStringItem(0, 2, date)
                else:
                    self.logObject.InsertStringItem(0, data[0])
