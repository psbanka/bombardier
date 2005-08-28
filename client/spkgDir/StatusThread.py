#!/cygdrive/c/Python24/python.exe

# StatusThread.py: This module maintains the text on the primay
# "general status" panel of the Bombardier GUI.

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
import time
import threading

from bombardier.staticData import *
import bombardier.miniUtility as miniUtility

class StatusThread(threading.Thread):
    
    def __init__(self, overall, application, main, sub,
                 action, setLightColor, iconControl,
                 todo, commSocket, filesystem, debug = False):
        threading.Thread.__init__(self)
        self.application   = application
        self.overall       = overall
        self.main          = main
        self.sub           = sub
        self.action        = action
        self.setLightColor = setLightColor
        self.todo          = todo
        self.iconControl   = iconControl
        self.commSocket    = commSocket
        self.filesystem    = filesystem
        self.debug         = debug
        self.stalled       = False
        self.badData       = False
        self.updateMult    = 0.01
        self.screenData    = {"application":0,
                              "overall": 0,
                              "main": "Initializing",
                              "sub": "",
                              "action":"",
                              "lightColor":OFFLIGHT,
                              "todo":[],
                              "icon": OK}

    def updateTodo(self, todoList):
        self.todo.DeleteAllItems()
        self.todoList = todoList
        index = 0
        for todoItem in todoList:
            data = todoItem.strip().split(',')
            self.todo.InsertStringItem(index, data[0])
            if len(data) > 1:
                self.todo.SetStringItem(index, 1, data[1])
            index += 1

    def paintScreen(self):
        if self.changed:
            self.application(self.screenData["application"])
            self.overall(self.screenData["overall"])
            self.main(self.screenData["main"])
            self.sub(self.screenData["sub"])
            self.action(self.screenData["action"])
            self.updateTodo(self.screenData["todo"])
            lightColor = self.screenData["lightColor"]
            self.setLightColor(self.screenData["lightColor"])
            if lightColor == REDLIGHT:
                self.iconControl(FAIL)
            else:
                self.iconControl(OK)
            self.updateMult = 0.1
        else:
            self.updateMult += 0.1
        if self.updateMult > 2:
            self.updateMult = 2

    def ensureStructure(self):
        base = {"status":{}, "timestamp":0, "todo":[]}
        status = {"action":"", "overall":0, "main": "", "package":"", "percentage":0}
        for key, thing in base.iteritems():
            if not self.status.has_key(key):
                self.status[key] = thing
        for key, thing in status.iteritems():
            if not self.status["status"].has_key(key):
                self.status["status"][key] = thing

    def updateData(self):
        try:
            statusPath = os.path.join(miniUtility.getSpkgPath(),STATUS_FILE)
            self.status = self.filesystem.loadYaml(statusPath)
            self.badData = False
        except:
            self.status = {}
            self.set({"main": "Unable to determine status", "lightColor": REDLIGHT})
            self.badData = True
        
    def set(self, updateDict):
        for key, value in updateDict.iteritems():
            if self.screenData[key] != value:
                self.screenData[key] = value
                self.changed = True

    def updateTimestamp(self):
        if self.badData:
            return
        timestamp = self.status.get("timestamp")
        if timestamp == None or time.time() - timestamp > DEAD_TIME:
            self.stalled = True
            self.set({"lightColor": REDLIGHT,
                      "main":"Bombardier has stalled.",
                      "sub":"", "icon":FAIL})
        else:
            self.stalled = False

    def updateGauges(self):
        if self.status.get("status") == None:
            return FAIL
        if self.status["status"].has_key("percentage"):
            try:
                appPercent = int(self.status["status"].get("percentage"))
            except TypeError:
                appPercent = 0
            if self.status.has_key("todo"):
                todo = self.status["todo"]
                leftToDo = float(len(todo))
                progressData = self.status.get("install-progress")
                if progressData:
                    total    = float(len(progressData.keys()) + leftToDo)
                    if total == 0:
                        percentDone = 100
                    else:
                        percentDone = 100.0 * ((total - leftToDo) / total)
                else:
                    percentDone = 0
                
        self.set({"application":appPercent, "overall": percentDone, "todo": todo})
        return OK

    def examineOverall(self):
        light  = OFFLIGHT
        sub = ''
        if not self.stalled and not self.badData:
            overall = self.status["status"].get("overall")
            if overall == INSTALLING:
                sub = self.status["status"]["package"]
                light = GREENLIGHT
            elif overall == IDLE:
                light = GREENLIGHT
            elif overall == ERROR:
                light = REDLIGHT
            elif overall == OFFLINE:
                light = OFFLIGHT
            elif overall == WARNING:
                light = YELLOWLIGHT
            else:
                light = GREENLIGHT
        else:
            light = REDLIGHT
        self.set({"sub":sub, "lightColor":light})

    def run(self):
        if self.debug: time.sleep(1)
        while not self.commSocket.testStop():
            self.changed = False
            self.updateData()
            self.ensureStructure()
            self.updateTimestamp()
            self.updateGauges()
            self.set({"action": self.status["status"]["action"]})
            if not self.stalled and not self.badData:
                self.set({"main": self.status["status"]["main"]})
            self.examineOverall()
            self.paintScreen()
            time.sleep(self.updateMult * UPDATE_FREQ)
