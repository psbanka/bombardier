from static import *
import Project
import Contact
import Hardware
import time
import webUtil
from bombardier.Exceptions import *

class Client:
    def __init__(self, name):
        self.name      = name
        self.status    = "UNKNOWN"
        self.alive     = False
        self.projects  = []
        self.endDays   = 0
        self.managers  = []
        self.owners    = []
        self.packageGroups = []
        self.percentage = 0.0
        self.minSinceUpdate = DEAD_TIME + 1000
        self.packageList = []
        self.hardware    = ''
        self.packageDetail = {}
        self.valid = True

    def getInfo(self):
        try:
            self.config = webUtil.readClientData(self.name)
        except ServerUnavailable:
            self.config = {}
        self.packageGroups = self.config.get("packageGroups")
        if self.packageGroups == None:
            self.packageGroups = []
        self.statusData = webUtil.readClientLastStatus(self.name)
        self.status = self.statusData.get("severity")
        if self.status == None or self.status == '':
            self.status = "UNKNOWN"
        self.lastMessage = self.statusData.get("message")
        if self.lastMessage == None or self.lastMessage.strip() == '':
            self.lastMessage = "<< None >>"

        if self.statusData.get("time"):
            try:
                self.lastUpdate = time.mktime(time.strptime(self.statusData["time"]))
                self.minSinceUpdate = float(time.time() - self.lastUpdate) / (60.0)
                if self.minSinceUpdate > DEAD_TIME:
                    self.alive = False
                else:
                    self.alive = True
            except ValueError:
                pass
        else:
            pass

        for packageGroup in self.packageGroups:
            self.packageList += webUtil.readBom(packageGroup)
        
        self.installed, self.uninstalled = webUtil.getClientInstalledUninstalled(self.name)

        if self.packageList != []:
            self.percentage = 100.0 * (float(len(self.installed)) / float(len(self.packageList)))
        contactNames = webUtil.getContactNames()
        for contactName in contactNames:
            contact = Contact.Contact(contactName)
            contact.getInfo()
            if self.name in contact.managedClients:
                self.managers.append(contactName)
            if self.name in contact.ownedClients:
                self.owners.append(contactName)

        projectNames = webUtil.getProjectNames()
        for projectName in projectNames:
            project = Project.Project(projectName)
            project.getInfo()
            if self.name in project.clients:
                self.projects.append(projectName)
                if project.endDays > self.endDays:
                    self.endDays = project.endDays

        hardwareNames = webUtil.getHardwareNames()
        for hardwareName in hardwareNames:
            hardware = Hardware.Hardware(hardwareName)
            hardware.getInfo()
            if hardware.client == self.name:
                self.hardware = hardwareName
                break
        
    def getPackageDetail(self):
        # want to know the installation status of each package
        for packageGroup in self.packageGroups:
            self.packageDetail[packageGroup] = {"packages":{},"installedStatus":"OK"}
            for packageName in webUtil.readBom(packageGroup):
                if packageName in self.installed:
                    self.packageDetail[packageGroup]["packages"][packageName] = "OK"
                else:
                    self.packageDetail[packageGroup]["packages"][packageName] = "FAIL"
                    self.packageDetail[packageGroup]["installedStatus"] = "FAIL"

