from static import *
import Project
import Contact
import Hardware
import time
import webUtil
from bombardier.Exceptions import *

def paragraphify(text):
    output = []
    for line in text.split('\n'):
        output.append('<p>%s</p>' % line)
    return '\n'.join(output)

class Client:
    def __init__(self, name, clientData, contactData, projectData,
                 hardwareData, bomData, progressData):
        self.clientData   = clientData
        self.projectData  = projectData
        self.hardwareData = hardwareData
        self.bomData      = bomData
        self.progressData = progressData
        self.contactData  = contactData

        self.name      = name
        self.status    = "UNKNOWN"
        self.alive     = False
        self.projects  = []
        self.endDays   = 0
        self.managers  = []
        self.owners    = []
        self.packageGroups = []
        self.percentage = 0.0
        self.minSinceUpdate = NEVER
        self.packageList = []
        self.hardware    = ''
        self.packageDetail = {}
        self.valid = True

    def getInfo(self):
        self.config = webUtil.lcDictFind(self.clientData, self.name)
        self.packageGroups = self.config.get("packageGroups")
        if self.packageGroups == None:
            self.packageGroups = []
        self.statusData = webUtil.readClientLastStatus(self.name)
        self.status = self.statusData.get("severity")
        if self.status == None or self.status == '':
            self.status = "UNKNOWN"
        self.lastMessage = paragraphify(self.statusData.get("message"))
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
            bomData = self.bomData.get(packageGroup)
            if bomData:
                self.packageList += self.bomData.get(packageGroup)

        groupProgress = webUtil.lcDictFind(self.progressData, self.name)
        self.installed, self.uninstalled = webUtil.getClientInstalledUninstalled(self.name, groupProgress)

        if self.packageList != []:
            self.percentage = 100.0 * (float(len(self.installed)) / float(len(self.packageList)))

        for contactName in self.contactData.keys():
            contact = Contact.Contact(contactName, self.contactData, self.projectData)
            contact.getInfo()
            if self.name in contact.managedClients:
                self.managers.append(contactName)
            if self.name in contact.ownedClients:
                self.owners.append(contactName)

        for projectName in self.projectData.keys():
            project = Project.Project(projectName, self.projectData, self.contactData)
            project.getInfo()
            if self.name in project.clients:
                self.projects.append(projectName)
                if project.endDays > self.endDays:
                    self.endDays = project.endDays

        for hardwareName in self.hardwareData.keys():
            hardware = Hardware.Hardware(hardwareName, self.hardwareData)
            hardware.getInfo()
            if hardware.client.lower() == self.name.lower():
                self.hardware = hardwareName
                break
        
    def getPackageDetail(self):
        # want to know the installation status of each package
        for packageGroup in self.packageGroups:
            self.packageDetail[packageGroup] = {"packages":{},"installedStatus":"OK"}
            packageNames = self.bomData.get(packageGroup)
            if packageNames:
                for packageName in packageNames:
                    if packageName in self.installed:
                        self.packageDetail[packageGroup]["packages"][packageName] = "OK"
                    else:
                        self.packageDetail[packageGroup]["packages"][packageName] = "FAIL"
                        self.packageDetail[packageGroup]["installedStatus"] = "FAIL"

