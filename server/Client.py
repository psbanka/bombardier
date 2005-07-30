from static import *
import time
import Project
import Contact
import Hardware
import time
import webUtil
from bombardier.Exceptions import *
import YamlData

def paragraphify(text):
    output = []
    for line in text.split('\n'):
        output.append('<p>%s</p>' % line)
    return '\n'.join(output)


class Client(YamlData.YamlData):

    def timelog(self, str):
        elapsed = time.time() - self.time
        open("time.log", 'a').write("%s :: %s elapsed\n" % (str, elapsed))
        self.time = time.time()

    def __init__(self, name):
        fields = ["packageGroups"]
        YamlData.YamlData.__init__(self, name, fields)

        self.status    = "UNKNOWN"
        self.alive     = False
        self.projects  = []
        self.endDays   = 0
        self.managers  = []
        self.owners    = []
        self.percentage = 0.0
        self.minSinceUpdate = NEVER
        self.packageList = []
        self.hardware    = ''
        self.packageDetail = {}
        self.valid = True
        
        self.config = webUtil.readClientData(self.name)
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
            bomData = webUtil.readBom(packageGroup)
            if bomData:
                self.packageList += bomData

        groupProgress = webUtil.readClientProgress(self.name)
        self.installed, self.uninstalled = webUtil.getClientInstalledUninstalled(self.name, groupProgress)


        if self.packageList != []:
            self.percentage = 100.0 * (float(len(self.installed)) / float(len(self.packageList)))

        index = webUtil.getYaml("deploy/client/index.yml")
        self.managers = webUtil.getIndexed("client", self.name, "contact", "managedclients", index)
        self.owners   = webUtil.getIndexed("client", self.name, "contact", "ownedclients", index)
        self.projects = webUtil.getIndexed("client", self.name, "project", "clients", index)
        self.hardware = webUtil.getIndexed("client", self.name, "hardware", "client", index)
        if self.hardware:
            self.hardware = self.hardware[0]
        else:
            self.hardware = ''

        for projectName in self.projects:
            project = Project.Project(projectName)
            if project.endDays > self.endDays:
                self.endDays = project.endDays

    def getPackageDetail(self):
        # want to know the installation status of each package
        for packageGroup in self.packageGroups:
            self.packageDetail[packageGroup] = {"packages":{},"installedStatus":"OK"}
            packageNames = webUtil.readBom(packageGroup)
            if packageNames:
                for packageName in packageNames:
                    if packageName in self.installed:
                        self.packageDetail[packageGroup]["packages"][packageName] = "OK"
                    else:
                        self.packageDetail[packageGroup]["packages"][packageName] = "FAIL"
                        self.packageDetail[packageGroup]["installedStatus"] = "FAIL"

