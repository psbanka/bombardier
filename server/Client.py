from static import *
import time
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

        self.action    = "UNKNOWN"
        self.alive     = False
        self.projects  = []
        self.endDays   = 0
        self.managers  = []
        self.owners    = []
        self.percentage = 0.0
        self.minSinceUpdate = NEVER
        self.packageList = []
        self.packageDetail = {}
        self.valid = True
        
        self.statusData = webUtil.readClientStatus(self.name)
        self.status = self.statusData.get("status")
        if self.status == None or self.status == '':
            self.status = {}
        else:
            self.action = self.status.get("action")
            if self.action == None or self.action == '':
                self.action = "UNKNOWN"
        if self.statusData.get("timestamp"):
            try:
                self.minSinceUpdate = float(time.time() - self.statusData["timestamp"]) / (60.0)
                if self.minSinceUpdate > DEAD_TIME:
                    self.alive = False
                else:
                    self.alive = True
            except ValueError:
                pass
        else:
            pass

        self.packageGroups = self.data.get("packageGroups")
        if self.packageGroups == None:
            self.packageGroups = []
        for packageGroup in self.packageGroups:
            bomData = webUtil.readBom(packageGroup)
            if bomData:
                self.packageList += bomData

        self.todo = self.statusData.get("todo")
        if self.todo:
            if self.packageList != []:
                totalPackages = float(len(self.packageList))
                self.percentage = 100.0 * ((totalPackages - float(len(self.todo))) / totalPackages)

    def getIndexes(self):
        index = webUtil.getYaml("deploy/client/index.yml")
        self.managers = webUtil.getIndexed("client", self.name, "contact", "managedclients", index)
        self.owners   = webUtil.getIndexed("client", self.name, "contact", "ownedclients", index)
        self.projects = webUtil.getIndexed("client", self.name, "project", "clients", index)
        
    def getAvailability(self):
        for projectName in self.projects:
            project = Project.Project(projectName)
            if project.endDays > self.endDays:
                self.endDays = project.endDays

    def getPackageDetail(self):
        groupProgress = self.statusData.get("install-progress")
        self.installed, self.uninstalled = webUtil.getClientInstalledUninstalled(self.name,
                                                                                 groupProgress)
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

