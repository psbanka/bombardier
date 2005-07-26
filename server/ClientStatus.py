import cherrypy
import yaml, os, time
import Root
import webUtil

from static import *

"""This thing allows queries and posts to client configuration
files. It should also allow puts, but doesn't yet. These routines
don't require authentication, and should. All posts are basically
added to the configuration for the client, where PUTs are meant to
overwrite."""

def doc():
    return """client status web service
    
get: <client> 
    (no arguments):           Will return a list of all system names that
                              you can obtain status about
    client:                   will return the current overall system install
                              status for this client, including details of every
                              package they supposedly should have installed,
                              when they last contacted the server, etc.

put: <client> (yaml data of install progress)
    client:                   name of the client that the install progress
                              pertains to.
   
"""
        
def updateStatus(clientName):
    """ Determine whether the client is up-to-date or not """

    output = {}
    clientPath = os.path.join(ROOT_DIR, "log", clientName)
    ymlPath = os.path.join(clientPath, LAST_STATUS)
    output = {"elapsed":0,
              "lastStatus":"",
              "lastMessage":""}
    if not os.path.isfile(ymlPath):
        return output
    y = yaml.loadFile(ymlPath)
    ydata = y.next()
    output["lastMessage"] = ydata["message"]
    output["lastStatus"]  = ydata["severity"]
    checkinTime  = time.mktime(time.strptime(ydata["time"]))
    now = time.time()
    output["elapsed"] = now - checkinTime 
    return output

def clientDetail(clientName):
    client = Client(clientName)
    client.gatherData()
    client.getPackageDetail()
    output = {"status":client.status,
              "packageGroups": client.packageGroups,
              "percentage":client.percentage,
              "lastUpdateElapsed":client.lastUpdateElapsed,
              "alive":client.alive,
              "projects":client.projects,
              "endTime":time.mktime(client.endTime),
              "itContact":client.itContact,
              "owner":client.owner,
              "system":client.system,
              "lastMessage":client.lastMessage,
              "packageDetail":client.packageDetail}
    return yaml.dump(output)

class Client:

    def __init__(self, name):
        self.name      = name
        self.status    = None
        self.alive     = False
        self.projects  = []
        self.endTime   = 0
        self.itContact = ''
        self.owner     = ''
        self.packageGroups = []
        self.percentage = 0.0
        self.lastUpdateElapsed = DEAD_TIME + 1000
        self.packageList = []
        self.system    = ''
        self.packageDetail = {}
        self.valid = True

    def gatherData(self):
        self.config = webUtil.readClientData(self.name)
        if self.config.get("info"):
            isClient = self.config.get("info").get("client")
            if type(isClient) == type("string"):
                if isClient.lower().startswith('f'):
                    self.valid = False
                    return
        packageGroupString = self.config.get("packageGroups")
        if type(packageGroupString) == type(dict()):
            for packageGroup in packageGroupString.values():
                self.packageGroups.append(packageGroup)
        self.statusData = webUtil.readClientLastStatus(self.name)
        self.status = self.statusData.get("severity")
        self.lastMessage = self.statusData.get("message")
        try:
            self.lastUpdate = time.mktime(time.strptime(self.statusData["time"]))
            self.lastUpdateElapsed = time.time() - self.lastUpdate
            if self.lastUpdateElapsed > DEAD_TIME:
                self.alive = False
            else:
                self.alive = True
        except ValueError:
            pass

        for packageGroup in self.packageGroups:
            self.packageList += webUtil.readBom(packageGroup)
        
        self.installed, self.uninstalled = webUtil.getClientInstalledUninstalled(self.name)
        if self.packageList != []:
            self.percentage = 100.0 * (float(len(self.installed)) / float(len(self.packageList)))
        projectData = webUtil.getProjectData()

        try:
            self.itContact  = self.config["info"]["itContact"]
            self.owner      = self.config["info"]["owner"]
            self.system     = self.config["info"]["system"]
        except:
            pass

        for projectName in projectData.keys():
            if projectName == "info":
                continue
            projectInfo = projectData[projectName]
            clients = projectInfo["clients"]
            if self.name in clients:
                self.projects.append(projectName)
                info            = webUtil.getProjectInfo().get(projectName)
                projectEnd      = time.strptime(info.get("finish"), "%Y-%m-%d")
                if projectEnd > self.endTime:
                    self.endTime = projectEnd
        if self.endTime == 0:
            self.endTime = time.localtime()
        
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


class ClientStatus(Root.Root):

    known_methods = ["GET", "POST", "PUT"]

    def GET(self, client=None):
        cherrypy.response.headerMap["Content-type"] = "text/plain"
        return "ClientStatus"
        if not client:
            clientNames = webUtil.getSystemNames()
            output = {}
            for clientName in clientNames:
                if clientName != '':
                    client = Client(clientName)
                    client.gatherData()
                    if not client.valid:
                        continue
                    output[client.name] = {"status":client.status,
                                           "packageGroups": client.packageGroups,
                                           "percentage":client.percentage,
                                           "lastUpdateElapsed":client.lastUpdateElapsed,
                                           "alive":client.alive,
                                           "projects":client.projects,
                                           "endTime":time.mktime(client.endTime),
                                           "itContact":client.itContact,
                                           "owner":client.owner,
                                           "system":client.system}
            return yaml.dump(output)
        if client.rfind('..') != -1:
            cherrypy.response.status = 500
            return "illegal client name\n"
        return clientDetail(client)

    def POST(self):
        """ This would be a good place for
        clients to put their install-progress or verify"""
        cherrypy.response.headerMap["Content-type"] = "text/plain"        
        return "NOT IMPLEMENTED"

    def PUT(self, client, message):
        cherrypy.response.headerMap["Content-type"] = "text/plain"
        data = cherrypy.request.body.read()
        if "INSTALL" in message.upper():
            installProgressDir = os.path.join(ROOT_DIR, "log", client)
            if not os.path.isdir(installProgressDir):
                try:
                    os.mkdir(installProgressDir)
                except OSError:
                    cherrypy.response.status = 405
                    return "--- Unable to create directory (%s)\n" % installProgressDir
            progressPath = os.path.join(installProgressDir, "install-progress.yml")
            status = webUtil.verifyAndWriteYaml(data, progressPath)
        else:
            "Good Place to upload verify data"
            return "--- Unknown status message\n"
        if status == OK:
            return "--- OK\n"
        return "--- FAIL (Bad YAML data)\n"
