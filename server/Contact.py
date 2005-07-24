import webUtil
from bombardier.Exceptions import *

class Contact:
    def __init__(self, name):
        self.name = name
        self.new  = True
        self.data = {}
        self.fullname = ""
        self.email = ''
        self.projects = []
        self.ownedClients = []
        self.managedClients = []

    def getInfo(self):
        self.filename = self.name.lower()
        try:
            self.data = webUtil.readContactData(self.name)
        except ServerUnavailable:
            self.data = {}
        if self.data == {}:
            return
        self.new       = False
        self.fullname  = self.data.get("fullname")
        self.email     = self.data.get("email")
        managedClients = self.data.get("managedclients")
        ownedClients   = self.data.get("ownedclients")
        if ownedClients:
            self.ownedClients = ownedClients
        if managedClients:
            self.managedClients = managedClients
        projectNames = webUtil.getProjectNames()
        for projectName in projectNames:
            projectOwner = webUtil.readProjectData(projectName).get("contact")
            if type(projectOwner) != type("string"):
                continue
            if projectOwner.lower() == self.name.lower():
                self.projects.append(projectName)

