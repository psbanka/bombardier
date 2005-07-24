import webUtil
from bombardier.Exceptions import *

class Contact:
    def __init__(self, name, contactData, projectData):
        self.contactData = contactData
        self.projectData = projectData
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
        self.data = self.contactData.get(self.name)
        if self.data == {} or self.data == None:
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

        for projectName in self.projectData.keys():
            projectOwner = self.projectData.get("contact")
            if type(projectOwner) != type("string"):
                continue
            if projectOwner.lower() == self.name.lower():
                self.projects.append(projectName)

