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

        self.filename = self.name.lower()
        self.data = webUtil.readContactData(self.name)
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

        self.projects = webUtil.getIndexed("contacts", self.name, "projects", "contact")

