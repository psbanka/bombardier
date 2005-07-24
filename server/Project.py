import webUtil
import Contact
import time
from bombardier.Exceptions import *

DAY = 60*60*24

class Project:
    def __init__(self, name, projectData, contactData):
        self.projectData = projectData
        self.contactData = contactData

        self.name = name
        self.new  = True
        self.contact = ''

    def getInfo(self):
        self.data = self.projectData.get(self.name)

        self.clients = []
        if not self.data:
            self.start = ''
            self.finish = ''
            self.contact = ''
            self.clients = []
            thirtyDays = time.localtime(time.time()+(DAY*30))
            self.endTime = time.localtime()
            self.endDays = 0
            return
        self.new     = False
        self.start   = self.data.get("start")
        self.finish  = self.data.get("finish")
        contactid    = self.data.get("contact")

        for contactName in self.contactData.keys():
            contact  = Contact.Contact(contactName, self.contactData, self.projectData)
            contact.getInfo()
            if contactid == contactName:
                self.contact = contact.fullname
        self.clients = self.data.get("clients")
        self.endTime = time.strptime(self.finish, "%Y-%m-%d")
        self.endDays = (time.mktime(self.endTime) - time.time()) / DAY

