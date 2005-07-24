import webUtil
import Contact
import time
from bombardier.Exceptions import *

DAY = 60*60*24

class Project:
    def __init__(self, name):
        self.name = name
        self.new  = True
        self.contact = ''

    def getInfo(self):
        try:
            self.data = webUtil.readProjectData(self.name)
        except ServerUnavailable:
            self.data = {}

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
        contactNames = webUtil.getContactNames()
        for contactName in contactNames:
            contact  = Contact.Contact(contactName)
            contact.getInfo()
            if contactid == contactName:
                self.contact = contact.fullname
        self.clients = self.data.get("clients")
        self.endTime = time.strptime(self.finish, "%Y-%m-%d")
        self.endDays = (time.mktime(self.endTime) - time.time()) / DAY

