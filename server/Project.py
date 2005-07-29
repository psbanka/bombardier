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
        self.data = webUtil.readProjectData(self.name)

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
        self.contactid = self.data.get("contact")
        contact      = Contact.Contact(self.contactid)
        self.contact = contact.fullname

        self.clients = self.data.get("clients")
        self.endTime = time.strptime(self.finish, "%Y-%m-%d")
        self.endDays = (time.mktime(self.endTime) - time.time()) / DAY

