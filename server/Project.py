import webUtil
import Contact
import time
from bombardier.Exceptions import *

import YamlData

DAY = 60*60*24

class Project(YamlData.YamlData):
    def __init__(self, name):
        fields = ["start", "finish", "contactid", "clients"]
        indexes = {"contact": "contactid", "client":"clients"}
        YamlData.YamlData.__init__(self, name, fields, indexes)

        if self.new:
            thirtyDays = time.localtime(time.time()+(DAY*30))
            self.endTime = time.localtime()
            self.endDays = 0
            self.contactName = ""
            return

        if self.contactid:
            self.contactName = Contact.Contact(self.contactid).fullname
        else:
            self.contactName = ""
        self.endTime = time.strptime(self.finish, "%Y-%m-%d")
        self.endDays = (time.mktime(self.endTime) - time.time()) / DAY

