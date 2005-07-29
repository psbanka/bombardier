import webUtil
from bombardier.Exceptions import *

import YamlData

class Contact(YamlData.YamlData):
    def __init__(self, name):
        fields = ["fullname", "email", "managedclients", "ownedclients"]
        indexes = {"client": "managedclients", "client":"ownedclients"}
        YamlData.YamlData.__init__(self, name, fields, indexes)
        self.projects = webUtil.getIndexed("contact", self.name, "project", "contactid")
