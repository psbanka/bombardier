import webUtil
from bombardier.Exceptions import *

class Hardware:
    def __init__(self, name):
        self.data = webUtil.readHardwareData(name)
        self.name = name
        self.location = ''
        self.description = ''
        self.type = ''
        self.rack = ''
        self.client = ''
        if self.data == None:
            return
        self.location    = self.data.get("location")
        self.description = self.data.get("description")
        self.type        = self.data.get("type")
        self.rack        = self.data.get("rack")
        self.client      = self.data.get("client")

