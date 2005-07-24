import webUtil
from bombardier.Exceptions import *

class Hardware:
    def __init__(self, name):
        self.name = name
        self.data = {}
        self.location = ''
        self.description = ''
        self.type = ''
        self.rack = ''
        self.client = ''

    def getInfo(self):
        try:
            self.data = webUtil.readHardwareData(self.name)
        except ServerUnavailable:
            self.data = {}
        if self.data == {}:
            return
        self.location    = self.data.get("location")
        self.description = self.data.get("description")
        self.type        = self.data.get("type")
        self.rack        = self.data.get("rack")
        self.client      = self.data.get("client")

