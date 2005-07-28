import webUtil, yaml
from bombardier.Exceptions import *

class Location:
    def __init__(self, name, locationData):
        self.locationData = locationData
        self.name = name
        self.data = {}
        self.description = ''
        self.configKeys = 0

    def getInfo(self):
        self.data = self.locationData.get(self.name)
        if self.data == None:
            return
        if self.data.has_key("description"):
            self.description = self.data.get("description")
            del self.data["description"]
        self.configKeys = len(self.data.keys())
        self.stringData = yaml.dump(self.data)
