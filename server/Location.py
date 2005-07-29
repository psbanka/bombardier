import webUtil, yaml
from bombardier.Exceptions import *
import YamlData

class Location(YamlData.YamlData):
    def __init__(self, name):
        fields = ["description"]
        YamlData.YamlData.__init__(self, name, fields)

        if self.data.has_key("description"):
            del self.data["description"]
        self.configKeys = len(self.data.keys())
        self.stringData = yaml.dump(self.data)
