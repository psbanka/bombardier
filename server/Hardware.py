import webUtil
from bombardier.Exceptions import *
import YamlData

class Hardware(YamlData.YamlData):
    def __init__(self, name):
        fields = ["location", "description", "type", "rack", "client"]
        indexes = {"client": "client"}
        YamlData.YamlData.__init__(self, name, fields, indexes)


