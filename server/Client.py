import bombardier.miniUtility as miniUtility
import os
import yaml

from bombardier.staticData import OK, FAIL

class Client:

    def __init__(self, systemName):
        self.data       = {}
        self.includes   = []
        self.systemName = systemName

    def findIncludeList(data):
        includeList = []
        for key in data.keys():
            if key.upper() == "INCLUDE":
                includeList += data[key]
        return includeList

    findIncludeList = staticmethod(findIncludeList)

    def loadIncludes(self, newIncludes):
        for includeName in newIncludes:
            if includeName not in self.includes:
                self.includes.append(includeName)
                self.downloadClient(includeName)

    ### TESTED
    def downloadClient(self, configName=''):
        if configName == '':
            ymlDirectory = "client"
            configName = self.systemName
        else:
            ymlDirectory = "include"
        filename   = "deploy/%s/%s.yml" % (ymlDirectory, configName)
        newData   = yaml.load( open(filename, 'r').read() )
        self.data = miniUtility.addDictionaries(self.data, newData)
        newIncludes = self.findIncludeList(newData)
        self.loadIncludes(newIncludes)
        return OK

    def loadIncludes(self, newIncludes):
        for includeName in newIncludes:
            if includeName not in self.includes:
                self.includes.append(includeName)
                self.downloadClient(includeName)


if __name__ == "__main__":
    import sys
    client = sys.argv[1]
    config = Client(client)
    status = config.downloadClient()
    print yaml.dump(config.data)
