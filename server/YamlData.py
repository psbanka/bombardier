import yaml, os, glob, sys
import webUtil

from static import *

INDEX_FILE = "index.yml"
import bombardier.Server
server = bombardier.Server.Server(None, {"address":"http://127.0.0.1:8080"})


def baseName(yamlFile):
    if os.path.sep in yamlFile:
        base1 = yamlFile.split(os.path.sep)[-1]
    else:
        base1 = yamlFile
    return base1[:base1.rfind('.yml')]

def getIndex(table1RecordName, table2, field): #clientName, tableName, fieldName):
    output = []
    table2YamlFiles   = server.serviceRequest("deploy/%s/" % table2,
                                              legacyPathFix=False).split('\n')
    for yamlFileName in table2YamlFiles:
        if not yamlFileName.endswith('.yml') or yamlFileName == INDEX_FILE:
            continue
        data = webUtil.getYaml("deploy/%s/%s" % (table2, yamlFileName))
        table2RecordName = baseName(yamlFileName)
        if not data.get(field):
            continue
        if table1RecordName in data[field]:
            output.append(table2RecordName)
    return output

def mkIndex(table1, table2, field): #tableName, fieldName):
    try:
        table1Index = webUtil.getYaml("deploy/%s/%s" % (table1, INDEX_FILE))
    except:
        table1Index = {}
    table1YamlFiles = server.serviceRequest("deploy/%s/" % table1,
                                              legacyPathFix=False).split('\n')
    for yamlFileName in table1YamlFiles:
        if not yamlFileName.endswith('.yml') or yamlFileName == INDEX_FILE:
            continue
        table1RecordName = baseName(yamlFileName)
        if table1RecordName not in table1Index.keys():
            table1Index[table1RecordName] = {}
        if not table1Index[table1RecordName].get(table2):
            table1Index[table1RecordName][table2] = {}
        value = getIndex(table1RecordName, table2, field)
        table1Index[table1RecordName][table2][field] = value
    uploadPath = "website/service/putfile/%s/%s" % (table1, INDEX_FILE)
    serverResponse = server.serviceYamlRequest(uploadPath, putData = table1Index,
                                               legacyPathFix=False)
    return serverResponse

#############################

def filterFiles(listing, suffix):
    output = []
    for item in listing:
        if item in IGNORE_FILES:
            continue
        if item.endswith(suffix):
            basename = item[:item.rfind('.')]
            output.append(basename)
    output.sort()
    return output
    
def filterYamlFiles(listing):
    return filterFiles(listing, '.yml')

def lowerCaseSearch(list, item):
    for listItem in list:
        if listItem.lower() == item.lower():
            return listItem

def readData(path):
    data = server.serviceYamlRequest(path, debug=True, legacyPathFix=False)
    if type(data) != type(dict()):
        return {}
    return data

class YamlData:

    def __init__(self, name, fields, indexes = {}):
        self.fields  = fields
        self.indexes = indexes
        self.cname   = self.__class__.__name__.lower()
        recordNames  = filterYamlFiles( server.serviceRequest("deploy/%s" % self.cname,
                                                             legacyPathFix=False, debug=True).split('\n') )
        if recordNames:
            lowerName = lowerCaseSearch(recordNames, name)
        else:
            lowerName = None
        if lowerName == None:
            self.name = name.lower()
            self.new  = True
        else:
            self.name = lowerName
            self.new  = False
        self.path = "deploy/%s/%s.yml" % (self.cname, self.name)
        if not self.new:
            self.data = readData(self.path)
            if self.data:
                self.new = False
        else:
            self.data = {}
        for field in fields:
            if self.data.get(field):
                command = "self.%s = self.data['%s']" % (field, field)
            else:
                command = "self.%s = None" % field
            exec(command)

    def commit(self):
        uploadPath = "website/service/putfile/%s/%s.yml/" % (self.cname, self.name)
        for field in self.fields:
            command = "self.data['%s'] = self.%s" % (field, field)
            exec(command)
        serverResponse = server.serviceYamlRequest(uploadPath, putData = self.data,
                                                   debug=True, legacyPathFix=False)
        if serverResponse == "OK":
            status = self.mkIndexes()
            if status == OK:
                return "OK"
        return "FAIL"
        
    def mkIndexes(self):
        status = OK
        for dependentTable in self.indexes.keys():
            field = self.indexes[dependentTable]
            indexStatus = mkIndex(dependentTable, self.cname, field)
            if indexStatus == FAIL:
                status = FAIL
        return status
