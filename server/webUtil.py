import ConfigParser, urlparse, urllib2, template, threading, string, httplib, StringIO, urllib, re
import cherrypy
import yaml, os, random, time, urllib

from static import *

EMPTY_LAST_STATUS = {"message": "", "section": "", "severity":"", "time":""}
PACKAGE_MATCH = re.compile("(\S+)\-\d+")

import bombardier.Server
server = bombardier.Server.Server(None, {"address":"http://127.0.0.1:8080"})

def readData(path):
    data = server.serviceYamlRequest(path)
    if type(data) != type(dict()):
        return {}
    return data

def readClientData(clientName):
    return readData("deploy/client/"+clientName+".yml")

def readContactData(contactName):
    return readData("deploy/contacts/"+contactName+".yml")

def readProjectData(projectName):
    return readData("deploy/projects/"+projectName+".yml")

def readHardwareData(hardwareName):
    return readData("deploy/hardware/"+hardwareName+".yml")

def readProjectData(projectName):
    data = server.serviceYamlRequest("deploy/projects/"+projectName+".yml")
    if type(data) != type(dict()):
        return {}
    return data
    

def readClientLastStatus(clientName):
    try:
        return server.serviceYamlRequest("deploy/log/"+ clientName + "/last.yml")
    except:
        return EMPTY_LAST_STATUS

def filterList(list):
    output = []
    for item in list:
        if item.strip() != '':
            output.append(item)
    return output

def readBom(pkgGroup):
    try:
        return filterList(server.serviceRequest("deploy/bom/" + pkgGroup + ".BOM").split('\n'))
    except:
        return []

def readClientProgress(clientName):
    try:
        return server.serviceYamlRequest("deploy/log/" + clientName + "/install-progress.yml")
    except:
        return {}

def stripVersion(packageName):
    if type(packageName) == type("string"):
        match = PACKAGE_MATCH.findall(packageName)
        if match:
            return match[0]
    return ''

# FIXME: code envy from Package.py
def getTimeStruct(s):
    if s == "NA":
        return 0
    try:
        timestruct = int(time.mktime(time.strptime(s)))
    except:
        timestruct = int(time.time())
    return timestruct

def getClientInstalledUninstalled(clientName): 
    progressData = readClientProgress(clientName)
    installed = []
    uninstalled = []
    for item in progressData.keys():
        datum  = progressData[item]
        if type(datum) != type(dict()):
            continue
        iTxt   = progressData[item].get("INSTALLED")
        iInt   = getTimeStruct(iTxt)
        uTxt   = progressData[item].get("UNINSTALLED")
        uInt   = getTimeStruct(uTxt)
        if uninstalled > installed:
            uninstalled.append(stripVersion(item))
            continue
        else:
            installed.append(stripVersion(item))
    return installed, uninstalled

def getClientNames():
    return filterYamlFiles( server.serviceRequest("deploy/client").split('\n') )

def getContactNames():
    return filterYamlFiles( server.serviceRequest("deploy/contacts").split('\n') )

def getProjectNames():
    return  filterYamlFiles(server.serviceRequest("deploy/projects").split('\n'))

def getHardwareNames():
    return  filterYamlFiles(server.serviceRequest("deploy/hardware").split('\n'))

def filterYamlFiles(listing):
    output = []
    for item in listing:
        if item.endswith('.yml'):
            output.append(item.split('.')[0])
    output.sort()
    return output

def makeTable(header, iterator):
    output = []
    output.append('<table width=750>')
    output.append(header)
    colorize = False
    for record in iterator():
        if colorize:
            output.append('<tr bgcolor=%s>' % ("#FFFFCC"))
        else:
            output.append('<tr bgcolor=%s>' % ("white"))
        colorize = not colorize
        name = record[0]
        path = urllib.pathname2url(name)
        path = name.replace(' ', '_')
        print ">>>>>>>>>",path
        output.append('<td><a href="./%s/">%s</a></td>' % (path, name))
        for i in record[1:]:
            output.append("<td>%s</td>" % i)
        output.append("</tr>")
    output.append('</table>')
    return output

def clientSelectionBox(selectedItems, name='clients', multi=True):
    output = []
    if multi:
        output.append('<SELECT multiple size="10" name="%s">' % name)
    else:
        output.append('<SELECT size="1" name="%s">' % name)
    clientNames = getClientNames()
    for client in clientNames:
        if client in selectedItems:
            output.append('<OPTION selected="selected" value="%s">%s</OPTION>' % (client, client))
        else:
            output.append('<OPTION>%s</OPTION>' % client)
    output.append('</SELECT>')
    return output

def nicifyForLegacyClients(data):
    output = {}
    for sectionName in data.keys():
        section = data[sectionName]
        if type(section) == type(dict()):
            output[sectionName] = section
        else:
            if sectionName == "packageGroups":
                newSection = {}
                for i in range(0,len(section)):
                    newSection["group"+`i`] = section[i]
                output["packageGroups"] = newSection
    return output

####################

def writeProjectInfo(projectInfo):
    projectsPath = os.path.join(getConfigPath(), "projects.yml")
    fh = open(projectsPath, 'w')
    try:
        yaml.dumpToFile(fh, projectInfo)
        status = OK
    except:
        status =  FAIL
    fh.close()
    return status

def getLogPath():
    return os.path.join(getDeployPath(), "log")

def getClientPath():
    return os.path.join(getDeployPath(), "client")

def getConfigPath():
    return os.path.join(getDeployPath(), "config")

def getInstallProgress(client):
    return os.path.join(getStatusPath(), client, "install-progress.yml")

def getStatusPath():
    return config.get("site", "statusdirectory")

def getConfigFile(filename):
    return os.path.join(getConfigPath(), filename)


def getDeployPath():
    path = config.get("site", "deployPath")
    return path

def getPackagesPath():
    packagesPath = os.path.join(getDeployPath(), PACKAGES_FILE)
    return packagesPath

def connectString(servername, instance, port):
    dataSource = servername
    if instance:
        dataSource += "\\"+instance
    if port:
        dataSource += ","+port
    return dataSource

def validateFilename(filename):
    if '..' in filename:
        return FAIL
    if '/' in filename:
        return FAIL
    return OK

def writeScratch(data=None, filename=None, location=None):
    identifier = `random.randint(0,10000)`
    filename = filename+identifier
    if validateFilename(filename) == FAIL:
        return FAIL
    if not os.path.isdir(location):
        os.makedirs(location)
    filePath = os.path.join(location, filename)
    if os.path.isfile(filePath):
        os.unlink(filePath)
    open(filePath, 'w').write(data)
    return filename

def verifyAndWriteConfig(configData, iniFile):
    # need to trick ConfigParser
    dataFile = StringIO.StringIO(configData)
    configParser = ConfigParser.ConfigParser()
    configParser.readfp(dataFile)
    dataFile.close()
    dataFile = open(iniFile, 'w')
    configParser.write(dataFile)
    dataFile.close()
    return OK

def verifyAndWriteYaml(configData, outputPath):
    try:
        data = yaml.load(configData)
        ymlString = yaml.dump(data.next())
        fh = open(outputPath, 'w')
        fh.write(ymlString)
        fh.close()
        return OK
    except:
        return FAIL

def serviceListing():
    baseDir = os.path.join(getDeployPath(),'config')
    inodes = os.listdir(baseDir)
    systemNames = set([''])
    for inode in inodes:
        if os.path.isfile(os.path.join(baseDir,inode)):
            if inode.lower().endswith('.ini') or inode.lower().endswith(".yml"):
                systemName = '.'.join(inode.split('.')[0:-1])
                systemName = systemName.lower()
                if systemName != '':
                    systemNames.update([systemName])
    systemList = list(systemNames)
    systemList.sort()
    return systemList


class Process(threading.Thread):
    def __init__(self, request, function, errlog, mainMenuList, 
                 subMenuList, args=[], capOnly=False):
        threading.Thread.__init__(self)
        self.request      = request
        self.function     = function
        self.mainMenuList = mainMenuList
        self.subMenuList  = subMenuList
        self.args         = args
        self.capOnly      = capOnly
        self.errlog       = errlog

    def run(self):
        output = self.function(self.args, self.errlog )
        if self.capOnly:
            self.request.write(string.join(output, '\n'))
            self.request.finish()
            return
        html   = template.generateHtml(mainMenuList=self.mainMenuList,
                                       subMenuList=self.subMenuList,
                                       body=string.join(output, '\n'))
        self.request.write(html)
        self.request.finish()

def serviceConfigPut(path, configParser, args={}):
    datafile = StringIO.StringIO()
    configParser.write(datafile)
    datafile.seek(0)
    outputString = datafile.read()
    return servicePut(path, args, data=outputString)

def servicePut(path, args={}, data=[], live = False):
    queryString = urllib.urlencode(args)
    return put(SERVICE+path+"/?"+queryString, data, live=live)

#^^^ Who uses this?
def put(location, data, username='', password='', live = False):
    location += '/'
    urldata = urlparse.urlparse(location)
    proto, site = urldata[:2]
    if len(urldata) >= 3:
        path = urldata[2]
    else:
        path=''
    if len(urldata) >= 5:
        variables = urldata[4]
    else:
        variables = ""
    h = httplib.HTTP(site)
    cherrypy.log( 'Sending a PUT to %s' % path+"/"+variables )
    h.putrequest('PUT', path+"?"+variables)
    h.putheader('Accept', '*/*')
    h.putheader('Allow', 'PUT')
    h.putheader('Accept-Encoding', '*,deflate')
    h.putheader('Expect', '100-continue')
    h.putheader('Connection', 'Keep-Alive')
    h.putheader('Content-Type', 'text/html')
    h.putheader('Content-Length', str(len(data)))
    h.endheaders()
    h.send(data)
    cherrypy.log( 'Getting reply...' )
    try:
        errcode, errmsg, headers = h.getreply()
    except: # would be more specific, but it just throws an "error"
        cherrypy.log( "Error performing PUT" )
        return FAIL
    if live:
        return h.getfile()
    body = h.getfile().read(500)
    h.close()
    if errcode == 200:
        return OK
    else:
        return FAIL

#^ RIPPED OFF FROM CONFIG
def makeConfigObject(data):
    configParser = ConfigParser.ConfigParser()
    for section in data.keys():
        if not configParser.has_section(section):
            configParser.add_section(section)
        datum = data[section]
        if type(datum) == type(dict()):
            for option in datum.keys():
                value = datum[option]
                if type(value) != type(dict()):
                    configParser.set(section, option, value)
                else:
                    ermsg =  "incompatible types (ini/yaml) for (%s:%s)" % (section, option)
                    cherrypy.log(ermsg)
        else:
            cherrypy.lo( "incompatible types (ini/yaml) for (%s)" % (section) )
    return configParser

def configToDict(configPath):
    configParser = ConfigParser.ConfigParser()
    configParser.read(configPath)
    outputDict = {}
    for sectionName in configParser.sections():
        outputDict[sectionName] = {}
        for optionName in configParser.options(sectionName):
            outputDict[sectionName][optionName] = configParser.get(sectionName, optionName)
    return outputDict

def configToYaml(configPath):
    outputDict = configToDict(configPath)
    return yaml.dump(outputDict)
    
# FIXME: Copy-and-paste from utility
def getPackageGroups(configObj, filetype=INI):
    packageGroups = []
    if filetype == INI:
        configParser = configObj
        groupNumber = 0
        while 1 == 1:
            try:
                groupStr = "group"+`groupNumber`
                group = configParser.get("packageGroups", groupStr)
                packageGroups.append(group)
                groupNumber += 1
            except ConfigParser.NoSectionError:
                break
            except ConfigParser.NoOptionError:
                break
        if packageGroups == []:
            try:
                systemtype = configParser.get("system", "type")
            except ConfigParser.NoOptionError:
                return []
            except ConfigParser.NoSectionError:
                return []
            packageGroups = ["base", systemtype]
    else:
        yamlObj = configObj
        configData = yaml.load(yamlObj).next()
        if configData.has_key("packageGroups"):
            if type(configData["packageGroups"]) == {}:
                packageGroups = configData["packageGroups"].values()
            elif type(configData["packageGroups"]) == ["list"]:
                packageGroups = configData["packageGroups"]
            elif type(configData["packageGroups"]) == "string":
                packageGroups = [configData["packageGroups"]]
            else:
                packageGroups = []
    return packageGroups

def readPackageGroups(clientName):
    output = {}
    filename, filetype = findFile(clientName)
    if not os.path.isfile(filename):
        cherrypy.log( "Data error: %s does not exist for client %s" % (filename, clientName))
        return output
    dataFile = open(os.path.join(filename))

    if filetype == INI:
        config = ConfigParser.ConfigParser()
        config.readfp(dataFile)
        dataFile.close()
        packageGroups = getPackageGroups(config)
    else:
        data = dataFile.read()
        packageGroups = getPackageGroups(data, filetype=YML)
    

