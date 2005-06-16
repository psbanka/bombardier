import ConfigParser, urlparse, urllib2, template, threading, string, httplib, StringIO, urllib
import yaml, os, random

from static import *

config = ConfigParser.ConfigParser()
config.read("webserver.ini")
SERVICE = config.get("site", "address")


def findFile(clientName):
    configPath = os.path.join(ROOT_DIR, "deploy", "config")
    filename = ''
    for inode in os.listdir(configPath):
        inodePath = os.path.join(configPath, inode)
        if not os.path.isfile(inodePath):
            continue
        basename  = ''.join(inode.split('.')[:-1])
        extension = ''.join(inode.split('.')[-1:])
        if basename.lower() == clientName.lower():
            if extension.lower() == 'yml':
                filename = os.path.join(configPath, inode)
                return filename, YML
            elif extension.lower() == 'ini':
                filename = os.path.join(configPath, inode)
    if filename:
        return filename,INI
    else:
        return '', None

def getDeployPath():
    path = config.get("site", "deployPath")
    return path

def connectString(servername, instance, port):
    dataSource = servername
    if instance:
        dataSource += "\\"+instance
    if port:
        dataSource += ","+port
    return dataSource

def writeScratch(data=None, filename=None, location=None):
    id = `random.randint(0,10000)`
    filename = filename+id
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
    config = ConfigParser.ConfigParser()
    config.readfp(dataFile)
    dataFile.close()
    dataFile = open(iniFile, 'w')
    config.write(dataFile)
    dataFile.close()

def verifyAndWriteYaml(configData, outputPath):
    try:
        config = yaml.load(configData)
        ymlString = yaml.dump(config.next())
        fh = open(outputPath, 'w').write(ymlString)
        return OK
    except:
        return FAIL

def getSystemNames():
    baseDir = os.path.join('deploy','config')
    inodes = os.listdir(baseDir)
    systemNames = ['']
    for inode in inodes:
        if os.path.isfile(os.path.join(baseDir,inode)) and inode.endswith('.ini'):
            systemName = inode[:inode.rfind('.ini')]
            if systemName != '':
                systemNames.append(systemName)
    systemNames.sort()
    return systemNames

def processOptions(request, errlog, mandatoryList, optionalList):
    status = OK
    config = {}
    for item in mandatoryList:
        if request.args.get(item) == None:
            request.write(err400(request, errlog, "BAD REQUEST", "Must provide %s\n" % item))
            request.finish()
            return FAIL, config
        else:
            config[item] = request.args.get(item)[0]
    for item in optionalList:
        if request.args.get(item) == None:
            config[item] = None
        else:
            config[item] = request.args.get(item)[0]
    return OK, config


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

def serviceConfigPut(path, config, args={}):
    datafile = StringIO.StringIO()
    config.write(datafile)
    datafile.seek(0)
    outputString = datafile.read()
    return servicePut(path, args, data=outputString)

def servicePut(path, args={}, data=[], live = False):
    queryString = urllib.urlencode(args)
    return put(SERVICE+path+"?"+queryString, data, live=live)

def put(location, data, username='', password='', live = False, logger=None):
    urldata = urlparse.urlparse(location)
    proto, site = urldata[:2]
    if len(urldata) >= 3: path = urldata[2]
    else: path=''
    if len(urldata) >= 4: port = urldata[3]
    else: port = ""
    if len(urldata) >= 5: variables = urldata[4]
    else: variables = ""
    h = httplib.HTTP(site)
    if logger: logger.debug( 'PUT', path+"/"+variables )
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
    if logger: logger.debug( 'Getting reply...' )
    try:
        errcode, errmsg, headers = h.getreply()
    except: # would be more specific, but it just throws an "error"
        if logger: logger.warning( "Error performing PUT" )
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
def makeConfigObject(data, logger=None):
    config = ConfigParser.ConfigParser()
    for section in data.keys():
        if not config.has_section(section):
            config.add_section(section)
        datum = data[section]
        if type(datum) == type(dict()):
            for option in datum.keys():
                value = datum[option]
                if type(value) != type(dict()):
                    config.set(section, option, value)
                else:
                    ermsg =  "incompatible types (ini/yaml) for (%s:%s)" % (section, option)
                    if logger: logger.error(ermsg)
        else:
            if logger: logger.error( "incompatible types (ini/yaml) for (%s)" % (section) )
    return config

def configToYaml(configPath):
    config = ConfigParser.ConfigParser()
    config.read(configPath)
    outputDict = {}
    for sectionName in config.sections():
        outputDict[sectionName] = {}
        for optionName in config.options(sectionName):
            outputDict[sectionName][optionName] = config.get(sectionName, optionName)
    return yaml.dump(outputDict)
    

def serviceYamlRequest(path, logger, args={}):
    try:
        ymlData = serviceRequest(path, args)
        config = yaml.load(string.join(ymlData, '\n'))
    except urllib2.HTTPError:
        logger.error( "Unable to connect to the service %s" % path )
        return {}
    try:
        return config.next() # FIXME
    except:
        logger.error( "error connecting to %s" % path )
        logger.error( "Received bad YAML from server: %s" % ymlData )
        return {}

def serviceConfigRequest(path, args={}):
    config = ConfigParser.ConfigParser()
    try:
        configData = serviceRequest(path, args)
        # need to trick ConfigParser
        dataFile = StringIO.StringIO(string.join(configData, '\n'))
        config.readfp(dataFile)
        dataFile.close()
    except urllib2.HTTPError:
        pass
    return config

def serviceRequest(path, args={}):
    queryString = ""
    if len(args.keys()) >= 1:
        for argIndex in range(0, len(args.keys())):
            key = args.keys()[argIndex]
            value = args[args.keys()[argIndex]]
            if argIndex == 0:
                queryString += "?%s=%s" % (key, value)
            else:
                queryString += "&%s=%s" % (key, value)
    print "connecting to %s" % SERVICE+path+queryString
    return wget(SERVICE+path+queryString).split('\n')

def wget(location, username='', password='', logger=None):
    if username != '' and password != '':
        auth_handler.add_password('Restricted', location, username, password)
        opener = urllib2.build_opener(auth_handler)
        urllib2.install_opener(opener)
    if logger: logger.debug( "Making service connection to: %s" % location )
    try:
        urlHandle = urllib2.urlopen(location)
    except urllib2.HTTPError, e:
        if logger: logger.error( "HTTP Error: %s" % e )
        return ""
    data = urlHandle.read()
    return data

def err400(request, logger, errorString="Bad Request", instructions=""):
    if instructions == '':
        instructions = errorString
    request.setResponseCode(400, errorString+"\n")
    request.setHeader('Connection', 'close')
    logger.warning( "ERR400 (%s) from (%s/%s)" % \
                    (errorString, request.getClient(), request.getClientIP() ))
    return instructions

def err404(request, logger, filePath=''):
    request.setResponseCode(404, 'Path Not Found')
    request.setHeader('Connection', 'close')
    logger.warning( "BAD FILENAME: %s (from %s/%s)" % \
                  (filePath, request.getClient(), request.getClientIP() ) )
    return '<h1>404 Path not found</h1>'    

def err500(request, logger):
    request.setResponseCode(500, "Access Denied")
    request.setHeader("Connection", 'close')
    logger.warning( "ACCESS DENIED: %s to (%s/%s)" % \
                               (request.path, request.getClient(), request.getClientIP() ) )
    return "<h1>500 Access denied</h1>"

def err301(request, logger, location):
    request.setResponseCode(301, "Moved Permanently")
    request.setHeader("Location", location)
    request.setHeader("Connection", 'close')
    logger.warning( "Redirect: %s to %s" % \
                    (request.getClient(), location ) )
    return "<h1>301 Moved permanently to %s</h1>" % location

# FIXME: Copy-and-paste from utility
def getPackageGroups(config, type=INI):
    packageGroups = []
    if type == INI:
        groupNumber = 0
        while 1 == 1:
            try:
                groupStr = "group"+`groupNumber`
                group = config.get("packageGroups", groupStr)
                packageGroups.append(group)
                groupNumber += 1
            except ConfigParser.NoSectionError:
                break
            except ConfigParser.NoOptionError:
                break
        if packageGroups == []:
            try:
                type = config.get("system", "type")
            except ConfigParser.NoOptionError:
                return []
            except ConfigParser.NoSectionError:
                return []
            packageGroups = ["base", type]
    else:
        configData = yaml.load(config).next()
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
        

