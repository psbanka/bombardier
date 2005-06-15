import os, StringIO
import md5, urlparse, time
import yaml
import socket
import Exceptions
import pycurl

import miniUtility
from staticData import *

def postServerMessage(content_type, body, url, logger=None):
    """ 
    Author: Wade Leftwich, 
    Modifications by Peter Banka.
    Further Modifications Mark Hickman
    """
    import httplib
    u = urlparse.urlparse(url)
    host = u[1]

    headers = {
        'User-Agent': 'Bombardier Client System',
        'Content-Type': content_type
        }
    try:
        h = httplib.HTTPConnection(host)
        h.request('POST', url, body, headers)
    except socket.error:
        errmsg = "Unable to connect to server"
        if logger:
            logger.error(errmsg)
        return FAIL
    res = h.getresponse()
    status = res.status
    if status >= 200 and status < 300:
        return OK
    else:
        errstr = "Cannot post because of %s" % res.reason
        if logger:
            logger.error(errstr)
        else:
            print errstr
    return FAIL

## TESTED
def encode_multipart_formdata(fields):    
    """
    fields is a sequence of (name, value) elements for regular form fields.
    Return (content_type, body) ready for httplib.HTTP instance
    Author: Wade Leftwich
    """
    BOUNDARY = '----------ThIs_Is_tHe_bouNdaRY_$'
    CRLF = '\r\n'
    L = []
    for (key, value) in fields:
        L.append('--' + BOUNDARY)
        L.append('Content-Disposition: form-data; name="%s"' % key)
        L.append('')
        L.append(str(value))
    L.append('--' + BOUNDARY + '--')
    L.append('')
    body = CRLF.join(L)
    content_type = 'multipart/form-data; boundary=%s' % BOUNDARY
    return content_type, body

def computeMd5(filename, checksum):
    if not checksum:
        return OK, ''
    mchk = md5.new()
    fileHandle = open(filename, 'rb')
    data = fileHandle.read(BLOCK_SIZE)
    while data:
        mchk.update(data)
        data = fileHandle.read(BLOCK_SIZE)
    fileHandle.flush()
    fileHandle.close()
    del fileHandle # necessary because the file may need to be moved before gc. -pbanka
    computedMd5 = mchk.hexdigest()
    if computedMd5 != checksum:
        return FAIL, computedMd5
    return OK, computedMd5

def prepareCurlObject(url, serverData):
    c = pycurl.Curl()
    c.setopt(pycurl.URL, url)
    c.setopt(pycurl.SSL_VERIFYPEER, 0)
    c.setopt(pycurl.SSL_VERIFYHOST, 0)
    c.setopt(pycurl.FAILONERROR, 1)
    if serverData.has_key("username") and serverData.has_key("password"):
        c.setopt(pycurl.USERPWD, "%s:%s" % (serverData["username"],serverData["password"]))
        #c.setopt(pycurl.HTTPAUTH, pycurl.HTTPAUTH_ANYSAFE)
        c.setopt(pycurl.HTTPAUTH, pycurl.HTTPAUTH_BASIC)
    if serverData.has_key("proxy") and serverData["proxy"]["address"] != "None":
        c.setopt(pycurl.PROXY, serverData["proxy"].get("address"))
        if serverData["proxy"].has_key("port") and serverData["proxy"]["port"] != None:
            try:
                c.setopt(pycurl.PROXYPORT, int(serverData["proxy"].get("port")))
            except ValueError:
                pass
        # FIXME: use proxyusername and password
    return c

def concatenateFiles(filename, destDir, parts):
    fh = open(os.path.join(destDir, filename), 'wb')
    keyList = parts.keys()
    keyList.sort(lambda x,y: cmp(int(x.split('part')[-1]),
                                 int(y.split('part')[-1]))) # complicated sort
    for partFilename in keyList:
        partPath = os.path.join(destDir, partFilename)
        fhp = open(partPath, 'rb')
        data = 'initialize'
        while data:
            data = fhp.read(BLOCK_SIZE)
            fh.write(data)
        fhp.close()
        os.unlink(partPath)
    fh.close()

def makeQueryString(args):
    queryString = ""
    if not type(args) == type({}):
        return queryString
    if len(args.keys()) >= 1:
        for argIndex in range(0, len(args.keys())):
            key = args.keys()[argIndex]
            value = args[args.keys()[argIndex]]
            if argIndex == 0:
                queryString += "?%s=%s" % (key, value)
            else:
                queryString += "&%s=%s" % (key, value)
    return queryString


class Server:

    """The purpose of this class is to provide an abstraction layer
    around all those activities that require interaction with a webserver.
    Probably the whole webops module aught to be refactored into this
    class."""

    def __init__(self, filesystem=None, logger=None, serverData = None):
        self.filesystem = filesystem
        self.logger     = logger
        self.serverData = serverData

    # this method is stupid. Should be refactored to just attempt to connect to each. -pbanka
    def getServerData(self):
        fallbackServerData = {"address":SERVERDATA_FALLBACK}
        serverDataPath = os.path.join(miniUtility.getSpkgPath(), SERVERDATA_FILE)
        try:
            serverDataDirectory = self.filesystem.loadYaml(serverDataPath)
        except:
            errmsg = "Unable to determine serverData: Cannot read file %s" % serverDataPath
            self.logger.critical(errmsg)
            self.logger.warning("using %s as a fall-back" % SERVERDATA_FALLBACK)
            self.serverData = fallbackServerData
            return
        if not serverDataDirectory:
            self.serverData = fallbackServerData
            return
        if len(serverDataDirectory) == 1:
            self.serverData = serverDataDirectory[serverDataDirectory.keys()[0]]
            return
        try:
            preferred = serverDataDirectory.get("~Preferred")
        except:
            self.logger.critical("Invalid serverData data. Using fallback.")
            self.serverData = fallbackServerData
            return
        if preferred:
            self.serverData = serverDataDirectory[preferred]
            return
        for serverDataName in serverDataDirectory.keys():
            self.serverData = serverDataDirectory[serverDataName]
            try:
                data = self.serviceRequest("pkggroups", timeout=2)
            except Exceptions.ServerUnavailable:
                ermsg = "server %s is not a good choice" % self.serverData["address"]
                self.logger.debug(ermsg)
                continue
            if data:
                self.addPreferred(serverDataDirectory, serverDataName)
                return
        self.logger.warning("using %s as a fall-back" % SERVERDATA_FALLBACK)
        self.serverData = fallbackServerData
        return

    def addPreferred(self, serverDataDirectory, serverDataName):
        filePath = os.path.join(miniUtility.getSpkgPath(), SERVERDATA_FILE)
        serverDataDirectory["~Preferred"] = serverDataName
        fh = self.filesystem.open(filePath, 'w')
        yamlString = yaml.dump(serverDataDirectory)
        fh.write(yamlString)
        fh.flush()
        fh.close()

    def serviceRequest(self, path, args={}, putData=None, debug=False,
                       putFile=None, timeout=30):
        if self.serverData == None:
            self.getServerData()
        base = self.serverData.get("base")
        if base == None:
            base = "website/service/"
        fullPath = self.serverData["address"] + "/" + base + path
        queryString = makeQueryString(args)
        url = fullPath+queryString
        if debug:
            self.logger.debug("Performing service request to %s" % url)
        try:
            c = prepareCurlObject(url, self.serverData)
        except pycurl.error, e:
            raise Exceptions.ServerUnavailable, (url, e[1])
        data = StringIO.StringIO()
        c.setopt(pycurl.WRITEFUNCTION, data.write)
        c.setopt(pycurl.CONNECTTIMEOUT, timeout)
        if putData:
            putFile = StringIO.StringIO(putData)
            c.setopt(c.INFILESIZE, len(putData))
        if putFile:
            c.setopt(pycurl.UPLOAD, 1)
            c.setopt(pycurl.READFUNCTION, putFile.read)
        try:
            c.perform()
            data.seek(0)
            output = data.read()
            return output
        except pycurl.error, e:
            raise Exceptions.ServerUnavailable, (url, e[1])

    def serviceYamlRequest( self, path, args={}, putData=None, debug=False, timeout=30):
        if putData:
            if type(putData) == type(["list"]) or type(putData) == type({}):
                putData = yaml.dump(putData)
        ymlData = self.serviceRequest(path, args, putData,
                                      debug=debug, timeout=timeout)
        if ymlData == '':
            return {}
        config = yaml.load(ymlData)
        try:
            return config.next()
        except:
            ermsg = "Received bad YAML: %s" % (repr(ymlData))
            raise Exceptions.ServerUnavailable, (path, ermsg)

    def serverLog( self, severity, message, section = "GENERAL"):
        try:
            hostname = os.environ["COMPUTERNAME"]
            args = {"client":hostname, "severity":severity, "section":section}
            response = self.serviceRequest("clientlog", args, message, debug=True)
            if response.strip() == "OK":
                self.logger.info("Message severity %s delivered to server" % severity)
                return OK
        except Exceptions.ServerUnavailable, e:
            ermsg = "Connection problem delivering "\
                    "severity %s message to server (%s)" % (severity, e)
            self.logger.warning(ermsg)
        self.logger.warning("Unable to deliver status messages to the server")
        return FAIL

    def wget(self, path, filename, destDir = '', retries = 4,
             checksum = '', abortIfTold=None):
        source = self.serverData["address"]+"/"+path+"/"
        url = urlparse.urljoin(source, filename)
        while retries:
            if abortIfTold != None:
                abortIfTold()
            c = prepareCurlObject(url, self.serverData)
            try:
                fileHandle = self.filesystem.open(filename, 'wb')
                c.setopt(pycurl.WRITEFUNCTION, fileHandle.write)
            except:
                ermsg = "Unable to open file %s for writing" % filename
                self.logger.warning(ermsg)
                return FAIL
            try:
                c.perform()
                fileHandle.flush()
                fileHandle.close()
                del fileHandle # necessary because the file may
                               # need to be moved before gc. -pbanka
                status, ccsum = computeMd5(filename, checksum)
                if status == FAIL:
                    retries -= 1
                    ermsg = "Downloaded file %s has an invalid"\
                            "checksum (%s != %s)" % (filename, ccsum, checksum)
                    self.logger.warning(ermsg)
                    continue
                else:
                    ermsg = "%s checksum [%s] verified." % (filename, checksum)
                    if checksum: self.logger.info(ermsg)
                    break
            except pycurl.error, e:
                erstr = "Connection problem downloading from %s: %s" % (url, e[1])
                self.logger.warning(erstr)
                retries -= 1
                continue
        if not retries:
            ermsg = "Giving up on %s -- too many retries" % filename
            raise Exceptions.ServerUnavailable, (url, ermsg)
        return self.filesystem.moveToDestination(destDir, self.logger, filename)

    def wgetMultiple(self, path, filename, destDir, retries = 4,
                     checksumList = [], checksum='', abortIfTold=None):
        parts = {}
        for i in range(0, len(checksumList)):
            parts['%s.part%d' % (filename, i)] = checksumList[i]
        self.logger.info("downloading %s in %s parts." % (filename, len(parts.keys())))
        basefile = filename[:filename.rfind('.spkg')]
        path = "%s/%s/" % (path, basefile)
        for partName in parts.keys():
            if abortIfTold != None:
                abortIfTold()
            checksum = parts[partName]
            status = self.wget(path, partName, destDir=destDir,
                               retries=retries, checksum=checksum)
            if status == FAIL:
                return FAIL
        concatenateFiles(filename, destDir, parts)
        return OK

    def nagiosLog(self, status = OK, data = {}):
        message = "Everything OK"
        if status == OK:
            returnVal = 0
        else:
            returnVal = 2 # assume critical

        badData = filter(lambda x: data[x] == FAIL, data.keys())
        if len(badData) > 0: 
            message = "Verify Failed: " + ','.join(badData)

        hostname = os.environ["COMPUTERNAME"]
        content_type, body = encode_multipart_formdata(
            [("time",int(time.time())),
            ("host",hostname),
            ("ip", socket.gethostbyname(socket.gethostname())),
            ("service",'BombardierVerify'),
            ("return",returnVal),
            ("message",message)])
        url = urlparse.urljoin(NAGIOS_HOST, "cgi-bin/log.pl")
        status = postServerMessage(content_type, body, url, self.logger)
        return status
