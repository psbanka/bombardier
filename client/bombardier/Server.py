#!/cygdrive/c/Python24/python.exe

# Server.py: This module is used primarily to abstract all
# functionality that is needed to interact with an http server. It is
# written in a similar vein to Filesystem.py, so that it can be
# mocked and unit-tests simplified.

# Copyright (C) 2005 Peter Banka

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

import os, StringIO, md5, urlparse, time, yaml, socket
import pycurl

import Exceptions, miniUtility, Logger
from staticData import *

def postServerMessage(content_type, body, url):
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
        Logger.error(errmsg)
        return FAIL
    res = h.getresponse()
    status = res.status
    if status >= 200 and status < 300:
        return OK
    else:
        errstr = "Cannot post because of %s to %s" % (res.reason, url)
        Logger.error(errstr)
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
    c.setopt(pycurl.FOLLOWLOCATION, 1)
    c.setopt(pycurl.MAXREDIRS, 4)
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

    def __init__(self, filesystem=None, serverData = None):
        self.filesystem = filesystem
        self.serverData = serverData
        self.cache      = {}

    def clearCache(self):
        self.cache = {}

    def getServerData(self):
        serverDataPath = os.path.join(miniUtility.getSpkgPath(), SERVERDATA_FILE)
        try:
            serverDataDirectory = self.filesystem.loadYaml(serverDataPath)
        except:
            Logger.error("The %s file is corrupt" % serverDataPath)
            return
        if not serverDataDirectory:
            Logger.error("The %s file is empty" % serverDataPath)
            return
        if len(serverDataDirectory) == 1:
            serverName = serverDataDirectory.keys()[0]
            Logger.info("Using server %s because it is the only one" % serverName)
            self.serverData = serverDataDirectory[serverName]
            return
        try:
            preferred = serverDataDirectory.get("~Preferred")
        except:
            Logger.error("The %s file is not a dictionary" % serverDataPath)
            return
        if preferred:
            Logger.info("Using server %s because was chosen before" % preferred)
            self.serverData = serverDataDirectory[preferred]
            return
        for serverDataName in serverDataDirectory.keys():
            self.serverData = serverDataDirectory[serverDataName]
            try:
                data = self.serviceRequest("pkggroups", timeout=2)
            except Exceptions.ServerUnavailable:
                ermsg = "server %s is not a good choice" % self.serverData["address"]
                Logger.debug(ermsg)
                continue
            if data:
                self.addPreferred(serverDataDirectory, serverDataName)
                return
        errmsg = "No servers in the %s file were reachable" % serverDataPath
        raise Exceptions.ServerUnavailable("unknown", errmsg)

    def addPreferred(self, serverDataDirectory, serverDataName):
        filePath = os.path.join(miniUtility.getSpkgPath(), SERVERDATA_FILE)
        serverDataDirectory["~Preferred"] = serverDataName
        fh = self.filesystem.open(filePath, 'w')
        yamlString = yaml.dump(serverDataDirectory)
        Logger.info("Setting %s as the preferred server." % serverDataName)
        fh.write(yamlString)
        fh.flush()
        fh.close()

    def serviceRequest(self, path, args={}, putData=None, debug=False,
                       putFile=None, timeout=30, legacyPathFix=False, verbose=False):
        if self.serverData == None:
            self.getServerData()
        if type(self.serverData) == type(None):
            Logger.error("Attempting to connect to an unconfigured server")
            raise Exceptions.ServerUnavailable, (path, "server not configured")
        base = self.serverData.get("base")
        if base == None:
            base = "/"
        if legacyPathFix:
            path = "website/service/"+path+"/"
        fullPath = self.serverData["address"] + "/" + base + path
        queryString = makeQueryString(args)
        url = fullPath+queryString
        if putFile or putData:
            return self.interact(debug, url, timeout, verbose, putData, putFile)
        if path not in self.cache.keys():
            output = self.interact(debug, url, timeout, verbose, putData, putFile)
            self.cache[path] = output
            return output
        else:
            return self.cache[path]

    def interact(self, debug, url, timeout, verbose, putData, putFile):
        if debug:
            Logger.debug("Performing service request to %s" % url)
        try:
            c = prepareCurlObject(url, self.serverData)
        except pycurl.error, e:
            raise Exceptions.ServerUnavailable, (url, `e`)
        data   = StringIO.StringIO()
        header = StringIO.StringIO()
        c.setopt(pycurl.WRITEFUNCTION, data.write)
        c.setopt(pycurl.CONNECTTIMEOUT, timeout)
        c.setopt(pycurl.FOLLOWLOCATION, 1)
        c.setopt(pycurl.MAXREDIRS, 4)
        c.setopt(pycurl.HEADERFUNCTION, header.write)
        if verbose:
            c.setopt(pycurl.VERBOSE, 1)
        if putData:
            putFile = StringIO.StringIO(putData)
            c.setopt(c.INFILESIZE, len(putData))
        if putFile:
            c.setopt(pycurl.UPLOAD, 1)
            c.setopt(pycurl.READFUNCTION, putFile.read)
        try:
            c.perform()
            data.seek(0)
            header.seek(0)
            output = data.read()
            return output
        except pycurl.error, e:
            if e[0] == 22:
                raise Exceptions.FileNotFound(url)
            raise Exceptions.ServerUnavailable, (url, e[1])

    def serviceYamlRequest( self, path, args={}, putData=None, debug=False,
                            timeout=30, legacyPathFix=False, verbose=False):
        if putData != None:
            if type(putData) == type(["list"]) or type(putData) == type({}):
                putData = yaml.dump(putData)
        ymlData = self.serviceRequest(path, args, putData,
                                      debug=debug, timeout=timeout,
                                      legacyPathFix=legacyPathFix, verbose=verbose)
        if ymlData == '':
            return {}
        try:
            config = yaml.load(ymlData)
            return config.next()
        except:
            ermsg = "Received bad YAML: %s" % (repr(ymlData))
            raise Exceptions.ServerUnavailable, (path, ermsg)

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
                Logger.warning(ermsg)
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
                    ermsg = "Downloaded file %s has an invalid "\
                            "checksum (%s != %s)" % (filename, ccsum, checksum)
                    Logger.warning(ermsg)
                    continue
                else:
                    ermsg = "%s checksum [%s] verified." % (filename, checksum)
                    if checksum: Logger.info(ermsg)
                    break
            except pycurl.error, e:
                erstr = "Connection problem downloading from %s: %s" % (url, e[1])
                Logger.warning(erstr)
                retries -= 1
                continue
        if not retries:
            ermsg = "Giving up on %s -- too many retries" % filename
            raise Exceptions.ServerUnavailable, (url, ermsg)
        return self.filesystem.moveToDestination(destDir, filename)

    def wgetMultiple(self, path, filename, destDir, retries = 4,
                     checksumList = [], checksum='', abortIfTold=None):
        parts = {}
        for i in range(0, len(checksumList)):
            parts['%s.part%d' % (filename, i)] = checksumList[i]
        Logger.info("downloading %s in %s parts." % (filename, len(parts.keys())))
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

        hostname = os.environ["COMPUTERNAME"].lower()
        content_type, body = encode_multipart_formdata(
            [("time",int(time.time())),
            ("host",hostname),
            ("ip", socket.gethostbyname(socket.gethostname())),
            ("service",'BombardierVerify'),
            ("return",returnVal),
            ("message",message)])
        url = urlparse.urljoin(self.serverData["address"], "cgi-bin/log.pl")
        status = postServerMessage(content_type, body, url)
        return status
