#!/cygdrive/c/Python23/python.exe
import os, sys, ConfigParser, shutil, StringIO
import urllib2, md5, urlparse, string, time, gc
import yaml
import socket

from static import *

def safeGet(optionList, config):
    output = {}
    for optionSet in optionList:
        section, option = optionSet
        try:
            value = config.get(section, option)
            output[option] = value
        except ConfigParser.NoOptionError:
            output[option] = None
        except ConfigParser.NoSectionError:
            output[option] = None
    return output

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

def tryPatiently(action, verify, logger, errorMessage=None, retries = 100):
    succeeded = True
    while retries:
        if not eval(verify):
            try:
                eval(action)
                if succeeded == False:
                    logger.info("Successful trying to %s" % action)
                succeeded = True
                break
            except Exception, e:
                logger.warning(e)
                retries -= 1
                time.sleep(.5)
                if not errorMessage:
                    logger.warning("still trying to %s" % action)
                succeeded = False
        else:
            if succeeded == False:
                logger.info("Successful trying to %s" % action)
            succeeded = True
            break
    if succeeded: return OK
    return FAIL

def moveToDestination(destDir, logger, dieOnFail, config, filename):
    gc.collect() # File this under the category 'windows sucks': can't have open files! - pbanka
    for x in gc.garbage:
        s = str(x)
    if destDir == '':
        return OK
    if not os.path.isdir(destDir):
        if logger: logger.error("Destination directory %s does not exist" % destDir)
        if dieOnFail:
            hang(FAIL, config)
        return FAIL
    elif os.getcwd().upper() != destDir.upper():
        try:
            destPath = os.path.join(destDir, filename)
            status1 = tryPatiently ("os.unlink(r'%s')" % destPath,
                                   "os.path.isfile(r'%s') == False" % destPath, logger)
            status2 = tryPatiently ("shutil.copy(r'%s', r'%s')" % (filename, destPath),
                                   "os.path.isfile(r'%s') == True" % destPath, logger)
            status3 = tryPatiently ("os.unlink(r'%s')" % filename,
                                   "os.path.isfile(r'%s') == False" % filename, logger)
            if (status1 or status2) == FAIL:
                return FAIL
        except IOError, e:
            if logger:
                logger.error("Problem moving %s to %s" % (filename, destPath))
                logger.error(e)
            return FAIL
        except OSError, e:
            if logger:
                logger.error("Problem moving %s to %s" % (filename, destPath))
                logger.error(e)
            return FAIL          
    return OK

def prepareCurlObject(url, repository):
    import pycurl
    c = pycurl.Curl()
    c.setopt(pycurl.URL, url)
    c.setopt(pycurl.SSL_VERIFYPEER, 0)
    c.setopt(pycurl.FAILONERROR, 1)
    if repository.has_key("username") and repository.has_key("password"):
        c.setopt(pycurl.USERPWD, "%s:%s" % (repository["username"],repository["password"]))
        #c.setopt(pycurl.HTTPAUTH, pycurl.HTTPAUTH_ANYSAFE)
        c.setopt(pycurl.HTTPAUTH, pycurl.HTTPAUTH_BASIC)
    if repository.has_key("proxy") and repository["proxy"]["address"] != "None":
        c.setopt(pycurl.PROXY, repository["proxy"].get("address"))
        if repository["proxy"].has_key("port") and repository["proxy"]["port"] != None:
            try:
                c.setopt(pycurl.PROXYPORT, int(repository["proxy"].get("port")))
            except ValueError:
                pass
        # FIXME: use proxyusername and password
    return c

def concatenateFiles(filename, destDir, parts):
    fh = open(os.path.join(destDir, filename), 'wb')
    keyList = parts.keys()
    keyList.sort(lambda x,y: cmp(int(x.split('part')[-1]), int(y.split('part')[-1]))) # complicated sort
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

def wgetMultiple(path, filename, config, logger, destDir, retries = 4, checksumList = [], checksum=''):
    parts = {}
    for i in range(0, len(checksumList)):
        parts['%s.part%d' % (filename, i)] = checksumList[i]
    logger.info("downloading %s in %s parts." % (filename, len(parts.keys())))
    basefile = filename[:filename.rfind('.spkg')]
    path = "%s/%s/" % (path, basefile)
    for partName in parts.keys():
        checksum = parts[partName]
        status = wget(path, partName, config, logger, destDir=destDir,
                      retries=retries, checksum=checksum)
        if status == FAIL:
            return FAIL
    concatenateFiles(filename, destDir, parts)
    return OK

#^ MOSTLY COPIED FROM WEBUTIL.PY
def serviceYamlRequest(path, repository, logger, args={}, putData=None, debug=False):
    try:
        if putData:
            if type(putData) == type(["list"]) or type(putData) == type({}):
                putData = yaml.dump(putData)
        ymlData = serviceRequest(path, repository, logger, args, putData, debug=debug)
        config = yaml.load(ymlData)
    except urllib2.HTTPError:
        logger.error("Unable to connect to the service %s" % path)
        return {}
    try:
        return config.next() # FIXME
    except:
        logger.error("error communicating with web service '%s'" % path)
        logger.error("Received bad YAML from server: \n%s" % repr(ymlData))
        return {}

# ^^ UNTESTED
def serviceConfigRequest(path, repository, logger, args={}):
    config = ConfigParser.ConfigParser()
    configData = serviceRequest(path, repository, logger, args)
    # need to trick ConfigParser
    dataFile = StringIO.StringIO(configData)
    config.readfp(dataFile)
    dataFile.close()
    return config

def makeQueryString(args):
    queryString = ""
    if len(args.keys()) >= 1:
        for argIndex in range(0, len(args.keys())):
            key = args.keys()[argIndex]
            value = args[args.keys()[argIndex]]
            if argIndex == 0:
                queryString += "?%s=%s" % (key, value)
            else:
                queryString += "&%s=%s" % (key, value)
    return queryString

# ^^ UNTESTED
def serviceRequest(path, repository, logger, args={}, putData=None, debug=False):
    import pycurl
    fullPath = urlparse.urljoin(repository["address"], "website/service/"+path)
    queryString = makeQueryString(args)
    url = urlparse.urljoin(fullPath, queryString)
    if debug:
        logger.debug("Performing service request to %s" % url)
    c = prepareCurlObject(url, repository)
    data = StringIO.StringIO()
    c.setopt(pycurl.WRITEFUNCTION, data.write)
    if putData:
        c.setopt(pycurl.UPLOAD, 1)
        c.setopt(c.INFILESIZE, len(putData))
        outData = StringIO.StringIO(putData)
        c.setopt(pycurl.READFUNCTION, outData.read)
    try:
        c.perform()
        data.seek(0)
        output = data.read()
        return output
    except pycurl.error, e:
        erstr = "Connection problem downloading from %s: %s" % (url, e[1])
        if logger: logger.warning(erstr)
        return ''


def wget(path, filename, config, logger, dieOnFail=0, debug=0,
         destDir='', retries = 4, checksum=''):
    source = config.repository["address"]+"/"+path+"/"
    try:
        import pycurl
    except ImportError, e:
        if logger: logger.info("No Pycurl -- REFUSING to use urllib2...")
        return FAIL
    url = urlparse.urljoin(source, filename)
    while retries:
        c = prepareCurlObject(url, config.repository)
        try:
            fileHandle = open(filename, 'wb')
            c.setopt(pycurl.WRITEFUNCTION, fileHandle.write)
        except:
            if logger: logger.warning("Unable to open file %s for writing" % filename)
            if dieOnFail:
                sys.exit(1)
            else:
                return FAIL
        try:
            c.perform()
            fileHandle.flush()
            fileHandle.close()
            del fileHandle # necessary because the file may need to be moved before gc. -pbanka
            status, ccsum = computeMd5(filename, checksum)
            if status == FAIL:
                retries -= 1
                if logger: logger.warning("Downloaded file %s has an invalid"\
                                          "checksum (%s != %s)" % (filename, ccsum, checksum))
                continue
            else:
                if checksum: logger.info("%s checksum [%s] verified." % (filename, checksum))
                break
        except pycurl.error, e:
            erstr = "Connection problem downloading from %s: %s" % (url, e[1])
            if logger: logger.warning(erstr)
            retries -= 1
            continue
    if not retries:
        logger.error("Giving up on %s -- too many retries" % filename)
        return FAIL
    return moveToDestination(destDir, logger, dieOnFail, config, filename)

### TESTED, but lacking.
def wgetOld(source, filename, config, logger, dieOnFail = 0,
         debug = 0, destDir = '', retries = 2, checksum=''):
    # FIXME : Should have an option to return a file handle
    # FIXME : Use etags
    auth_handler = urllib2.HTTPBasicAuthHandler()
    location = urlparse.urlparse(source)[1]

    if not source.endswith('/'):
        source += '/'
    while retries:
        mchk = md5.new()
        try:
            urlHandle = urllib2.urlopen(source+filename)
            data = 1
            try:
                fileHandle = open(filename, 'wb')
            except:
                if logger: logger.warning("Unable to open file %s" % filename)
                if dieOnFail:
                    sys.exit(1)
                else:
                    return FAIL
            while data:
                try:
                    data = urlHandle.read(10000)
                    mchk.update(data)
                except:
                    fileHandle.write(data)
                    break
                fileHandle.write(data)
            urlHandle.close()
            fileHandle.flush()
            fileHandle.close()
        except socket.error, e:
            erstr = "Connection problem downloading from %s" % source
            if logger: logger.warning(erstr)
            if logger: logger.debug("details: %s " % `e`)
            retries -= 1
            continue
        except urllib2.URLError, e:
            erstr = "Connection error to %s" % source
            if logger: logger.warning(erstr)
            if hasattr(e, "msg") and hasattr(e, "code"):
                if logger: logger.debug("details: %s/%s" % (e.code, e.msg))
            if dieOnFail:
                hang(FAIL, config=config)
            else:
                return FAIL
        except urllib2.HTTPError, e:
            if e.code == 401:
                logger.error("Repository requires authorization")
                return FAIL
            erstr = "File %s does not exist "\
                    "on repository %s." % (filename, source)
            if logger: logger.warning(erstr)
            if logger: logger.debug("Error: %s (%s)" % (`e.code`, e.msg))
            if dieOnFail:
                hang(FAIL, config)
            else:
                return FAIL
        computedMd5 = mchk.hexdigest()
        if checksum:
            if computedMd5 != checksum:
                logger.warning("Downloaded file %s has an invalid checksum "\
                               "(%s != %s)" % (filename, computedMd5, checksum))
                retries -= 1
                continue
            else:
                logger.info("Checksum %s verified." % checksum)
                break
        break
    if debug:
        erstr = "Downloaded %s." % filename
        if logger: logger.debug(erstr)
    return moveToDestination(destDir, logger, dieOnFail, config, filename)


class DefaultErrorHandler(urllib2.HTTPDefaultErrorHandler):
    def http_error_default(self, req, fp, code, msg, headers):
        result = urllib2.HTTPError(req.get_full_url(),
                                   code, msg, headers, fp)
        result.status = code
        return result

def ul2Directory(url, logger):
    request = urllib2.Request(url)
    opener = urllib2.build_opener(DefaultErrorHandler()) # FIXME: This LEAKS. Fixed in Python2.4?
    try:
        dataStream = opener.open(request)
    except urllib2.URLError:
        if logger: logger.error("Repository is offline or unavailable")
        return ''
    data = dataStream.read()
    return data

def pycurlDirectory(url, logger, config):
    import pycurl
    c = prepareCurlObject(url, config)
    b = StringIO.StringIO()
    c.setopt(pycurl.WRITEFUNCTION, b.write)
    try:
        c.perform()
    except pycurl.error, e:
        erstr = "Connection problem downloading from %s: %s" % (url, e[1])
        if logger: logger.warning(erstr)
        return ''
    return b.getvalue()

### TESTED
def wgetCaseless(url, name, config, logger, destDir = "", debug = 0):

    " CASE-insensitive downloader; TESTED WITH APACHE 2 and twisted/bombardier"

    if not url.endswith('/'):
        url += '/'
    try:
        import pycurl
        data = pycurlDirectory(url, logger, config)
    except ImportError:
        data = ul2Directory(url, logger)
    for line in data.split('\n'):
        if line.upper().rfind(name.upper()) != -1:
            fname = ''
            for entry in line.split('"'):
                if entry.upper() == name.upper():
                    status = wget(url, entry, config, logger,
                                  dieOnFail=0,
                                  destDir = destDir,
                                  debug=debug)
                    opener = None
                    return status
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
        L.append(value)
    L.append('--' + BOUNDARY + '--')
    L.append('')
    body = CRLF.join(L)
    content_type = 'multipart/form-data; boundary=%s' % BOUNDARY
    return content_type, body

def serverLog(severity, message, config, logger=None, section="GENERAL"):
    hostname = os.environ["COMPUTERNAME"]
    #^ FIXME: proxy, etc.
    url = urlparse.urljoin(config.repository["address"],
                           "website/service/clientlog?client=%s" % (hostname))
    status = postServerMessage(severity, message, url, logger, section)
    return status

## TESTED
def postServerMessage(severity, message, url, logger=None, section = "GENERAL"):
    " Author: Wade Leftwich, modifications by Peter Banka "
    import httplib
    u = urlparse.urlparse(url)
    host, path = u[1], u[2]
    content_type, body = encode_multipart_formdata([("severity",severity),
                                                    ("message",message),
                                                    ("section",section),
                                                    ("version",VERSION)])
    headers = {
        'User-Agent': 'Bombardier Client System',
        'Content-Type': content_type
        }
    try:
        h = httplib.HTTPConnection(host)
        h.request('POST', url, body, headers)
    except socket.error, e:
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
