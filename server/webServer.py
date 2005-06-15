#!/cygdrive/c/Python24/python
from twisted.web import server, resource, static, error
from twisted.protocols import http
from twisted.internet import reactor
import threading, sys, os, shelve, md5, time, urlparse
import string, logging
import logging.handlers
import ConfigParser
import website
import webUtil

from static import *

BLOCK_SIZE   = 10000
MD5SUM       = "md5sum"
DATE         = "date"

DEBUG = 1

def pageList(website):
    output = []
    for page in website:
        if page.has_key('link'):
            output.append(page['link'])
        else:
            output.append(page['name'])
    return output

class UploadThread(threading.Thread):
    
    def __init__(self, request, filename, logger, recordInfo = False):
        threading.Thread.__init__(self)
        self.request    = request
        self.filename   = filename
        self.recordInfo = recordInfo
        self.logger     = logger
        self.logger.info( "delivering file ... %s" % filename )
        self.logger.info( "Content-Length %s/%s" % ( os.stat(filename)[6], filename ) )
        request.setHeader("Content-Length", os.stat(filename)[6])
        
    def run(self):
        m = md5.new()
        try:
            fh = open(self.filename, 'rb')
        except:
            self.request.setResponseCode(404, 'Access denied: %s' % filename)
            self.request.setHeader('Connection', 'close')
            self.request.write('<h1>404 Access Denied %s </h1>' % filename)
            self.request.finish()
            return
        newData = '0'
        while newData:
            try:
                newData = fh.read(BLOCK_SIZE)
                if self.recordInfo:
                    m.update(newData)
            except:
                self.request.finish()
                return
            self.request.write(newData)
        self.request.finish()
        if self.recordInfo:
            md5sum   = m.hexdigest()
            fileInfo = os.stat(self.filename)
            mode, inode, device, hlinks, uid, gid, size, access, mod, creation = fileInfo
            filedate = time.strftime("%a, %d %b %Y %H:%m:%S GMT", time.gmtime(creation))
            if not os.path.isfile(PKGINFO_FILE):
                packageData = shelve.open(PKGINFO_FILE, 'c')
            else:
                packageData = shelve.open(PKGINFO_FILE, 'w')
            info = {MD5SUM: md5sum, DATE: filedate}
            packageData[self.filename] = info
            packageData.close()

def getDirectoryListing(directoryPath, logger):
    logger.info( "looking at files in %s" % directoryPath )
    files = os.listdir(directoryPath)
    files.sort()
    output = [HEADER]
    output.append("<HEAD><TITLE>Index of /%s</TITLE>" % directoryPath)
    output.append('<FONT size="+3" face="Helvetica,Arial,sans-serif">')
    output.append("<body><B>Index of %s</B><br></FONT>" % directoryPath)
    output.append('<HR noshade align="left" width="80%"><br>')
    output.append('<A HREF="/">Parent Directory</A><br>')
    files.sort()
    for file in files:
        if os.path.isdir(os.path.join(directoryPath, file)):
            output.append('<A HREF="%s/">%s/</A><br>' % (file, file))
        else:
            output.append('<A HREF="%s">%s</A><br>' % (file, file))
    output.append('''</PRE><HR noshade align="left" width="80%">
<ADDRESS>Twisted/1.3.31 Server at - </ADDRESS>
</BODY></HTML>''')
    return string.join(output, '\n')

#########################################################################
# Web-server proper

class WebSite(resource.Resource):
    isLeaf = True
    def __init__(self):
        resource.Resource.__init__(self)
        accessLogFile = config.get("site", "accesslog")
        errorLogFile  = config.get("site", "errorlog")
        logLevel = logging.DEBUG
        try:
                logLevelString  = config.get("site", "logginglevel")
                if logLevelString.upper() == "INFO":
                        logLevelString = logging.INFO
                elif logLevelString.upper() == "WARNING":
                        logLevelString = logging.WARNING
        except:
                pass
        if sys.platform == "win32":
            print "Note -- Windows does not have the ability to do rotating log handlers..."
            accessFileHandler = logging.FileHandler(accessLogFile)
            errorFileHandler  = logging.FileHandler(errorLogFile)
        else:
            accessFileHandler = logging.handlers.RotatingFileHandler(accessLogFile, 'a', 10000, 5)
            errorFileHandler  = logging.handlers.RotatingFileHandler(errorLogFile, 'a', 10000, 5)
        formatter = logging.Formatter('%(asctime)s|%(levelname)s|%(message)s')
        accessFileHandler.setFormatter(formatter)
        errorFileHandler.setFormatter(formatter)
        self.logger = logging.getLogger("AccessLog")
        self.logger.setLevel(logLevel)
        self.logger.addHandler(accessFileHandler)
        self.logger.info("===================================================")
        self.logger.info("Acces Log started:  %s" % time.strftime("%A, %b %c", time.localtime()))
        self.errlog = logging.getLogger("ErrorLog")
        self.errlog.setLevel(logLevel)
        self.errlog.addHandler(errorFileHandler)
        self.errlog.info("===================================================")
        self.errlog.info("Error Log started:  %s" % time.strftime("%A, %b %c", time.localtime()))

    def render_PUT(self, request):
        self.logger.info("PUT: %s" % request.path)
        self.user = request.getUser()
        self.password = request.getPassword()
##         if not self.user or not self.password:
##             return self.rejectInvalid(self, request)
        if self.user and self.password:
            self.logger.info("Authenticated access from %s / user %s" % (request.getClient(), self.user) )
        parts = request.path.split('/')
        if request.path.startswith('/'):
            request.path = request.path[1:]
        if request.postpath[0] == 'client' or request.postpath[1] == 'client':
            directory = string.join(request.postpath[:-1], '\\')
            if not os.path.isdir(directory):
                self.errlog.debug( "making directory %s" % directory )
                os.makedirs(directory)
            else:
                self.errlog.debug( "Directory %s exists, continuing..." % directory )
            fullPath = os.path.join(directory, request.postpath[-1])
            self.errlog.debug( "writing to: %s" % fullPath )
            data = request.content.read()
            open(fullPath, 'w').write(data)
            return "<html><body>OK</body></html>"
        if "website" in parts:
            function = self.findMethod(request.path,
                                       request, action="put")
            if type(function) == type("string"):
                return function
            if DEBUG:
                return function(request, self.logger, self.errlog)
            else:
                try:
                    return function(request, self.logger, self.errlog)
                except Exception, e:
                    self.logger.critical("Exception thrown in %s: %s" % (request.path, e))
                    self.errlog.critical("Exception thrown in %s: %s" % (request.path, e))
                self.errlog.critical("%s" % sys.exc_info()[-1].tb_lineno)
        else:
            return webUtil.err404(request, self.errlog, request.prepath)

    def render_POST(self, request):
        self.logger.info("POST: %s" % request.path)
        self.logger.info("GET: %s" % request.path)
        self.user = request.getUser()
        self.password = request.getPassword()
##         if not self.user or not self.password:
##             return self.rejectInvalid(self, request)
        if self.user and self.password:
            self.logger.info("Authenticated access from %s / user %s" % (request.getClient(), self.user) )
        path = request.path
        parts = path.split('/')
        if len(parts) >= 4:
            if parts[3] == "client":
                if len(parts) > 5:
                    fileName = os.path.join("client",parts[4], parts[5])
                    if os.path.isfile(fileName):
                        text = []
                        for argName in request.args.keys():
                            text.append("%s: %s" % (argName, request.args.get(argName)[0]))
                        open(fileName, 'a').write(string.join(text, '\r\n')+'\r\n')
                        request.setResponseCode(200, 'OK')
                        return "<html>OK</html>"
        if "website" in parts:
            function = self.findMethod(request.path,
                                       request, action="post")
            if type(function) == type("string"):
                return function
            try:
                return function(request, self.logger, self.errlog)
            except Exception, e:
                self.errlog.critical("Exception thrown in %s: %s" % (request.path, e))            
                self.errlog.critical("%s" % sys.exc_info()[-1].tb_lineno)
        else:
            return webUtil.err404(request, self.errlog, request.prepath)

    def rejectInvalid(self, request):
        request.setHeader('WWW-authenticate', 'Basic realm="%s"' % ("bombardier"))
        self.errlog.warning("rejected unauthenticated user from %s" % request.getClient())
        errpage = error.ErrorPage(http.UNAUTHORIZED,"Unauthorized","401 Authentication required")
        return errpage.render(request)

    def render_GET(self, request):
        self.logger.info("GET: %s" % request.path)
        self.user = request.getUser()
        self.password = request.getPassword()
##         if not self.user or not self.password:
##             return self.rejectInvalid(self, request)
        if self.user and self.password:
            self.logger.info("Authenticated access from %s / user %s" % (request.getClient(), self.user) )
        path = request.path
        if path.startswith('/'): # This is just stupid. Twisted bug? --pbanka
            path = path[1:]
        parts = path.split('/')
        filePath = urlparse.urlparse(path)[2]
        if filePath.startswith('/'):
            filePath = filePath[1:]
        if "website" in parts:
            function = self.findMethod(request.path,
                                       request, action="get")
            if type(function) == type("string"):
                return function
            try:
                return function(request, self.logger, self.errlog)
            except Exception, e:
                self.errlog.critical("Exception thrown in %s: %s" % (request.path, e))
                self.errlog.critical("%s" % sys.exc_info()[-1].tb_lineno)

        # FIXME: if this is a directory listing that doesn't end in '/', do a redirect.
        if parts[-1] == '' or os.path.isdir(filePath):
            if filePath.rfind('..') != -1:
                webUtil.err500(request, self.errlog)
            if os.path.isdir(filePath):
                return getDirectoryListing(filePath, self.logger)
            else:
                return webUtil.err404(request, self.errlog, filePath)
        if not os.path.isfile(filePath):
            return webUtil.err404(request, self.errlog, filePath)
        if filePath.endswith("css"):
            request.setHeader("Content-Type", "text/css")
        elif filePath.endswith("ini"):
            request.setHeader("Content-Type", "text/ini")
        elif filePath.endswith("txt"):
            request.setHeader("Content-Type", "text/plain")
        else:
            request.setHeader("Content-Type", "application/octet-stream")
        uploadThread = None
        if os.path.isfile(PKGINFO_FILE):
            packageData = shelve.open(PKGINFO_FILE, 'r')
            if filePath.rfind('..') != -1:
                webUtil.err500(request, self.errlog)
            if packageData.has_key(filePath):
                info = packageData[filePath]
                request.setHeader("ETag", info[MD5SUM])
                request.setHeader("Date", info[DATE])
                uploadThread = UploadThread(request, filePath, self.logger)
            else:
                uploadThread = UploadThread(request, filePath, self.logger, recordInfo = True)
            packageData.close()
        else:
            uploadThread = UploadThread(request, filePath, self.logger, recordInfo = True)
        uploadThread.start()
        return server.NOT_DONE_YET

    def findMethod(self, path, request, action="get"):
        "Returns a function pointer for an action that should be taken"
        path = path[path.index("website"):]
        if path.startswith('/'):
            path = path[1:]
        if path.endswith('/'):
            path = path[:-1]
        found = False
        classPath = path.replace('/', '.')
        parts = classPath.split('.')
        index = len(parts)
        while not found and index >= 1:
            pagePath = string.join(parts[0:index], '.').lower()
            module = "%s" % pagePath
            try:
                self.logger.debug( 'import %s' % module )
                exec('import %s' % module)
            except SyntaxError:
                self.errlog.warning( "Syntax error in import statement "\
                                     "for %s/%s" % (pagePath, module))
                index -= 1
                continue
            except ImportError, e:
                self.errlog.warning( "Import error importing: %s " % (module))
                index -= 1
                continue
            if hasattr(eval(module), action):
                self.logger.info( "Executing method : %s.%s(accessLog, errLog) " % (module, action))
                exec('function = %s.%s' % (module, action))
                return function
            elif hasattr(eval(module), "default") and action == "get":
                exec("default = %s.default" % module)
                self.logger.info( "Using default method for %s: %s" % (module, default) )
                return webUtil.err301(request, self.errlog, "/%s/%s" % (pagePath.replace('.','/')
                                                                        ,default))
            else:
                self.errlog.warning( "CANNOT Import : %s/%s " % (pagePath, module))
                index -= 1
                continue
        if not found:
            return webUtil.err404(self.request, self.errlog)

def main():
    os.chdir(ROOT_DIR)
    site = server.Site(WebSite())
    reactor.listenTCP(TCP_PORT, site)
    print "Bombardier twisted web server listening on %s" % TCP_PORT
    reactor.run(installSignalHandlers=0)

if __name__ == "__main__":
    arg = ''
    if len(sys.argv) > 1:
        arg = sys.argv[-1]
        print arg
    if sys.platform != "win32" and arg != '-d' and DEBUG == 0:
        # do the UNIX double-fork magic, see Stevens' "Advanced 
        # Programming in the UNIX Environment" for details (ISBN 0201563177)
        try: 
            pid = os.fork() 
            if pid > 0:
                # exit first parent
                sys.exit(0) 
        except OSError, e: 
            print >>sys.stderr, "fork #1 failed: %d (%s)" % (e.errno, e.strerror) 
            sys.exit(1)

        # decouple from parent environment
        os.chdir("/") 
        os.setsid() 
        os.umask(0) 

        # do second fork
        try: 
            pid = os.fork() 
            if pid > 0:
                # exit from second parent, print eventual PID before
                print "Daemon PID %d" % pid 
                sys.exit(0) 
        except OSError, e: 
            print >>sys.stderr, "fork #2 failed: %d (%s)" % (e.errno, e.strerror) 
            sys.exit(1) 
    else:
        print "Since we're running on Windows, we're not going to daemonize..."

    # start the daemon main loop
    main()
