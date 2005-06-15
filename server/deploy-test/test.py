#!/usr/bin/python
#!/cygdrive/c/Python23/python.exe

import urllib2, httplib, sys, urllib

USER_AGENT = "foomanchu"

def openAnything(source, etag=None, lastmodified=None, agent=USER_AGENT):
    # non-HTTP code omitted for brevity
    if urlparse.urlparse(source)[0] == 'http':
        # open URL with urllib2
        request = urllib2.Request(source)
        request.add_header('User-Agent', agent)
        if etag:
            request.add_header('If-None-Match', etag)
        if lastmodified:
            request.add_header('If-Modified-Since', lastmodified)
        request.add_header('Accept-encoding', 'gzip')
        opener = urllib2.build_opener(SmartRedirectHandler(), DefaultErrorHandler())
        return opener.open(request)

class DefaultErrorHandler(urllib2.HTTPDefaultErrorHandler):
    def http_error_default(self, req, fp, code, msg, headers):
        result = urllib2.HTTPError(
            req.get_full_url(), code, msg, headers, fp)
        result.status = code
        return result

httplib.HTTPConnection.debuglevel = 1
#feedData = urllib.urlopen("http://192.168.1.199/repository/system-setup.ini")
#sys.exit(0)
url = 'http://192.168.1.199/repository/isscript9-1.spkg'
#url = 'http://localhost:8080/deploy/isscript9-1.spkg'
request = urllib2.Request(url)
#opener = urllib2.build_opener()
#firstDatastream = opener.open(request)
#print firstDatastream.headers.dict
request.add_header('If-Modified-Since', "Thu, 08 Jul 2004 04:46:49 GMT")
#f = opener.open(request)
#print request.headers
# {'If-modified-since': 'Thu, 15 Apr 2004 19:45:21 GMT'}
#sys.exit(0)

auth_handler = urllib2.HTTPBasicAuthHandler()
auth_handler.add_password('default', 'localhost:8080', 'peter', 'abc')

#opener = urllib2.build_opener(DefaultErrorHandler())
opener = urllib2.build_opener(auth_handler)
urllib2.install_opener(opener)
seconddatastream = opener.open(request)

#print "Status:", seconddatastream.status
print "DATA:", seconddatastream.read(100)


