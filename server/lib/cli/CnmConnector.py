import StringIO
import urllib2, urlparse, os
import yaml, syck
import socket
import pycurl
import httplib
import urllib
from bombardier_core.static_data import OK, FAIL

LOGIN_PATH = "/accounts/login/"

def make_query_string(args):
    query_string = ""
    if len(args.keys()) >= 1:
        for arg_index in range(0, len(args.keys())):
            key = args.keys()[arg_index]
            value = args[args.keys()[arg_index]]
            if arg_index == 0:
                query_string += "?%s=%s" % (key, value)
            else:
                query_string += "&%s=%s" % (key, value)
    return query_string

class ServerException(Exception):
    def __init__(self, url, curl_err, http_code):
        Exception.__init__(self)
        self.url = url
        self.curl_err = curl_err
        self.http_code = http_code
    def __repr__(self):
        return "Can't connect to %s (%s) (%d)" % (self.url, self.curl_err, self.http_code)
    def __str__(self):
        return self.__repr__()

class UnexpectedDataException(Exception):
    def __init__(self, reason):
        Exception.__init__(self)
        self.reason = reason
    def __repr__(self):
        return "Got unexpected data from the server (%s)" % self.reason
    def __str__(self):
        return self.__repr__()

class Storage:
    def __init__(self):
        self.contents = []

    def store(self, buf):
        self.contents.append(buf)

    def __str__(self):
        return '\n'.join(self.contents)

class Header(Storage):

    def convert_from_yaml(self):
        str = '\n'.join(self.contents[1:])
        data = syck.load(str)
        return data

    def get_content_type(self):
        data = self.convert_from_yaml()
        return data.get("Content-Type")

class Response:
    def __init__(self, header, output):
        self.header = header
        self.output = output
        self.http_code = None

    def set_http_code(self, http_code):
        self.http_code = http_code

    def convert_from_yaml(self):
        if self.header.get_content_type() == "application/json":
            output = syck.load(str(self.output))
            if type(output) != type([]) and type(output) != type({}):
                raise UnexpectedDataException("Not a list or dictionary")
            if output == None:
                raise UnexpectedDataException("Null value received")
            return output
        raise UnexpectedDataException("can't convert to yaml")

    def __str__(self):
        return str(self.output)

class CnmConnector:

    def __init__(self, address, username, logger):
        self.address = address
        self.username = username
        self.logger = logger
        self.password = None
        self.proxy_address = None
        self.proxy_port = None
        self.debug = False
        self.session_id = None
        self.cookie_file = os.path.join(os.environ.get("HOME"), '.bom_cookie.txt')

    def prepare_curl_object(self, url):
        curl_obj = pycurl.Curl()
        curl_obj.setopt(pycurl.URL, url)
        curl_obj.setopt(pycurl.SSL_VERIFYPEER, 0)
        curl_obj.setopt(pycurl.FAILONERROR, 1)
        curl_obj.setopt(pycurl.FOLLOWLOCATION, 1)
        curl_obj.setopt(pycurl.COOKIEFILE, self.cookie_file)
        curl_obj.setopt(pycurl.COOKIEJAR, self.cookie_file)
        if self.username and self.password:
            auth_string = "%s:%s" % (self.username, self.password)
            curl_obj.setopt(pycurl.USERPWD, auth_string)
            #curl_obj.setopt(pycurl.HTTPAUTH, pycurl.HTTPAUTH_ANYSAFE)
            curl_obj.setopt(pycurl.HTTPAUTH, pycurl.HTTPAUTH_BASIC)
        if self.proxy_address != None:
            curl_obj.setopt(pycurl.PROXY, self.proxy_address)
            if self.proxy_port != None:
                curl_obj.setopt(pycurl.PROXYPORT, self.proxy_port)
        return curl_obj

    def perform_request(self, curl_obj, full_path):
        output = Storage()
        header = Header()
        curl_obj.setopt(pycurl.WRITEFUNCTION, output.store)
        curl_obj.setopt(pycurl.HEADERFUNCTION, header.store)
        response = Response(header, output)
        try:
            curl_obj.perform()
            http_code = curl_obj.getinfo(pycurl.HTTP_CODE)
            response.set_http_code(http_code)
            return response
        except pycurl.error, curl_err:
            http_code = curl_obj.getinfo(pycurl.HTTP_CODE)
            raise ServerException(full_path, curl_err[1], http_code)

    def post(self, path, data):
        full_path = urlparse.urljoin(self.address, path)
        curl_obj = self.prepare_curl_object(full_path)
        post_data = []
        for key in data:
            post_data.append((key, data[key]))
        encoded_post_data = urllib.urlencode(post_data)
        curl_obj.setopt(pycurl.POSTFIELDS, encoded_post_data)
        return self.perform_request(curl_obj, full_path)

    def login(self, password):
        data = {"username": self.username, "password": password}
        response = self.post(LOGIN_PATH, data)
        data = response.convert_from_yaml()
        self.password = password
        return data.get("super_user")

    def service_request(self, path, args=None, put_data=None, post_data=None):
        if not args:
            args = {}
        full_path = urlparse.urljoin(self.address, path)
        query_string = make_query_string(args)
        url = urlparse.urljoin(full_path, query_string)
        if self.debug:
            self.logger.debug("Performing service request to %s" % url)
        curl_obj = self.prepare_curl_object(url)
        if put_data:
            curl_obj.setopt(pycurl.UPLOAD, 1)
            curl_obj.setopt(pycurl.INFILESIZE, len(put_data))
            out_data = StringIO.StringIO(put_data)
            curl_obj.setopt(pycurl.READFUNCTION, out_data.read)
        if post_data != None:
            post_list = []
            for key in post_data:
                post_list.append((key, data[key]))
            encoded_post_data = urllib.urlencode(post_list)
            curl_obj.setopt(pycurl.POSTFIELDS, encoded_post_data)
            curl_obj.setopt(pycurl.POST, 1)
        return self.perform_request(curl_obj, full_path)

    def service_yaml_request(self, path, args=None, put_data=None, post_data=None):
        if not args:
            args = {}
        try:
            if put_data:
                if type(put_data) == type(["list"]) or \
                   type(put_data) == type({}):
                    put_data = yaml.dump(put_data)
            response = self.service_request(path, args, put_data, post_data)
            return response.convert_from_yaml()
        except urllib2.HTTPError:
            self.logger.error("Unable to connect to the service %s" % path)
            return {}

