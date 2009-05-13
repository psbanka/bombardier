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

class CnmConnector:

    def __init__(self, address, username, password, logger):
        self.address = address
        self.username = username
        self.password = password
        self.logger = logger
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

    def perform_request(self, curl_obj):
        output_file = StringIO.StringIO()
        curl_obj.setopt(pycurl.WRITEFUNCTION, output_file.write)
        try:
            curl_obj.perform()
            output_file.seek(0)
            output = output_file.read()
            return output
        except pycurl.error, curl_err:
            url = curl_obj.getopt(pycur.URL)
            erstr = "Connection problem to %s: %s" % (url, curl_err[1])
            if self.logger:
                self.logger.warning(erstr)
            return ''

    def post(self, path, data):
        full_path = urlparse.urljoin(self.address, path)
        curl_obj = self.prepare_curl_object(full_path)
        post_data = []
        for key in data:
            post_data.append((key, data[key]))
        encoded_post_data = urllib.urlencode(post_data)
        curl_obj.setopt(pycurl.POSTFIELDS, encoded_post_data)
        return self.perform_request(curl_obj)

    def login(self):
        data = {"username": self.username, "password": self.password}
        return self.post(LOGIN_PATH, data)

    def service_request(self, path, args=None, put_data=None):
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
        return self.perform_request(curl_obj)

    def service_yaml_request(self, path, args=None, put_data=None):
        if not args:
            args = {}
        try:
            if put_data:
                if type(put_data) == type(["list"]) or \
                   type(put_data) == type({}):
                    put_data = yaml.dump(put_data)
            yml_data = self.service_request(path, args, put_data)
            output = syck.load(yml_data)
        except urllib2.HTTPError:
            self.logger.error("Unable to connect to the service %s" % path)
            return {}
        return output

