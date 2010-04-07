# BSD License
# Copyright (c) 2009, Peter Banka et al
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the name of the GE Security nor the names of its contributors may
#   be used to endorse or promote products derived from this software without
#   specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

"""Provides utilities for accessing the RESTful web service, which is the
new core of the Bombardier server."""

__author__ =  'Peter Banka'
__version__ = '1.0'

import BreakHandler
import StringIO
import urllib2, urlparse, os, time
import yaml
import pycurl
import exceptions
import re
import urllib
from bombardier_core.static_data import OK, FAIL
from Exceptions import UnexpectedDataException, ServerException
from Exceptions import MachineTraceback, ServerTracebackException
from Exceptions import MachineUnavailableException
import libUi
from SystemStateSingleton import SystemState
system_state = SystemState()

LOGIN_PATH = "/accounts/login/"

def make_query_string(args):
    """args -- a dictionary.
    Converts a dictionary into HTML keyword arguments for

    >>> make_query_string({'a': 'hello', 'b':'there'})
    '?a=hello&b=there'
    """
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

class Storage:
    '''An object used to hold the output of a pyCurl command'''
    def __init__(self):
        self.contents = []

    def store(self, buf):
        '''Command used by pycurl to store data
        buf -- content to add '''
        self.contents.append(buf)

    def __str__(self):
        return '\n'.join(self.contents)

class Header(Storage):
    '''A specific kind of storage object which holds HTML header information'''
    def convert_from_yaml(self):
        '''Since a header is largely YAML, this simply converts it to a
        dictionary'''
        yaml_lines = [ line for line in list(self.contents) if ':' in line ]
        yaml_string = ''.join(yaml_lines)
        data = yaml.load(yaml_string)
        return data

    def get_content_type(self):
        '''Returns the self-described content type'''
        data = self.convert_from_yaml()
        return data.get("Content-Type")

class Response:
    '''Aggregation of Header and Storage objects to form an HTTP response'''
    def __init__(self, header, output):
        '''header -- Header object
        output -- Storage object (for body)
        '''
        self.header = header
        self.output = output
        self.http_code = None

    def set_http_code(self, http_code):
        '''setter for http response code
        http_code -- HTTP response code from server
        '''
        self.http_code = http_code

    def convert_from_yaml(self):
        '''Assumes that the server attempted to respond with YAML(JSON).
        Verifies the content type and then converts the body'''
        if self.header.get_content_type() == "application/json":
            try:
                output = yaml.load(str(self.output))
            except yaml.parser.ParserError:
                # New Django JSON is not compatible with yaml. Massage it.
                try:
                    new_output = ''.join(str(self.output).split('\n'))
                    output = yaml.load(new_output)
                except yaml.parser.ParserError:
                    msg = "can't convert to yaml (%s)" % self.output
                    raise UnexpectedDataException(msg)
            if type(output) != type([]) and type(output) != type({}):
                raise UnexpectedDataException("Not a list or dictionary")
            if output == None:
                raise UnexpectedDataException("Null value received")
            if 'traceback' in output:
                raise ServerTracebackException(output['traceback'])
            return output
        raise UnexpectedDataException("can't convert to yaml")

    def __str__(self):
        return str(self.output)

class CnmConnector:
    '''Object to make it easier to communicate with the Bombardier RESTFul
    web server'''

    def __init__(self, address, username):
        '''
        address -- URL of the Bombardier web server
        username -- username for connecting to the server'''
        self.break_handler = BreakHandler.BreakHandler()
        self.address = address
        self.username = username
        self.password = None
        self.proxy_address = None
        self.proxy_port = None
        self.debug = False
        self.session_id = None
        self.cookie_file = os.path.join(os.environ.get("HOME"),
                                        '.bom_cookie.txt')

    def prepare_curl_object(self, url):
        '''prepares pycurl object that can be used to communicate to the
        Bombardier web service
        url -- full path of the destination
        '''
        curl_obj = pycurl.Curl()
        curl_obj.setopt(pycurl.URL, url)
        curl_obj.setopt(pycurl.SSL_VERIFYPEER, 0)
        curl_obj.setopt(pycurl.FAILONERROR, 1)
        curl_obj.setopt(pycurl.FOLLOWLOCATION, 1)
        curl_obj.setopt(pycurl.COOKIEFILE, self.cookie_file)
        curl_obj.setopt(pycurl.COOKIEJAR, self.cookie_file)
        if self.debug:
            print "CONNECTING TO: ", url
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
        '''Connects to the web server and prepares returns a response
        object.
        curl_obj -- a pycurl object, initialized
        full_path -- the full destination of connection
        '''
        output = Storage()
        header = Header()
        curl_obj.setopt(pycurl.WRITEFUNCTION, output.store)
        curl_obj.setopt(pycurl.HEADERFUNCTION, header.store)
        response = Response(header, output)
        if self.debug:
            print "PERFORMING:", curl_obj
        try:
            self.break_handler.enable()
            curl_obj.perform()
            http_code = curl_obj.getinfo(pycurl.HTTP_CODE)
            response.set_http_code(http_code)
            self.break_handler.disable()
            return response
        except pycurl.error, curl_err:
            http_code = curl_obj.getinfo(pycurl.HTTP_CODE)
            raise ServerException(full_path, curl_err[1], http_code)

    def post(self, path, data):
        '''Prepare to perform a POST
        path -- relative URL for posting
        data -- dictionary of POST data
        '''
        full_path = urlparse.urljoin(self.address, path)
        curl_obj = self.prepare_curl_object(full_path)
        post_data = []
        for key in data:
            post_data.append((key, data[key]))
        encoded_post_data = urllib.urlencode(post_data)
        curl_obj.setopt(pycurl.POSTFIELDS, encoded_post_data)
        if self.debug:
            print "POSTING:", encoded_post_data
        return self.perform_request(curl_obj, full_path)

    def login(self, password):
        '''Log in to the web server. Saves a sessionid
        password -- cleartext password to use to authenticate
        '''
        data = {"username": self.username, "password": password}
        response = self.post(LOGIN_PATH, data)
        return_data = response.convert_from_yaml()
        self.password = password
        return return_data.get("super_user")

    def service_request(self, path, args=None,
                        put_data=None, post_data=None,
                        delete=False, timeout=None):
        '''Performs a GET or a POST or a PUT, depending on data provided
        and returns text data based on output
        path -- relative path
        args -- query string variables to set
        put_data -- data to convert to a PUT
        post_data -- data to convert to a POST
        '''
        if not args:
            args = {}
        full_path = urlparse.urljoin(self.address, path)
        query_string = make_query_string(args)
        url = urlparse.urljoin(full_path, query_string)
        if self.debug:
            print "Performing service request to %s" % url
        curl_obj = self.prepare_curl_object(url)
        if timeout != None:
            curl_obj.setopt(pycurl.TIMEOUT, timeout)
            if self.debug:
                print "Setting timeout: ", timeout
        if put_data != None:
            curl_obj.setopt(pycurl.UPLOAD, 1)
            curl_obj.setopt(pycurl.INFILESIZE, len(put_data))
            out_data = StringIO.StringIO(put_data)
            curl_obj.setopt(pycurl.READFUNCTION, out_data.read)
            if self.debug:
                print "Setting PUT data"
        if delete:
            curl_obj.setopt(pycurl.CUSTOMREQUEST, "DELETE")
        if post_data != None:
            post_list = []
            for key in post_data:
                post_list.append((key, post_data[key]))
            encoded_post_data = urllib.urlencode(post_list)
            curl_obj.setopt(pycurl.POSTFIELDS, encoded_post_data)
            curl_obj.setopt(pycurl.POST, 1)
            if self.debug:
                print "POSTING:", encoded_post_data
        return self.perform_request(curl_obj, full_path)

    def sync_server_home(self, server_home_path):
        "For setting the base location of all data on the CNM"
        url = '/json/server/config'
        post_data = {"server_home": server_home_path}
        output = self.service_yaml_request(url, post_data=post_data)
        new_path = output["server_home"]
        if new_path != server_home_path:
            raise UnexpectedDataException("server_home is set to %s" % new_path)

    def service_yaml_request(self, path, args=None,
                             put_data=None, post_data=None,
                             delete=False, timeout=None):
        '''same as service_request, but assumes that return data from the server
        is YAML(JSON) and converts it to return a python dictionary instead of
        text.'''
        try:
            if put_data != None:
                if type(put_data) == type(["list"]) or \
                   type(put_data) == type({}):
                    put_data = yaml.dump(put_data)
            response = self.service_request(path, args, put_data, post_data,
                                            delete, timeout)
            return response.convert_from_yaml()
        except urllib2.HTTPError:
            print "Unable to connect to the service %s" % path
            return {}

    def cleanup_connections(self):
        "Call the ReST interface for clearing connections no a machine"
        url = "json/machine/cleanup_connections"
        self.service_yaml_request(url, post_data={})

    def _is_any_alive(self, poll_output):
        "Determine if anu jobs are alive from what the CNM told us"
        for job_name in poll_output:
            if poll_output[job_name].get("alive", False):
                return True
        return False

    def watch_jobs(self, job_names):
        "Given a list of jobs, watch their progress."
        summary_output = []
        summary_status = OK
        jobs = set(job_names)
        try:
            while job_names:
                jobs = jobs.union(job_names)
                time.sleep(0.25)
                url = "json/job/poll/"
                post_data = {"yaml": yaml.dump({"job_names": job_names})}
                output = self.service_yaml_request(url, post_data=post_data)
                if type(output) != type({}):
                    msg = "Request to %s returned and invalid type (%s)"
                    libUi.error(msg % (url, output))
                    return FAIL, output
                for job_name in output:
                    job_output = output[job_name]
                    if "traceback" in job_output:
                        raise MachineTraceback(url, job_output["traceback"])
                    new_output = job_output.get("new_output", '')
                    if new_output:
                        libUi.process_cnm(new_output)
                    alive = job_output.get("alive")
                    if alive == None:
                        if job_output.get("command_status") == "QUEUED":
                            pending_job = job_output.get("pending_job")
                            msg = "%s is pending %s." % (job_name, pending_job)
                            libUi.warning(msg)
                            libUi.info("Watching %s..." % pending_job)
                            job_names = [pending_job]
                            break
                    if alive == False:
                        out = system_state.cnm_connector.join_job(job_name)
                        summary_output.append(out.get("command_output"))
                        status = out.get("command_status", OK)
                        if status != OK:
                            summary_status = FAIL
                        job_names +=  out.get("children", [])
                        job_names.remove(job_name)
                        comments = out.get("jobs_requiring_comment", 0)
                        if comments:
                            system_state.comment_counter = len(comments)
        except exceptions.KeyboardInterrupt:
            term = libUi.ask_yes_no("Terminate job", libUi.YES)
            if term == libUi.NO:
                msg = ["Disconnected from %s" % job_names,
                       "To re-connect, use 'job view'"]
                return OK, msg
            else:
                matcher = re.compile("(\S+)@(\S+)\-(\d+)")
                machine_names = set()
                for job_name in job_names:
                    _user, machine_name, _counter = matcher.findall(job_name)[0]
                    machine_names.update([machine_name])
                output = {}
                for machine_name in machine_names:
                    url = "/json/machine/stop-jobs/"
                    post_data = {"machine": machine_name}
                    print ">>> Halting all jobs on %s..." % machine_name
                    output = self.service_yaml_request(url, post_data=post_data)
                    libUi.user_output(output["command_output"], FAIL)
                raise exceptions.KeyboardInterrupt

        libUi.info("Finishing processing of %d jobs..." % len(jobs))
        return summary_status, summary_output

    def get_job(self, url, post_data):
        "Given an action dictated by URL, get a job_name that we can track"
        out = self.service_yaml_request(url, post_data=post_data)
        if "traceback" in out:
            raise MachineTraceback(url, out["traceback"])
        if out.get("command_status") != OK:
            raise MachineUnavailableException(out.get("command_output",""))
        job_name = out.get("job_name")
        return job_name

    def join_job(self, job_name):
        "We think a job's finished. Let's check in with the server"
        url = "json/job/join/%s" % job_name
        output = self.service_yaml_request(url)
        if "traceback" in output:
            raise MachineTraceback(url, output["traceback"])
        return output

    def set_password(self, password):
        "Set the configuration-key"
        url = "/json/dispatcher/set-password"
        post_data = {"password": password}
        output = self.service_yaml_request(url, post_data=post_data)
        return output["command_status"], output["command_output"]

    def dispatcher_control(self, action, post_data = {}):
        "Performing actions on the dispatcher"
        dispatcher_url = "/json/dispatcher/%s" % action
        if action == "status":
            output = self.service_yaml_request(dispatcher_url)
            if output.get('command_status') == OK:
                return OK, output
            else:
                return_list = ["Dispatcher is offline."]
                return_status = FAIL
            return return_status, return_list
        else:
            try:
                output = self.service_yaml_request(dispatcher_url, 
                                                   post_data = post_data,
                                                   timeout=5)
            except ServerException:
                if action == "start":
                    output = {"command_status": OK,
                              "command_output": ['Dispatcher started']}
                else:
                    raise
        return output["command_status"], output["command_output"]

    def get_uncommented_jobs(self):
        "get a list of tuples of jobs that need commenting"
        url = '/json/job/comment/pending/'
        content_dict = self.service_yaml_request(url)
        return content_dict["job_info"]

    def post_comments(self, publish, job_names, comments):
        "Attach comments to a list of job names"
        data = {"job_names": job_names,
                "comment": comments,
                "publish": publish,
               }
        post_data = {"yaml": yaml.dump(data)}
        url = '/json/job/comment/pending/'
        content_dict = self.service_yaml_request(url, post_data=post_data)
        return content_dict

