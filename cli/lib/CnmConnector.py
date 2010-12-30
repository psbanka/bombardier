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
import tempfile
from bombardier_core.static_data import OK, FAIL, ABORTED_JOB_NAME
from bombardier_core.static_data import BDR_CLIENT_TYPE

from Exceptions import UnexpectedDataException, ServerException
from Exceptions import MachineTraceback, ServerTracebackException
from Exceptions import MachineUnavailableException, CommandError
import libUi
from SystemStateSingleton import SystemState
system_state = SystemState()

NULL_POST = {"spam": "eggs"}
LOGIN_PATH = "accounts/login/"
SEARCH_PATH = "json/%s/search/"
NAME_PATH = "json/%s/name/%s"
MACHINE_PATH = "json/machine/name/%s"
PACKAGE_SEARCH_PATH = "json/package/search/%s"
PACKAGE_NAME_PATH = "json/package/name/%s"
SUMMARY_NAME_PATH = "json/summary/name/%s"
URL_LOOKUP = {'test': "json/machine/start_test/%s",
              'dist': "json/machine/dist/%s",
              'init': "json/machine/init/%s",
              'reconcile': "json/machine/reconcile/%s",
              'check-status': "json/machine/check_status/%s",
              'fix': "json/machine/status/%s",
              'purge': "json/machine/status/%s",
              'enable': "json/machine/enable/%s",
              'disable': "json/machine/disable/%s",
              'setup': "json/machine/setup/%s",
             }


def curl_debug(debug_type, debug_msg):
    "Interface for debugging curl commands"
    print "debug(%d): %s" % (debug_type, debug_msg.strip())

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
                _fd, file_name = tempfile.mkstemp(suffix=".yml", text=True)
                handle = open(file_name, 'w')
                handle.write(str(self.output))
                handle.close()
                msg = "Not a list or dictionary (written to %s)" % file_name
                raise UnexpectedDataException(msg)
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
        if not address.endswith('/'):
            address = "%s/" % address
            #address = address[:-1]
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
            curl_obj.setopt(pycurl.VERBOSE, 1)
            curl_obj.setopt(pycurl.DEBUGFUNCTION, curl_debug)
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
        #full_path = '/'.join(self.address, path)
        curl_obj = self.prepare_curl_object(full_path)
        post_data = []
        for key in data:
            post_data.append((key, data[key]))
        encoded_post_data = urllib.urlencode(post_data)
        curl_obj.setopt(pycurl.POSTFIELDS, encoded_post_data)
        if self.debug:
            print "POSTING:", encoded_post_data
        return self.perform_request(curl_obj, full_path)

    def get_server_version(self):
        url = "json/version"
        output = self.service_yaml_request(url)
        return output

    def delete_user(self, user_name):
        "Remove an administrative user from the system"
        url = "json/user/name/%s" % user_name
        output = self.service_yaml_request(url, delete=True)
        return output["command_status"], output["command_output"]

    def set_password(self, user_name, password):
        "Set a user's password"
        url = "json/user/name/%s" % user_name
        post_data = { "password": password }
        output = self.service_yaml_request(url, post_data=post_data)
        return output["command_status"], output["command_output"]

    def login(self, password):
        '''Log in to the web server. Saves a sessionid
        password -- cleartext password to use to authenticate
        '''
        data = {"username": self.username, "password": password}
        msg = "Logging in as %s to %s"
        msg = msg  % (self.username, self.address)
        print msg
        response = self.post(LOGIN_PATH, data)
        return_data = response.convert_from_yaml()
        self.password = password
        return return_data.get("super_user")

    def package_build_job(self, package_name, svn_user, svn_password, bg_flag):
        "start a package-building job and watch its progress"
        url = "json/package_build/%s" % package_name
        post_data = {"svn_user": svn_user,
                     "svn_password": svn_password,
                     "debug": True,
                     "prepare": True}
        job_name = self.get_job(url, post_data)
        if bg_flag:
            return OK, ["Started job: %s" % job_name]
        return self.watch_jobs([job_name])

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
        #full_path = '/'.join(self.address, path)

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

    def get_restore_targets(self, machine_name, package_name):
        "Get restore data available for a machine"
        url = 'json/machine/restore/{0}/{1}'.format(machine_name, package_name)
        output = self.service_yaml_request(url)
        return output

    def get_server_config(self):
        "Get server-side configuration data"
        url = 'json/server/config'
        output = self.service_yaml_request(url)
        return output

    def sync_server_home(self, server_home_path):
        "For setting the base location of all data on the CNM"
        url = 'json/server/config'
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
            try:
                return response.convert_from_yaml()
            except UnexpectedDataException, err:
                err.url = path
                raise err
        except urllib2.HTTPError:
            print "Unable to connect to the service %s" % path
            return {}

    # MACHINE stuff:

    def machine_job(self, machine_name, command, bg_flag, post_data = None):
        "Run a non-package job on a remote machine"
        if post_data == None:
            post_data = {}
        url = URL_LOOKUP[command] % machine_name
        job_name = self.get_job(url, post_data)
        if bg_flag:
            return OK, ["Started job: %s" % job_name]
        libUi.info("Watching job progress. Press ^C to abort or disconnect.")
        return self.watch_jobs([job_name])

    def setup_command(self, machine_name, password, bg_flag):
        "Set a machine up"
        post_data = {"yaml": yaml.dump( {"password" : password } )}
        job_name =  self.machine_job(machine_name, "setup", bg_flag, post_data)
        return job_name

    def enable_command(self, machine_name, password, bg_flag):
        "Transfer ssh keys"
        post_data = {"yaml": yaml.dump( {"password" : password,
                                         "machine_type": BDR_CLIENT_TYPE }
                                      )}
        return self.machine_job(machine_name, "enable", bg_flag, post_data)

    def dist_command(self, machine_name, dist_name, bg_flag):
        "Set up a dist on a remote machine"
        post_data = {"dist": dist_name}
        return self.machine_job(machine_name, "dist", bg_flag, post_data)

    def package_command(self, command, machine_name, package_name,
                        argument, bg_flag):
        "Watch a package-job run on a machine"
        url = "json/package_action/%s" % package_name
        post_data = {"machine": machine_name,
                     "action": command}
        if argument:
            post_data["argument"] = argument
        job_name = self.get_job(url, post_data)
        if bg_flag:
            return OK, ["Started job: %s" % job_name]
        return self.watch_jobs([job_name])

    def machine_command(self, url_base, machine_name, bg_flag):
        "Perform a simple command on a remote machine"
        url = url_base + "/" + machine_name
        post_data = {"machine": machine_name}
        job_name = self.get_job(url, post_data)
        if bg_flag:
            return OK, ["Started job: %s" % job_name]
        return self.watch_jobs([job_name])

    def show_snapshots(self, machine_name):
        return OK, []

    def push_machine_config(self, machine_name, bg_flag):
        "Push the configuration to a remote machine"
        return self.machine_command("json/machine/push", machine_name, bg_flag)

    def unpush_machine_config(self, machine_name, bg_flag):
        "Remove the configuration on a remote machine"
        return self.machine_command("json/machine/unpush",
                                    machine_name, bg_flag)

    # END OF MACHINE STUFF

    def clear_broken_jobs(self, machine_name):
        """Remove all entries in the dispatcher that show broken
        jobs on a machine"""
        url = "json/machine/clear-broken-jobs/"
        post_data = {"machine": machine_name}
        output = self.service_yaml_request(url, post_data=post_data)
        return output["command_status"], output["command_output"]

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

    def kill_job(self, job_name):
        "Kill a running job on the server or one yet to run"
        post_data = {"post_data": "hello"}
        url = "json/job/kill/%s" % job_name
        output = self.service_yaml_request(url, post_data=post_data)
        return output["command_status"], output["command_output"]

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
                    _status, output = self.stop_jobs(machine_name)
                    print ">>> Halting all jobs on %s..." % machine_name
                    libUi.user_output(output, FAIL)
                raise exceptions.KeyboardInterrupt

        libUi.info("Finishing processing of %d jobs..." % len(jobs))
        return summary_status, summary_output

    def stop_jobs(self, machine_name):
        "Stop all the jobs on a machine"
        url = "json/machine/stop-jobs/"
        post_data = {"machine": machine_name}
        output = self.service_yaml_request(url, post_data=post_data)
        return output["command_status"], output["command_output"]

    def get_job(self, url, post_data):
        "Given an action dictated by URL, get a job_name that we can track"
        out = self.service_yaml_request(url, post_data=post_data)
        job_name = out.get("job_name")
        if job_name == ABORTED_JOB_NAME:
            msg = "Missing required .tar.gz files in <server_home>/dist."
            raise CommandError(msg)
        if "traceback" in out:
            raise MachineTraceback(url, out["traceback"])
        if out.get("command_status") != OK:
            raise MachineUnavailableException(out.get("command_output",""))
        return job_name

    def join_job(self, job_name):
        "We think a job's finished. Let's check in with the server"
        url = "json/job/join/%s" % job_name
        output = self.service_yaml_request(url)
        if "traceback" in output:
            raise MachineTraceback(url, output["traceback"])
        return output

    def show_jobs(self, machine_name):
        "Show what jobs are assigned to a given machine"
        url = "json/machine/show-jobs/%s" % machine_name
        output = self.service_yaml_request(url)
        return output

    def set_configuration_key(self, password):
        "Set the configuration-key"
        url = "json/dispatcher/set-password"
        post_data = {"password": password}
        output = self.service_yaml_request(url, post_data=post_data)
        return output["command_status"], output["command_output"]

    def dispatcher_status(self):
        "Find out how the dispatcher's doing"
        dispatcher_url = "json/dispatcher/"
        output = self.service_yaml_request(dispatcher_url)
        if output.get('command_status') == OK:
            return OK, output
        return FAIL, ["Dispatcher is offline."]

    def dispatcher_attach(self):
        "Attach to a running dispatcher"
        post_data = NULL_POST
        dispatcher_url = "json/dispatcher/"
        output = self.service_yaml_request(dispatcher_url, 
                                           post_data = post_data,
                                           timeout=5)
        return output["command_status"], output["command_output"]

    def get_uncommented_jobs(self):
        "get a list of tuples of jobs that need commenting"
        url = 'json/job/comment/pending/'
        content_dict = self.service_yaml_request(url)
        return content_dict["job_info"]

    def post_comments(self, publish, job_names, comments):
        "Attach comments to a list of job names"
        data = {"job_names": job_names,
                "comment": comments,
                "publish": publish,
               }
        post_data = {"yaml": yaml.dump(data)}
        url = 'json/job/comment/pending/'
        content_dict = self.service_yaml_request(url, post_data=post_data)
        return content_dict

