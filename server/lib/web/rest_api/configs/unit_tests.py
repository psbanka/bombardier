import os, sys, yaml
sys.path.append('.')
sys.path.append('..')
os.environ["DJANGO_SETTINGS_MODULE"] = 'bombardier_server.web.rest_api.settings'

from binascii import b2a_base64
from datetime import datetime
from django.core import serializers
import django.utils.simplejson as json
from bombardier_core.mini_utility import ensure_bombardier_config_dir
from bombardier_server.cnm.Exceptions import InvalidServerHome
from bombardier_core.Cipher import Cipher

from settings import *
from django.contrib.auth.models import User
import django.utils.simplejson as json
import os, sys, yaml
from bombardier_core.static_data import SERVER_CONFIG_FILE, CLIENT_CONFIG_FILE, OK, FAIL
import time

import unittest
from django.test.client import Client

import glob

DEBUG = True
DEBUG = False

def get_version(base_name):
    dist_dir = os.path.join("configs", "fixtures", "dist")
    search_files = glob.glob(os.path.join(dist_dir, base_name+"*"))
    if len(search_files) != 1:
        msg = """
               There needs to be one (and only one) %stype file in %s 
               to perform the unit tests.\n"""
        print msg % (base_name, dist_dir)
        sys.exit(1)
    found_file = search_files[0]
    version = found_file.rpartition(base_name)[-1].rpartition('.tar.gz')[0]
    return version
    
TEST_PASSWORD = "testpass1234"
CONFIG_PASSWORD = "abc123"
CLIENT_VERSION = get_version("bombardier_client-")
CORE_VERSION = get_version("bombardier_core-")
CONFIG_FILE = "unit_test_config_info.yml"
DISPATCHER_CONTROL_CMD = "bdr_dispatcher"

print "Clients will be updated to the following:"
print "1.00-client:", CLIENT_VERSION
print "1.00-core:", CORE_VERSION

BUILD_PACKAGE_NAME = "TestPackageBuild"

def get_current_configs():
    ensure_bombardier_config_dir()
    server_conf = {}
    client_conf = {}
    if os.path.isfile(SERVER_CONFIG_FILE):
        test_server_conf = yaml.load(open(SERVER_CONFIG_FILE).read())
        if type(test_server_conf) == type({}):
            server_conf = test_server_conf
    if os.path.isfile(CLIENT_CONFIG_FILE):
        test_client_conf = yaml.load(open(CLIENT_CONFIG_FILE).read())
        if type(test_client_conf) == type({}):
            client_conf = test_client_conf
    return server_conf, client_conf

def modify_configs(test_server_home, test_machine_home):
    server_config_data = {"server_home": test_server_home}
    client_config_data = {"spkg_path": test_machine_home}
    server_conf, client_conf = get_current_configs()

    print "Updating %s" % SERVER_CONFIG_FILE
    if server_conf:
        if "server_home" in server_conf:
            server_conf["old_server_home"] = server_conf["server_home"]
    server_conf.update(server_config_data)
    print "Setting server home to: %s" % test_server_home
    open(SERVER_CONFIG_FILE, 'w').write(yaml.dump(server_conf))

    print "Updating %s" % CLIENT_CONFIG_FILE
    if client_conf:
        if "spkg_path" in client_conf:
            client_conf["old_spkg_path"] = client_conf["spkg_path"]
    client_conf.update(client_config_data)
    print "Client home to: %s" % test_machine_home
    open(CLIENT_CONFIG_FILE, 'w').write(yaml.dump(client_conf))

def revert_configs():
    server_conf, client_conf = get_current_configs()
    if "old_server_home" in server_conf:
        print "Reverting %s" % SERVER_CONFIG_FILE
        server_conf["server_home"] = server_conf["old_server_home"]
        del server_conf["old_server_home"]
        open(SERVER_CONFIG_FILE, 'w').write(yaml.dump(server_conf))
    if "old_spkg_path" in client_conf:
        print "Reverting %s" % CLIENT_CONFIG_FILE
        client_conf["spkg_path"] = client_conf["old_spkg_path"]
        del client_conf["old_spkg_path"]
        open(CLIENT_CONFIG_FILE, 'w').write(yaml.dump(client_conf))
    #os.system("rm -rf test_spkg")
    print "rm -rf test_spkg"
        
class BasicTest:
    def setUp(self):
        self.client = Client()

        self.staff_user = User.objects.get(username='test_guy')
        self.staff_user.is_staff = True
        self.staff_user.save()

        self.super_user = User.objects.get(username='super_guy')
        self.super_user.is_superuser = True
        self.super_user.save()

        self.login_super()
        cwd = os.getcwd()
        self.test_server_home = os.path.join(cwd, "configs", "fixtures")
        self.test_machine_home = os.path.join(cwd, "test_spkg")

        dispatcher_info = yaml.load(open(os.path.join(self.test_server_home, "dispatcher_info.yml")).read())
        uri = dispatcher_info["dispatcher_uri"]
        self.attach_dispatcher(uri)
        self.make_localhost_config()

    def attach_dispatcher(self, uri):
        # set the configuration string
        url = '/json/dispatcher/attach'
        response = self.client.post(url, {"uri": uri})
        content_dict = json.loads( response.content )
        self.failUnlessEqual( content_dict[ u"command_status" ], OK)
        assert content_dict[ u"command_output" ][0].startswith("Attached to dispatcher")
        url = '/json/dispatcher/status'
        response = self.client.get(url)
        content_dict = json.loads( response.content )
        if DEBUG:
            msg = "DISPATCHER BEING TESTED: %s (%s)" % (uri, content_dict["uptime"])
            print "=" * len(msg)
            print msg
            print "=" * len(msg)

    def login(self, user=None):
        if not user:
            user = self.staff_user
        result = self.client.login(username=user.username, password=TEST_PASSWORD)
        self.failUnless(result)

    def login_super(self):
        result = self.client.login(username=self.super_user.username, password=TEST_PASSWORD)
        self.failUnless(result)

    def get_field_value_set(self, test_dict, value):
        return set([ i["fields"][value] for i in test_dict ])

    def get_content_dict(self, url, expected_status_code = 200):
        response = self.client.get(url)
        try:
            content_dict = json.loads( response.content )
            self.check_for_traceback(content_dict)
        except ValueError:
            self.fail("Did not receive any proper JSON from the server!")
        self.failUnlessEqual(response.status_code, expected_status_code)
        return content_dict

    def yml_file_search_test(self, config_type, search_term, expected_list):
        url = '/json/%s/search/%s' % ( config_type, search_term )
        expected = set(expected_list)
        content_dict = self.get_content_dict(url)
        name_set = self.get_field_value_set( content_dict, "name" )
        self.failUnlessEqual(name_set, expected)

    def get_bombardier_yaml_dict(self):
        yaml_str = open(SERVER_CONFIG_FILE).read()
        config_dict = yaml.load(yaml_str)
        return config_dict

    def set_status(self, status_dict):
        yaml_str = json.dumps(status_dict)
        open("%s/localhost/status.yml" % self.test_machine_home, 'w').write(yaml_str)
        open("configs/fixtures/status/localhost.yml", 'w').write(yaml_str)

    def check_for_traceback(self, content_dict):
        if "traceback" in content_dict:
            print "EXCEPTION===================================="
            print ''.join(content_dict["traceback"])
            print "============================================="
            
        assert not "traceback" in content_dict, content_dict

    def run_job(self, url, data={}, timeout=6, verbose=False, require_comment=None):
        username = self.super_user.username
        
        response = self.client.post(path=url, data=data)
        if verbose:
            print "RESPONSE.CONTENT============================="
            print response.content
            print "============================================="
        content_dict = json.loads( response.content )
        if verbose:
            print "CONTENT_DICT================================="
            print content_dict
            print "============================================="

        try:
            job_name = content_dict["job_name"]
        except KeyError:
            print "CONT========"
            for line in content_dict['traceback']:
                print "> %s" % (line)
            assert False
        #assert job_name.startswith("%s@localhost" % (self.super_user.username))

        timeout_counter = 0
        next_number = 1
        while timeout_counter < timeout:
            job_names = { "job_names": [job_name] }
            data = {'yaml': yaml.dump(job_names)} 
            url = '/json/job/poll/'
            response = self.client.post(path=url, data=data)
            content_dict = json.loads( response.content )
            time.sleep(0.1)
            if verbose:
                print "NEW OUTPUT:", content_dict
            job_content_dict = content_dict.get(job_name)
            if job_content_dict["command_status"] == "QUEUED":
                continue
            assert "alive" in job_content_dict
            if not job_content_dict["alive"]:
                break
            time.sleep(1)
            timeout_counter += 1

        url = '/json/job/join/%s' % job_name
        join_output = self.get_content_dict(url)
        if require_comment != None:
            assert "jobs_requiring_comment" in join_output, join_output
            found_job_name = False
            for c_job_name, c_job_info in join_output["jobs_requiring_comment"]:
                if job_name == c_job_name:
                    found_job_name = True
                    break
            assert found_job_name == True, join_output["jobs_requiring_comment"]
        assert "command_status" in join_output, join_output
        if verbose:
            print "JOIN OUTPUT================================"
            print join_output
            print "==========================================="

        url = '/json/machine/cleanup'
        response = self.client.post(path=url, data={})
        if verbose:
            print "FINAL OUTPUT: %s" % join_output
        return join_output["command_status"], join_output["complete_log"], join_output.get("command_output")

    def make_localhost_config(self, additional_config={}):
        config = {"ip_address": "127.0.0.1",
                  "default_user": "root",
                  #"default_user": os.getlogin(),
                  "spkg_path": self.test_machine_home,
                  "platform": sys.platform,
                 }
        self.make_localhost_yaml_file(config, additional_config, "machine")

    def make_localhost_status(self, additional_config={}):
        status_dict = { "clientVersion" : CLIENT_VERSION,
                        "coreVersion" : CORE_VERSION,
                        "install-progress" : {},
                        "local-packages" : [],
                        "status" : { "newInstall" : 'True' }}
        self.make_localhost_yaml_file(status_dict, additional_config, "status")

    def make_localhost_yaml_file(self, data_dict, additional_data, yaml_subdir):
        data_dict.update(additional_data)
        data_path = os.path.join(self.test_server_home, yaml_subdir,
                                   "localhost.yml")
        open(data_path, 'w').write(yaml.dump(data_dict))

class CnmTests(unittest.TestCase, BasicTest):

    def setUp(self):
        BasicTest.setUp(self)
        self.make_localhost_config()

    def test_search(self):
        client_dist = "bombardier_client-%s" % CLIENT_VERSION
        core_dist = "bombardier_core-%s" % CORE_VERSION
        test_dict  = { "machine":  { "tes": ["tester", "tester1", "tester2"],
                                      "":  ["tester", "tester1", "tester2", "other1", "localhost"],
                                    "foo": [] },
                       "include": { ""    : ["app1", "otherapp"],
                                    "app" : ["app1"] },
                       "bom":     { ""    : ["foo", "bomp"],
                                    "fo"  : ["foo"],
                                    "swe" : [] },
                       "dist": { "": ["test", client_dist, core_dist]  }, 
                       "package": { "": ["TestPackageType5", "TestPackageType4", "TestPackageBuild", "TestT5Backup"]  } }
        for section in test_dict:
            for search_term in test_dict[section]:
                expected = test_dict[section][search_term]
                self.yml_file_search_test( section, search_term, expected )

    def test_get_server_home(self):
        url = '/json/server/config'

        content_dict = self.get_content_dict(url)
        self.failUnlessEqual(content_dict["server configuration"]["server_home"],
                             self.test_server_home)

    def test_make_machine(self):
        self.login(self.super_user)
        machine_name = "delete_me"
        data = {"ip_address": "127.0.0.1",
                "include": ["app1"],
                "bom": ["bomp"]}
        url = '/json/machine/name/%s' % machine_name
        new_machine_data = {"yaml": data}
        response = self.client.post(url, new_machine_data)
        content_dict = json.loads( response.content )
        assert content_dict["command_status"] == OK, output
        machine_file = os.path.join(self.test_server_home, "machine", "delete_me.yml")
        assert os.path.isfile(machine_file)
        os.system("rm -f %s" % machine_file)

    def test_get_machine(self):
        self.login(self.super_user)
        machine_name = "tester1"
        url = '/json/machine/name/%s' % machine_name
        content_dict = self.get_content_dict(url)
        expected = {"ip_address": "127.0.0.1",
                    "include": ["otherapp", "app1"],
                    "bom": ["bomp"]}
        for key in expected:
            self.failUnlessEqual(type(expected[key]), type(content_dict[key]))
            if type(expected[key]) != type([]):
                self.failUnlessEqual(content_dict[key], expected[key])
            else:
                assert set(expected[key]).issubset(set(content_dict[key]))

    def test_get_dist(self):
        self.login(self.super_user)
        dist_name = "test"
        url = '/json/dist/name/%s' % dist_name
        content_dict = self.get_content_dict(url)
        expected = {"fields": {"name": "test"}}
        for key in expected:
            self.failUnlessEqual(type(expected[key]), type(content_dict[key]))
            if type(expected[key]) != type([]):
                self.failUnlessEqual(content_dict[key], expected[key])
            else:
                assert set(expected[key]).issubset(set(content_dict[key]))
    def test_merged(self):
        self.login(self.super_user)
        machine_name = "tester1"
        url = '/json/merged/name/%s' % machine_name
        content_dict = self.get_content_dict(url)
        expected = {"ip_address": "127.0.0.1",
                    "include": ["otherapp", "app1"],
                    "stuff": 127,
                    "packages": ["apache2.2.1", "bomp"]}
        for key in expected:
            self.failUnlessEqual(type(expected[key]), type(content_dict[key]))
            if type(expected[key]) != type([]):
                self.failUnlessEqual(content_dict[key], expected[key])
            else:
                assert set(expected[key]).issubset(set(content_dict[key]))

    def test_authentication(self):
        self.client.logout()

        url = '/json/check_authentication'
        content_dict = self.get_content_dict(url, 403)
        self.failUnlessEqual(content_dict["status"], "NOT_AUTHENTICATED")

        result = self.client.login(username=self.staff_user.username, password='foodyfoo')
        self.failIf(result)

        content_dict = self.get_content_dict(url, 403)
        self.failUnlessEqual(content_dict["status"], "NOT_AUTHENTICATED")

        result = self.client.login(username=self.staff_user.username, password=TEST_PASSWORD)
        self.failUnless(result)

        url = '/json/check_authentication'
        content_dict = self.get_content_dict(url)
        self.failUnlessEqual(content_dict["status"], "OK")

    def test_user(self):
        self.login(self.super_user)
        search_url = '/json/user/search/'
        specific_url = '/json/user/name/'

        content_dict = self.get_content_dict(search_url)
        user0 = content_dict[0].get("fields",{})
        self.failUnlessEqual(user0["username"], "test_guy")
        user1 = content_dict[1].get("fields",{})
        self.failUnlessEqual(user1["username"], "super_guy")

        new_user_data = {"first_name": "delete",
                         "last_name": "me",
                         "email": "delete_me@test.com",
                         "active": True,
                         "admin": True}
        self.client.post(specific_url+'delete_me', new_user_data)

        content_dict = self.get_content_dict('/json/user/name/delete_me')
        self.failUnlessEqual(content_dict["username"], "delete_me")
        self.failUnlessEqual(content_dict[u"first_name"], "delete")


        response = self.client.post('/json/user/name/delete_me', {"first_name": "Buffy"})
        content_dict = json.loads( response.content )
        self.failUnlessEqual(content_dict["command_status"], OK)

        content_dict = self.get_content_dict('/json/user/name/delete_me')
        self.failUnlessEqual(content_dict[u"first_name"], "Buffy")

    #def test_set_bad_password(self):
        #self.login(self.super_user)
        #url = '/json/dispatcher/set_password'
        #content_dict = self.set_config_password("F8t3Babb1%tz")
        #self.failUnlessEqual( content_dict[ u"command_status" ], FAIL)
        #self.failUnlessEqual( content_dict[ u"command_output" ], ["Invalid configuration key."])

    def test_data_modification(self):
        url = "/json/machine/name/tester"
        config_data = {"ip_address": "127.0.0.2", 
                       "default_user": "rudy",
                       "platform": "linux",
                      }
        yaml_string = yaml.dump(config_data)
        response = self.client.post(path=url, data={"yaml": yaml_string})
        content_dict = json.loads( response.content )
        assert content_dict["command_status"] == OK
        assert content_dict["command_output"].startswith("updated"), content_dict

    def test_summary(self):
        self.make_localhost_status()
        url = "/json/summary/name/localhost"
        response = self.client.get(url)
        content_dict = json.loads( response.content )
        assert content_dict["command_status"] == u"OK", content_dict

        test_dict = {"value":"test_value", "directory": "/tmp/foogazi"}
        config_packages = ["TestPackageType4"]
        package_config = {"test": test_dict,
                          "packages": config_packages,  
                        }
        self.make_localhost_config(additional_config=package_config)

        install_progress_yaml = """TestPackageType5:
                                       DEPENDENCY_ERRORS: []
                                       INSTALLED: Tue Jul 28 12:06:13 2009
                                       UNINSTALLED: NA
                                       VERIFIED: Mon Jul 13 13:46:28 2009"""
        install_progress = yaml.load(install_progress_yaml)
        status_config = { "install-progress": install_progress }

        self.make_localhost_status(additional_config=status_config)
        
        response = self.client.get(url)
        content_dict = json.loads( response.content )
        assert content_dict["command_status"] == "OK", content_dict
        assert content_dict["not_installed"] == config_packages, content_dict
        assert content_dict["installed"] == ["TestPackageType5"], content_dict

class DispatcherTests(unittest.TestCase, BasicTest):

    def setUp(self):
        BasicTest.setUp(self)
        self.make_localhost_config()

    def test_push_config(self):
        url = '/json/machine/push/localhost'
        status, output, cmd_output = self.run_job(url)
        assert status == OK
        config_file = os.path.join(self.test_machine_home, "localhost" , "config.yml")
        assert os.path.isfile(config_file)
        url = '/json/machine/unpush/localhost'
        status, output, cmd_output = self.run_job(url)
        assert status == OK
        assert os.path.isfile(config_file) == False

    def test_kill_job(self):
        url = '/json/machine/start_test/localhost'
        username = self.super_user.username
        
        response = self.client.post(path=url)
        content_dict = json.loads( response.content )
        try:
            job_name = content_dict["job_name"]
        except KeyError:
            print "CONT========", content_dict
            assert False

        url = '/json/job/kill/%s' % job_name
        response = self.client.post(path=url, data={"timeout": 1})
        content_dict = json.loads( response.content )
        assert "command_status" in content_dict, content_dict

    def test_run_check_job(self):
        url = '/json/machine/start_test/localhost'
        status, output, cmd_output = self.run_job(url)
        assert "1" in output, output

    def test_dist_update(self):
        url = '/json/machine/dist/localhost'
        status, output, cmd_output = self.run_job(url, data={"dist": "test"}, timeout=60)
        assert status == OK
        assert "EMPTY_TEST-1.egg-info" in output, output

    def test_stop_all_jobs(self):
        url = '/json/machine/start_test/localhost'
        response = self.client.post(path=url, data={"machine": "localhost"})
        content_dict = yaml.load(response.content)
        assert "job_name" in content_dict, content_dict
        job_name1 = content_dict["job_name"]

        response = self.client.post(path=url, data={"machine": "localhost"})
        content_dict = yaml.load(response.content)
        job_name2 = content_dict["job_name"]

        url = "/json/dispatcher/status"
        start_time = time.time()
        while True:
            response = self.client.get(url)
            content_dict = json.loads( response.content )
            if job_name1 in content_dict["active_jobs"]:
                break
            time.sleep(1)
            assert (time.time() - start_time) < 5.0

        data = yaml.dump({"jobs": [job_name2]})
        response = self.client.post("/json/job/poll/", data={"yaml": data})
    
        url = '/json/machine/stop-jobs/'
        response = self.client.post(path=url, data={"machine": "localhost"})
        content_dict = json.loads( response.content )
        assert content_dict["command_status"] == OK, content_dict
        test_str = "%s killed" % job_name1
        output = content_dict["command_output"]
        assert test_str in output, "%s not in %s" % (test_str, output)
        test_str = "%s removed" % job_name2
        assert "%s removed" % job_name2 in output, "%s not in %s" % (test_str, output)

        url = "/json/machine/show-jobs/localhost"
        start_time = time.time()
        found1 = False
        found2 = False
        while not (found1 and found2):
            response = self.client.get(url)
            content_dict = json.loads( response.content )
            assert "job_info" in content_dict, content_dict
            broken_job_info = content_dict["job_info"]["broken_jobs"]
            for job_info in broken_job_info:
                if job_name1 in job_info:
                    found1 = True
                elif job_name2 in job_info:
                    found2 = True
            time.sleep(1)
            assert (time.time() - start_time) < 5.0
    
        url = '/json/machine/clear-broken-jobs/'
        response = self.client.post(path=url, data={"machine": "localhost"})
        content_dict = json.loads( response.content )
        assert content_dict["command_status"] == OK, content_dict
        output = content_dict["command_output"]
        assert "%s cleared" % job_name1 in output, output
        assert "%s cleared" % job_name2 in output, output

        url = "/json/dispatcher/status"
        response = self.client.get(url)
        content_dict = json.loads( response.content )
        broken_jobs = content_dict["broken_jobs"]

        assert not job_name1 in content_dict["broken_jobs"], content_dict
        assert not job_name2 in content_dict["broken_jobs"], content_dict

    def test_client_update(self):
        url = '/json/machine/dist/localhost'
        client_dist = "bombardier_client-%s" % CLIENT_VERSION
        core_dist = "bombardier_core-%s" % CORE_VERSION
        status, output, cmd_output = self.run_job(url, data={"dist": core_dist}, timeout=60, verbose=False)
        assert core_dist in output, output
        status, output, cmd_output = self.run_job(url, data={"dist": client_dist}, timeout=60, verbose=False)
        assert client_dist in output, output

        url = '/json/machine/init/localhost'
        status,output, cmd_output = self.run_job(url, data={}, timeout=60)
        assert status == OK

    def acknowledge_everything(self):
        url = '/json/job/comment/pending/'
        response = self.client.get(url)
        content_dict = json.loads( response.content )
        job_names = []
        for job_name, job_info in content_dict["job_info"]:
            job_names.append(job_name)
        data = {"job_names": job_names,
                "comment": "test comment",
                "publish": False,
               }
        post_data = {"yaml": yaml.dump(data)}
        response = self.client.post(path=url, data=post_data)
        content_dict = json.loads( response.content )

    def test_dist_update_for_commenting(self):
        self.acknowledge_everything()

        url = '/json/machine/dist/localhost'
        status, output, cmd_output = self.run_job(url, data={"dist": "test"}, timeout=60, require_comment=True)
        assert status == OK

        url = '/json/job/comment/pending/'
        response = self.client.get(url)
        content_dict = json.loads( response.content )
        found_job_name = None
        for job_name, job_info in content_dict["job_info"]:
            if job_info == "DIST: test":
                found_job_name = job_name
                break
        assert found_job_name

        data = {"job_names": [found_job_name],
                "comment": "test comment",
                "publish": True,
               }
        post_data = {"yaml": yaml.dump(data)}
        response = self.client.post(path=url, data=post_data)
        content_dict = json.loads( response.content )
        assert found_job_name in content_dict, content_dict
        assert content_dict[found_job_name] == 'comment applied.', content_dict

        url = '/json/comments/'
        response = self.client.get(url)
        content_dict = json.loads( response.content )
        assert content_dict["command_status"] == OK

        url = '/json/job/comment/pending/'
        response = self.client.get(url)
        content_dict = json.loads( response.content )
        found_job_name = None
        for job_name, job_info in content_dict["job_info"]:
            if job_info == "DIST: test":
                found_job_name = job_name
                break
        assert not found_job_name, content_dict

    def test_dispatcher_status(self):
        url = "/json/dispatcher/status"
        response = self.client.get(url)
        content_dict = json.loads( response.content )
        assert float(content_dict["uptime"]) > 0.000001
        assert content_dict["active_jobs"] == [], content_dict

class PackageTests(unittest.TestCase, BasicTest):

    def setUp(self):
        BasicTest.setUp(self)
        self.make_localhost_config()

    def package_action(self, action, package_name, arguments = [], verbose=False):
        url = '/json/package_action/%s' % package_name
        package_config = {"test": {"value":"nowisthetimeforalldooment",
                                   "directory": "/tmp/foogazi"},
                          "packages": [package_name],  
                         }
        self.make_localhost_config(additional_config=package_config)
        post_data={"machine": "localhost", "action": action, "arguments": arguments}
        yaml_post_data = {"yaml": yaml.dump(post_data)}
        status, output, cmd_output = self.run_job(url, data=yaml_post_data, timeout=60, verbose=verbose)
        return status, output, cmd_output

    def reset_packages(self):
        status_dict = {"clientVersion": CLIENT_VERSION,
                       "coreVersion": CORE_VERSION,
                       "install-progress": {},
                       "local-packages": [],
                       "status": {"newInstall": 'True'},
                       "timestamp": 1250868198.8639009,
                      }
        os.system("rm -rf %s/localhost/packages/*" % self.test_machine_home)
        os.system("rm -rf %s/repos/libs/*" % self.test_machine_home)
        os.system("rm -rf %s/repos/injectors/*" % self.test_machine_home)
        self.set_status(status_dict)

    def test_package_error(self):
        self.reset_packages()
        package_name = "TestPackageType4"
        status, output, cmd_output = self.package_action("configure", package_name)
        assert status == FAIL
        status, output, cmd_output = self.package_action("verify", package_name)
        assert status == FAIL
        status, output, cmd_output = self.package_action("uninstall", package_name)
        assert status == FAIL

    def test_type5_package_actions(self):
        self.reset_packages()
        package_name = "TestPackageType5"
        status, output, cmd_output = self.package_action("install", package_name)
        assert status == OK
        url = '/json/status/name/localhost'
        status_data = self.get_content_dict(url)
        progress_data = status_data["install-progress"]["TestPackageType5-23"]
        assert "INSTALLED" in progress_data

    def test_type5_backup(self):
        self.reset_packages()
        # clean out archive directory
        archive_dir = os.path.join(self.test_server_home, "archive", "localhost")
        os.system("rm -rf {0}".format(archive_dir))

        package_name = "TestT5Backup"
        status, output, cmd_output = self.package_action("install", package_name)
        assert status == OK, status
        url = '/json/status/name/localhost'
        status_data = self.get_content_dict(url)
        progress_data = status_data["install-progress"]["TestT5Backup-1"]
        assert "INSTALLED" in progress_data

        # should show as a backup provider
        response = self.client.get("/json/summary/name/localhost")
        content_dict = json.loads( response.content )
        assert "backup_providers" in content_dict, content_dict
        assert content_dict["backup_providers"] == ["TestT5Backup"], content_dict

        # Run backup and check output
        status, output, cmd_output = self.package_action("backup", package_name)
        assert status == OK, status
        expected_keys = set(["size", "elapsed_time", "status", "md5", "backup_file"])
        assert type(cmd_output) == type({}), cmd_output
        received_keys = set(cmd_output["/tmp/foogazi/test_type_5"].keys())
        assert expected_keys == received_keys, received_keys
        start_time = str(int(cmd_output.get("__START_TIME__")))
        latest_archive = os.path.join(self.test_server_home, "archive", "localhost", "TestT5Backup", start_time, "tmp", "foogazi")
        assert os.path.isdir(latest_archive), latest_archive
        backup_file_name = cmd_output["/tmp/foogazi/test_type_5"]["backup_file"]
        assert backup_file_name == "test_type_5.bz2", backup_file_name
        backup_path = os.path.join(latest_archive, backup_file_name)
        assert os.path.isfile(backup_path), backup_path
        assert os.system("bunzip2 %s" % backup_path) == OK
        backup_path = backup_path[:backup_path.rfind('.bz2')]
        current_data = open("/tmp/foogazi/test_type_5").read().replace('\n', '|')
        assert current_data == "INSTALLED|STOPPED|STARTED|", current_data
        backup_data = open(backup_path).read().replace('\n', '|')
        assert backup_data == "INSTALLED|STOPPED|", backup_data
        assert os.system("bzip2 %s" % backup_path) == OK
        backup_path = backup_path + ".bz2"
        assert os.path.isfile(backup_path)

        # Examine what restore targets are available
        machine_name = "localhost"
        url = '/json/machine/restore/{0}/{1}'.format( machine_name, "TestT5Backup" )
        content_dict = self.get_content_dict(url)
        expected = [ start_time ]
        assert "targets" in content_dict, content_dict
        assert content_dict["targets"] == expected, content_dict["targets"]

        os.system("rm -f /tmp/foogazi/test_type_5")
        # Restore the target we want
        status, output, cmd_output = self.package_action("restore", package_name, [start_time])
        assert status == OK
        current_data = open("/tmp/foogazi/test_type_5").read().replace('\n', '|')
        assert current_data == backup_data, current_data
        

    def _get_file_names(self, file_dict, file_names):
        for name in file_dict:
            sub_item = file_dict[name]
            if type(sub_item) == type({}):
                file_names += self._get_file_names(sub_item, file_names)
            elif type(sub_item) == type('string') or type(sub_item) == type(u'string'):
                if '.tar.gz' in sub_item:
                    file_names.append(sub_item)
        return file_names

    def _remove_paths(self, package_name):
        url = '/json/package/name/%s' % package_name
        status_data = self.get_content_dict(url)
        for section in ["injectors", "libs"]:
            file_names = self._get_file_names(status_data[section], [])
            for file_name in file_names:
                if os.path.isfile(file_name):
                    os.unlink(file_name)

    def _get_package_data(self, package_name):
        url = '/json/package/name/%s' % package_name
        status_data = self.get_content_dict(url)
        return status_data

    def test_package_build(self):
        package_data = self._get_package_data(BUILD_PACKAGE_NAME)
        current_release = package_data["release"]
        self._remove_paths(BUILD_PACKAGE_NAME)
        url = '/json/package_build/%s' % BUILD_PACKAGE_NAME
        post_data = {"svn_user": "",
                     "svn_password": "",
                     "debug": False, "prepare": True
                    }
        status, output, cmd_output = self.run_job(url, data=post_data, timeout=60)
        assert status == OK
        new_package_data = self._get_package_data(BUILD_PACKAGE_NAME)
        updated_release = new_package_data["release"]
        assert updated_release == current_release + 1, "%s, %s" % (updated_release, current_release)
        executables = new_package_data.get("executables", {})
        assert "public_function" in executables.keys(), executables
        assert executables["public_function"] == "This function does something interesting.", executables["public_function"]
        assert "generic_function" in executables.keys(), executables
        assert executables["generic_function"] == 'Undocumented Executable', executables["public_function"]

    def test_package_actions(self):
        self.reset_packages()
        package_name = "TestPackageType4"
        status, output, cmd_output = self.package_action("install", package_name)
        assert status == OK
        url = '/json/status/name/localhost'
        status_data = self.get_content_dict(url)
        progress_data = status_data["install-progress"]["TestPackageType4-7"]
        assert "INSTALLED" in progress_data

        status, output, cmd_output = self.package_action("configure", package_name)
        assert status == OK
        status, output, cmd_output = self.package_action("verify", package_name)
        assert status == OK
        status, output, cmd_output = self.package_action("install", package_name)
        assert status == OK # bc shines us on
        status, output, cmd_output = self.package_action("uninstall", package_name)
        assert status == OK
        status, output, cmd_output = self.package_action("fix", package_name)
        assert status == OK
        status, output, cmd_output = self.package_action("purge", package_name+"-7")
        assert status == OK

    def test_package_global(self):
        self.reset_packages()
        package_config = {"test": {"value":"nowisthetimeforalldooment",
                                   "directory": "/tmp/foogazi"},
                          "packages": ["TestPackageType4"],  
                         }
        self.make_localhost_config(additional_config=package_config)

        url = '/json/machine/status/localhost'
        status, output, cmd_output = self.run_job(url, data={}, timeout=60)
        assert status == OK
        url = '/json/machine/reconcile/localhost'
        status, output, cmd_output = self.run_job(url, data={}, timeout=60)
        assert status == OK

        package_config = {"test": {"value":"nowisthetimeforalldooment",
                                   "directory": "/tmp/foogazi"},
                          "packages": [],  
                         }
        self.make_localhost_config(additional_config=package_config)

        status, output, cmd_output = self.run_job(url, data={}, timeout=60)
        assert status == OK
        return output

    def test_insufficient_config(self):
        self.reset_packages()
        package_config = {"test": {"value":"nowisthetimeforalldooment"},
                          "packages": ["TestPackageType4"],  
                         }
        self.make_localhost_config(additional_config=package_config)

        self.reset_packages()
        url = '/json/machine/reconcile/localhost'
        status, output, cmd_output = self.run_job(url, data={}, timeout=60)
        #print "OUTPUT:",output
        #print "STATUS",status
        assert status == FAIL

    def test_encrypted_ci(self):
        self.reset_packages()
        cipher = Cipher(CONFIG_PASSWORD)
        cipher_text = cipher.encrypt_string("/tmp/foogazi")
        package_config = {"test": {"value":"nowisthetimeforalldooment",
                                   "enc_directory": cipher_text},
                          "packages": ["TestPackageType4"],  
                         }
        self.make_localhost_config(additional_config=package_config)

        self.reset_packages()

        url = '/json/machine/reconcile/localhost'
        status, output, cmd_output = self.run_job(url, data={}, timeout=60)
        assert status == OK, output


class DangerousTests(unittest.TestCase, BasicTest):

    def setUp(self):
        BasicTest.setUp(self)
        self.make_localhost_config()

    def test_disable(self):
        url = "/json/machine/disable/localhost" 
        response = self.client.post(path=url, data={})
        content_dict = json.loads( response.content )
        assert content_dict["command_status"] == OK

    def test_good_enable(self):
        url = "/json/machine/enable/localhost"
        import getpass
        password = getpass.getpass("What is the correct password?")
        config_data = {
                          "password" : password
                      }
        yaml_string = yaml.dump(config_data)
        response = self.client.post(path=url, data={"yaml": yaml_string})
        content_dict = json.loads( response.content )
        assert content_dict["command_status"] == OK


class ExperimentTest(unittest.TestCase, BasicTest):

    def setUp(self):
        BasicTest.setUp(self)
        self.make_localhost_config()

def create_test_package_yaml(test_server_home):
        svn_uri = "file://%s/test_repos" % test_server_home
        package_yaml = """
class_name: Tester.Tester
configuration: {}
injectors:
  test_injector: { svn: '%s/injectors/Tester'}
libs:
  test_lib: { svn: '%s/libs/Tester'}
package-version: 5
release: 2
""" % ( svn_uri, svn_uri )
        package_path = "%s/package/%s.yml" % (test_server_home, BUILD_PACKAGE_NAME)
        open(package_path, "w").write(package_yaml)

def initialize_tests(client):
    from django.test.utils import connection

    cwd = os.getcwd()
    test_server_home = os.path.join(cwd, 'configs', 'fixtures')
    test_machine_home = os.path.join(os.getcwd(), "test_spkg")
    modify_configs(test_server_home, test_machine_home)

    connection.creation.create_test_db(autoclobber=True)
    fixtures = ['init.json']
    staff_user = User.objects.create_user('test_guy', 'tester@testy.com', TEST_PASSWORD)
    staff_user.is_staff = True
    staff_user.save()

    super_user = User.objects.create_user('super_guy', 'super@testy.com', TEST_PASSWORD)
    super_user.is_superuser = True
    super_user.save()
    login_result = client.login(username="super_guy", password=TEST_PASSWORD)
    url = '/json/server/config'
    response = client.post(url, data={"server_home": test_server_home})

    cmd = "%s -s %s -p %s -x -d %s -i start" % (DISPATCHER_CONTROL_CMD, test_server_home, test_server_home, CONFIG_PASSWORD)
    print "Running test dispatcher: %s" % cmd
    os.system(cmd)

    create_test_package_yaml(test_server_home)
    os.system("rm -f %s/machine/localhost.yml" % test_server_home)
    os.system("rm -rf %s" % test_machine_home)
    os.system("mkdir -p %s/localhost/packages" % test_machine_home)
    open("%s/localhost/status.yml" % test_machine_home, 'w').write('{}\n')

def tear_down_dispatcher(client):
    cwd = os.getcwd()
    test_server_home = os.path.join(cwd, 'configs', 'fixtures')
    cmd = "%s -s %s -p %s -x stop" % (DISPATCHER_CONTROL_CMD, test_server_home, test_server_home)
    print "Stopping test dispatcher: %s" % cmd
    os.system(cmd)
    revert_configs()

if __name__ == '__main__':
    client = Client()
    initialize_tests(client)

    full_suite = 1
    if len(sys.argv) > 1:
        full_suite = 0

    suite = unittest.TestSuite()
    if full_suite:
        suite.addTest(unittest.makeSuite(CnmTests))
        suite.addTest(unittest.makeSuite(DispatcherTests))
        suite.addTest(unittest.makeSuite(PackageTests))
        suite.addTest(unittest.makeSuite(ExperimentTest))
    else:
#        suite.addTest(CnmTests("test_data_modification"))
#        suite.addTest(CnmTests("test_get_machine"))
#        suite.addTest(CnmTests("test_get_dist"))
#        suite.addTest(CnmTests("test_search"))
#        suite.addTest(CnmTests("test_merged"))
#       suite.addTest(CnmTests("test_summary"))
#        suite.addTest(CnmTests("test_user"))
#        suite.addTest(DispatcherTests("test_dist_update_for_commenting"))
#        suite.addTest(DispatcherTests("test_push_config"))
#        suite.addTest(DispatcherTests("test_stop_all_jobs"))
#        suite.addTest(DispatcherTests("test_client_update"))
#        suite.addTest(PackageTests("test_encrypted_ci"))
#        suite.addTest(PackageTests("test_insufficient_config"))
#        suite.addTest(PackageTests("test_package_error"))
        #suite.addTest(PackageTests("test_type5_package_actions"))
         suite.addTest(PackageTests("test_package_build"))
#        suite.addTest(PackageTests("test_package_error"))
#        suite.addTest(PackageTests("test_package_global"))
#        suite.addTest(PackageTests("test_type5_backup"))

#        suite.addTest(PackageTests("test_package_actions"))

    status = unittest.TextTestRunner(verbosity=2).run(suite)

    tear_down_dispatcher(client)

