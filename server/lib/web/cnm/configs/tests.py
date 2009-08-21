from binascii import b2a_base64
from datetime import datetime
from django.core import serializers
from django.test import TestCase
from django.contrib.auth.models import User
import django.utils.simplejson as json
import webbrowser, re
import os, sys, yaml
from bombardier_core.static_data import SERVER_CONFIG_FILE, OK, FAIL
from Exceptions import InvalidServerHome
import time

TEST_PASSWORD = "testpass1234"

class BasicTest(TestCase):

    fixtures = ['init.json']

    def setUp(self):
        self.staff_user = User.objects.create_user('test_guy', 'tester@testy.com', TEST_PASSWORD)
        self.staff_user.is_staff = True
        self.staff_user.save()

        self.super_user = User.objects.create_user('super_guy', 'super@testy.com', TEST_PASSWORD)
        self.super_user.is_superuser = True
        self.super_user.save()

        self.login_super()
        # set the server home
        cwd = os.getcwd()
        config_url = '/json/server/config'
        self.test_server_home = os.path.join(cwd, 'configs', 'fixtures')
        response = self.client.post(path=config_url,
                                    data={"server_home": self.test_server_home})
        content_dict = json.loads( response.content )

        # sync the DB
        self.make_localhost_config()
        self.dbsync()

    def login(self, user=None):
        if not user:
            user = self.staff_user
        result = self.client.login(username=user.username, password=TEST_PASSWORD)
        self.failUnless(result)

    def login_super(self):
        result = self.client.login(username=self.super_user.username, password=TEST_PASSWORD)
        self.failUnless(result)

    def make_localhost_config(self, additional_config={}):
        config = {"ip_address": "127.0.0.1",
                  "default_user": os.getlogin(),
                  "spkg_path": "/opt/spkg",
                  "platform": sys.platform,
                 }
        config.update(additional_config)
        config_path = os.path.join(self.test_server_home, "machine",
                                   "localhost.yml")
        open(config_path, 'w').write(yaml.dump(config))

    def get_field_value_set(self, test_dict, value):
        return set([ i["fields"][value] for i in test_dict ])

    def get_content_dict(self, url, expected_status_code = 200):
        response = self.client.get(url)
        try:
            content_dict = json.loads( response.content )
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

    def dbsync(self):
        self.login(self.super_user)
        url = '/json/dbsync'
        response = self.client.post(path=url)
        self.failUnlessEqual(response.status_code, 200)

    def test_search(self):
        test_dict  = { "machine":  { "tes": ["tester1", "tester2"],
                                      "":  ["tester1", "tester2", "other1", "localhost"],
                                    "foo": [] },
                       "include": { ""    : ["app1", "otherapp"],
                                    "app" : ["app1"] },
                       "bom":     { ""    : ["foo", "bomp"],
                                    "fo"  : ["foo"],
                                    "swe" : [] },
                       "dist": { "": ["test", "bombardier-0.70-591"]  }, 
                       "package": { "": ["TestPackageType5", "TestPackageType4"]  } }
        for section in test_dict:
            for search_term in test_dict[section]:
                expected = test_dict[section][search_term]
                self.yml_file_search_test( section, search_term, expected )

    def test_get_server_home(self):
        url = '/json/server/config'

        content_dict = self.get_content_dict(url)
        self.failUnlessEqual(content_dict["server configuration"]["server_home"],
                             self.test_server_home)


    def set_status(self, status_dict):
        yaml_str = yaml.dump(status_dict)
        open("/opt/spkg/localhost/status.yml", 'w').write(yaml_str)
        open("configs/fixtures/status/localhost.yml", 'w').write(yaml_str)

    def reset_packages(self):
        status_dict = {"clientVersion": "0.70-591",
                       "install-progress": {},
                        "local-packages": [],
                        "status": {"newInstall": 'True'},
                        "timestamp": 1250868198.8639009,
                      }
        os.system("rm -rf /opt/spkg/localhost/packages/*")
        self.set_status(status_dict)

    def run_job(self, url, data={}, timeout=6, verbose=False):
        username = self.super_user.username
        
        response = self.client.post(path=url, data=data)
        content_dict = json.loads( response.content )
        job_name = content_dict["job_name"]
        assert job_name.startswith("%s@localhost" % (self.super_user.username))

        testing = True
        timeout_counter = 0
        next_number = 1
        while timeout_counter < timeout:
            url = '/json/job/poll/%s' % job_name
            content_dict = self.get_content_dict(url)
            assert "alive" in content_dict
            if verbose:
                print "NEW OUTPUT:", content_dict["new_output"], "ALIVE:", content_dict["alive"]
            if not content_dict["alive"]:
                break
            time.sleep(1)
            timeout_counter += 1

        url = '/json/job/join/%s' % job_name
        content_dict2 = self.get_content_dict(url)
        self.failUnlessEqual(content_dict2["status"], OK)

        url = '/json/machine/cleanup'
        response = self.client.post(path=url, data={})
        content_dict = json.loads( response.content )
        return content_dict2["command_output"]

    def package_action(self, action):
        url = '/json/package/TestPackageType4'
        package_config = {"test": {"value":"nowisthetimeforalldooment",
                                   "directory": "/tmp/foogazi"},
                           "packages": ["TestPackageType4"],  
                         }
        self.make_localhost_config(additional_config=package_config)
        post_data={"machine": "localhost", "action": action}
        output = self.run_job(url, data=post_data, timeout=60)
        return output

    def NOtest_run_check_job(self):
        url = '/json/machine/start_test/localhost'
        status, output = self.run_job(url)
        assert "1" in output, output

    def NOtest_dist_update(self):
        url = '/json/machine/dist/localhost'
        status, output = self.run_job(url, data={"dist": "test"}, timeout=60)
        assert "EMPTY_TEST-1.egg-info" in output, output

    def NOtest_client_update(self):
        url = '/json/machine/dist/localhost'
        status, output = self.run_job(url, data={"dist": "bombardier-0.70-591"}, timeout=60, verbose=False)
        assert "bombardier-0.70_591.egg-info" in output, output

        url = '/json/machine/init/localhost'
        output = self.run_job(url, data={}, timeout=60)

    def NOtest_package_error(self):
        self.reset_packages()
        status, output = self.package_action("configure")
        assert status == FAIL
        status, output = self.package_action("verify")
        assert status == FAIL
        status, output = self.package_action("uninstall")
        assert status == FAIL

    def NOtest_package_actions(self):
        self.reset_packages()
        status, output = self.package_action("install")
        assert status == OK
        status, output = self.package_action("configure")
        assert status == OK
        status, output = self.package_action("verify")
        assert status == OK
        status, output = self.package_action("install")
        assert status == OK # bc shines us on
        status, output = self.package_action("uninstall")
        assert status == OK
        status, output = self.package_action("fix")
        assert status == OK
        status, output = self.package_action("purge")
        assert status == OK

    def test_package_global(self):
        self.reset_packages()
        url = '/json/machine/reconcile/localhost'
        package_config = {"test": {"value":"nowisthetimeforalldooment",
                                   "directory": "/tmp/foogazi"},
                          "packages": ["TestPackageType4"],  
                         }
        self.make_localhost_config(additional_config=package_config)

        status, output = self.run_job(url, data={}, timeout=60)
        assert status == OK

        package_config = {"test": {"value":"nowisthetimeforalldooment",
                                   "directory": "/tmp/foogazi"},
                          "packages": [],  
                         }
        self.make_localhost_config(additional_config=package_config)

        status, output = self.run_job(url, data={}, timeout=60)
        assert status == OK



        return output

    def test_merged(self):
        self.login(self.super_user)
        url = '/json/server/config'
        test_home = os.path.join(os.getcwd(), "configs", "fixtures")
        response = self.client.post(path=url, data={"server_home": test_home})
        self.failUnlessEqual(response.status_code, 200)
        machine_name = "tester1"
        url = '/json/merged/name/%s' % machine_name
        content_dict = self.get_content_dict(url)
        expected = {"ip_address": u"127.0.0.1",
                    "include": [u"otherapp", u"app1"],
                    "stuff": 127,
                    "packages": [u"apache2.2.1", u"bomp"]}
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
        self.failUnlessEqual(content_dict["status"], OK)

        content_dict = self.get_content_dict('/json/user/name/delete_me')
        self.failUnlessEqual(content_dict[u"first_name"], "Buffy")
