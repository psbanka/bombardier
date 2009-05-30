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
        self.login()

        self.super_user = User.objects.create_user('super_guy', 'super@testy.com', TEST_PASSWORD)
        self.super_user.is_superuser = True
        self.super_user.save()

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

    def test_search(self):
        test_dict  = { "machine":  { "tes": ["tester1", "tester2"],
                                      "":  ["tester1", "tester2", "other1"],
                                    "foo": [] },
                       "include": { ""    : ["app1", "otherapp"],
                                    "app" : ["app1"] },
                       "bom":     { ""    : ["foo", "bomp"],
                                    "fo"  : ["foo"],
                                    "swe" : [] } }
        for section in test_dict:
            for search_term in test_dict[section]:
                expected = test_dict[section][search_term]
                self.yml_file_search_test( section, search_term, expected )

    def get_bombardier_yaml_dict(self):
        yaml_str = open(SERVER_CONFIG_FILE).read()
        config_dict = yaml.load(yaml_str)
        return config_dict

    def dbsync(self):
        self.login(self.super_user)
        url = '/json/dbsync'
        response = self.client.post(path=url)
        self.failUnlessEqual(response.status_code, 200)

    def test_get_server_home(self):
        url = '/json/server/config'
        content_dict = self.get_content_dict(url)
        expected = {"server_home": "NULL"}
        self.failUnlessEqual(content_dict["server_home"], expected["server_home"])

        self.dbsync()

        expected = self.get_bombardier_yaml_dict()
        url = '/json/server/config'
        content_dict = self.get_content_dict(url)
        self.failUnlessEqual(content_dict["server_home"], expected["server_home"])

    def test_run_check_job(self):
        self.dbsync()

        self.login(self.super_user)
        machine_name = 'localhost'
        username = self.super_user.username

        url = '/json/machine/start_test/%s' % machine_name
        response = self.client.post(path=url, data={})
        content_dict = json.loads( response.content )
        job_name = content_dict["job_name"]
        assert job_name.startswith("%s@%s" % (self.super_user.username, machine_name))

        url = '/json/job/poll/%s' % job_name
        content_dict = self.get_content_dict(url)
        assert u"alive" in content_dict

        url = '/json/job/join/%s' % job_name
        content_dict = self.get_content_dict(url)
        self.failUnlessEqual(content_dict["status"], OK)

        url = '/json/machine/cleanup'
        response = self.client.post(path=url, data={})
        content_dict = json.loads( response.content )

        # Fails on cygwin sometimes. Might work elsewhere more reliably.
        #self.failUnlessEqual(content_dict["localhost"], OK)

    def test_change_server_home(self):
        self.login(self.super_user)
        url = '/json/server/config'
        response = self.client.post(path=url, data={"server_home": "NO_PATH"})
        content_dict = json.loads( response.content )
        expected = {"server_home": u"NO_PATH"}
        self.failUnlessEqual(content_dict["server_home"], expected["server_home"])

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
                self.failUnlessEqual(set(content_dict[key]), set(expected[key]))

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
