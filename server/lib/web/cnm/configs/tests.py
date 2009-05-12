from binascii import b2a_base64
from datetime import datetime
from django.core import serializers
from django.test import TestCase
from django.utils.functional import curry
import django.utils.simplejson as json
import webbrowser, re
import os, sys, yaml
from bombardier_core.static_data import SERVER_CONFIG_FILE

class BasicTest(TestCase):

    fixtures = ['init.json']

    def setUp(self):
        pass

    def get_field_value_set(self, test_dict, value):
        return set([ i["fields"][value] for i in test_dict ])

    def get_content_dict(self, url):
        response = self.client.get(url)
        try:
            content_dict = json.loads( response.content )
        except ValueError:
            self.fail("Did not receive any proper JSON from the server!")
        self.failUnlessEqual(response.status_code, 200)
        return content_dict

    def yml_file_search_test(self, config_type, search_term, expected_list):
        url = '/json/%s/search/%s' % ( config_type, search_term )
        expected = set(expected_list)
        content_dict = self.get_content_dict(url)
        name_set = self.get_field_value_set( content_dict, "name" )
        self.failUnlessEqual(name_set, expected)

    def test_search(self):
        test_dict  = { "client":  { "tes": ["tester1", "tester2"],
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

    def test_get_server_home(self):
        url = '/json/server/config'
        content_dict = self.get_content_dict(url)
        expected = {"server_home": "NULL"}
        self.failUnlessEqual(content_dict["server_home"], expected["server_home"])

        url = '/json/dbsync'
        response = self.client.post(path=url)
        self.failUnlessEqual(response.status_code, 200)

        expected = self.get_bombardier_yaml_dict()
        url = '/json/server/config'
        content_dict = self.get_content_dict(url)
        self.failUnlessEqual(content_dict["server_home"], expected["server_home"])

    def test_change_server_home(self):
        url = '/json/server/config'
        response = self.client.post(path=url, data={"server_home": "NO_PATH"})
        content_dict = json.loads( response.content )
        expected = {"server_home": u"NO_PATH"}
        self.failUnlessEqual(content_dict["server_home"], expected["server_home"])

    def test_merged(self):
        url = '/json/server/config'
        test_home = os.path.join(os.getcwd(), "configs", "fixtures")
        response = self.client.post(path=url, data={"server_home": test_home})
        client_name = "tester1"
        url = '/json/merged/name/%s' % client_name
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

