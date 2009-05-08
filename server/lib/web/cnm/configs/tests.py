from binascii import b2a_base64
from datetime import datetime
from django.core import serializers
from django.test import TestCase
from django.utils.functional import curry
import django.utils.simplejson as json
import webbrowser, re
import os

class BasicTest(TestCase):

    fixtures = ['init.json']
        
    def setUp(self):
        pass

    def get_field_value_set(self, test_dict, value):
        return set([ i["fields"][value] for i in test_dict ])

    def generic_response_test(self, resp):
        self.failUnlessEqual(resp.status_code, 200)
        self.failUnlessEqual(resp.content.find('secret'), -1)

    def yml_file_search_test(self, config_type, search_term, expected_list):
        url = '/json/%s/search/%s' % ( config_type, search_term )
        expected = set(expected_list)
        response = self.client.get(url)
        content_dict = json.loads( response.content )
        name_set = self.get_field_value_set( content_dict, "name" )
        self.failUnlessEqual(name_set, expected)
        self.generic_response_test(response)

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


