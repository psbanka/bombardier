#!c:\Python24\python.exe

import unittest, StringIO, sets, yaml, sys, os
import Tcommon

sys.path = [os.path.join("..", "client"), os.path.join('..', 'spkgDir')] + sys.path

import bombardier.Config

from bombardier.staticData import *
from testdata import *
import MockObjects

class ConfigTest(unittest.TestCase):

    def setUp(self):
        self.filesystem                 = MockObjects.MockFilesystem()
        self.server                     = MockObjects.MockServer()
        self.windows                    = MockObjects.MockWindows()
        self.filesystem.yamlData        = {SERVERDATA_FILE: repodata1}
        self.config = bombardier.Config.Config(self.filesystem,
                                               self.server, self.windows)

    def tearDown(self):
        pass

    def testDownloadConfig(self):
        self.server.yamlRequests = [configData1]
        self.config.downloadConfig("testsystem")
        assert self.config.data["body"]["residence"] == "portland"
        scalls = self.server.getAllCalls()
        assert len(scalls) == 1, scalls
        assert `scalls[0]` == "serviceYamlRequest('deploy/client/testsystem.yml', {}, None, False, False)", scalls[0]

    def testDownloadConfigParents(self):
        self.server.yamlRequests = [configData2, margaretData, billData, frankData]
        status = self.config.downloadConfig("testsystem", False)
        assert status == OK, status
        assert self.config.data["body"]["residence"] == "portland", self.config.data["body"]
        assert self.config.data["system"]["ipaddress"] == "22.22.22.22", self.config.data["system"]
        assert self.config.data["system"]["serviceuser"] == "drillbit", self.config.data["system"]
        assert self.config.data["system"]["servicepasswd"] == "foomanchoo", self.config.data["system"]
        scalls = self.server.getAllCalls()
        assert len(scalls) == 4, scalls
        assert `scalls[0]` == "serviceYamlRequest('deploy/client/testsystem.yml', {}, None, False, False)", `scalls[0]`
        assert `scalls[1]` == "serviceYamlRequest('deploy/include/margaret.yml', {}, None, False, False)", `scalls[1]`
        assert `scalls[2]` == "serviceYamlRequest('deploy/include/bill.yml', {}, None, False, False)", `scalls[2]`
        assert `scalls[3]` == "serviceYamlRequest('deploy/include/frank.yml', {}, None, False, False)", `scalls[3]`

    def testFreshen(self):
        self.server.yamlRequests = [configData2, margaretData, billData, frankData,
                                    indexData, bombardierProjectData, testhw]
        self.config.freshen()
        scalls = self.server.getAllCalls()
        assert len(scalls) == 4, len(scalls)
        assert `scalls[0]` == "serviceYamlRequest('deploy/client/testsystem.yml', {}, None, False, False)"
        assert `scalls[1]` == "serviceYamlRequest('deploy/include/margaret.yml', {}, None, False, False)"
        assert `scalls[2]` == "serviceYamlRequest('deploy/include/bill.yml', {}, None, False, False)"
        assert `scalls[3]` == "serviceYamlRequest('deploy/include/frank.yml', {}, None, False, False)"
        assert self.config.username == "drillbit", self.config.username
        assert self.config.password == "foomanchoo", self.config.password
        assert self.config.domain   == ".", self.config.domain

    def testMakeConfigObject(self):
        self.config.data["foo"] = {"spam": "eggs"}
        self.config.makeConfigObject()
        assert self.config.get("foo", "spam") == 'eggs', self.config.get("foo", "spam")
        
    def testGetDict(self):
        self.config.data["foo"] = {"spam": {"eggs":"sausage", "spam":"eggs"}, "eggs": "spam" }
        self.config.makeConfigObject()
        assert self.config.get_dict("foo", "spam") == {"eggs":"sausage", "spam":"eggs"}
        try:
            self.config.get_dict("foo", "eggs") != "eggs"
            assert False
        except TypeError:
            assert True

    def testSaveHash(self):
        self.config.set("section", "option", 12)
        status = self.config.saveHash("foo")
        assert status == OK, "Failed to save hash values"
        rawdata = '\n'.join(self.filesystem.writeFiles[0].buflist)
        data = yaml.load(rawdata).next()
        assert data.has_key("section"), "data missing 'section'"
        assert data["section"].has_key("option"), "data missing 'option"
        assert data["section"]["option"] == "c20ad4d76fe97759aa27a0c99bff6710", \
               "Hash value is incorrect"

    def testCheckHash(self):
        savedConfig = {}
        savedConfig["section1"] = {}
        savedConfig["section1"]["option1"] = "c20ad4d76fe97759aa27a0c99bff6710"
        savedConfig["section1"]["option2"] = "c20ad4d76fe97759aa27a0c99bff6710"
        savedConfig["section2"] = {}
        savedConfig["section2"]["option"] = "c20ad4d76fe97759aa27a0c99bff6710"
        self.filesystem.readFiles = [StringIO.StringIO(yaml.dump(savedConfig))]
        #self.config.filesystem = self.filesystem
        self.config.set("section1", "option1", 13)
        self.config.set("section1", "option2", 12)
        difference = self.config.checkHash("foo")
        #print self.filesystem.getAllCalls()
        assert difference.has_key("section1"), difference
        assert difference.has_key("section2"), difference
        assert difference["section1"].has_key("option1"), difference
        assert difference["section2"].has_key("option")

    def testSet(self):
        status = self.config.set("spam", "eggs", "no")
        assert status == OK, "Unable to write a configuration value"
        # FIXME: have put reload the configuration data.
        assert self.config.get("spam", "eggs") == "no", "configuration did not set data properly"
        data = self.config.get("spam","eggs")
        assert data == "no", "config object did save data properly."

    def testFindIncludeList(self):
        list = bombardier.Config.findIncludeList(configData2)
        assert "bill" in list, list
        assert "margaret" in list, list

if __name__ == "__main__":
    tcommon = Tcommon.Tcommon()
    tcommon.setForTest()
    suite = unittest.TestSuite()
    #suite.addTest(ConfigTest("testIpConfig"))
    #suite.addTest(ConfigTest("testGetIpAddress"))
    #suite.addTest(ConfigTest("testGetNetworkList"))
    #suite.addTest(ConfigTest("testCheckHash"))
    suite.addTest(unittest.makeSuite(ConfigTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
    tcommon.unsetForTest()

