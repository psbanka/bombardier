import mock
import ConfigParser, StringIO
import os
from bombardier_core.static_data import OK, FAIL
import bombardier_core.mini_utility as mini_utility

class MockPackage(mock.Mock):
    def __init__(self):
        mock.Mock.__init__(self)
        self.dependencies = []
        self.priority = 0
        self.console = False
        self.preboot = False
        self.processResults = OK
        self.fullName = ''
    def process(self, install_list):
        mock.Mock.__getattr__(self, 'process')(install_list)
        return self.processResults
    def get_configuration(self):
        return self.meta_data.data.get("configuration", {})
    def get_path(self):
        return "no_path"
        
class MockChain(mock.Mock):
    def __init__(self):
        mock.Mock.__init__(self)

class MockMetaData(mock.Mock):
    def __init__(self, data):
        mock.Mock.__init__(self)
        self.data = data

    def alive(self):
        mock.Mock.__getattr__(self, 'alive')()
        return "YES"

    def has_key(self, section):
        mock.Mock.__getattr__(self, 'has_key')(section)
        if self.data.has_key(section):
            return True
        return False

    def __getitem__(self, key):
        mock.Mock.__getattr__(self, '__getitem__')(key)
        return self.data[key]

    def __setitem__(self, key, value):
        mock.Mock.__getattr__(self, '__setitem__')(key)
        self.data[key] = value

    def get(self, section, option, default=None):
        mock.Mock.__getattr__(self, 'get')(section, option, default)
        try:
            return self.data[section][option]
        except:
            if default:
                return default
            raise ConfigParser.NoOptionError(section, option)

class MockRepository(mock.Mock):
    def __init__(self, packages):
        mock.Mock.__init__(self)
        self.packages = packages
    def get_meta_data(self, pkn):
        mock.Mock.__getattr__(self, 'get_meta_data')(pkn)
        meta_data = MockMetaData(self.packages.get(pkn))
        assert meta_data.alive() == "YES"
        return meta_data
    def unzip_type_4(self, pkg_path, full_name):
        assert 1 == 0
    def get_type_4(self, full_name):
        start_dir = os.getcwd()
        os.chdir("spkg/TEST_INSTANCE/packages")
        cmd = "tar -xzvf ../../../packages/%s.spkg &>/dev/null"
        cmd = cmd % full_name
        os.system(cmd)
        os.chdir(start_dir)
    def hunt_and_explode(self):
        assert 1 == 0
    def make_symlinks(self, pkg_path, info_dict, full_name):
        assert 1 == 0
    def determine_pkg_version(self, pkn):
        assert 1 == 0
    def get_package(self, pkn, checksum=""):
        mock.Mock.__getattr__(self, 'getPackage')(pkn, checksum)
        return "OK"

class MockBombardier(mock.Mock):
    def __init__(self):
        mock.Mock.__init__(self)
        self.reconcileStatus = OK
        self.verifyStatus = {"pkg1": OK}
    def reconcileSystem(self):
        mock.Mock.__getattr__(self, 'reconcileSystem')()
        return self.reconcileStatus
    def verifySystem(self):
        mock.Mock.__getattr__(self, 'verifySystem')()
        return self.verifyStatus

class MockTar(mock.Mock):
    def __init__(self):
        mock.Mock.__init__(self)
        self.data  = {}
        self.index = 0
    def extract(self, tarinfo):
        mock.Mock.__getattr__(self, 'extract')(tarinfo)
        return self.data[tarinfo]
    def __getitem__(self, index):
        mock.Mock.__getattr__(self, '__getitem__')(index)
        return self.data.keys(index)
    def __iter__(self):
        mock.Mock.__getattr__(self, '__iter__')()
        return self
    def next(self):
        mock.Mock.__getattr__(self, 'next')()
        try:
            output = self.data.keys()[self.index]
        except IndexError:
            raise StopIteration
        self.index += 1
        return output

class MockFile(mock.Mock):
    def __init__(self):
        mock.Mock.__init__(self)

    def seek(self, position):
        mock.Mock.__getattr__(self, 'seek')(position)

    def tell(self):
        mock.Mock.__getattr__(self, 'tell')()
        return 0

    def readline(self):
        mock.Mock.__getattr__(self, 'readline')()
        raise "DoneTestingException"
        
    
class MockYaml(mock.Mock):
    def __init__(self):
        mock.Mock.__init__(self)
    def next(self):
        mock.Mock.__getattr__(self, 'next')
        return self.data


class MockServer(mock.Mock):
    def __init__(self):
        mock.Mock.__init__(self, {"serverLog": OK})
        self.bom = {}
        self.output = {}
        self.packageData = {}
        self.configData = {}
    def reset(self):
        mock.Mock.__getattr__(self, 'reset')()
        self.jsonIndex = -1
        self.serviceRequestIndex = -1
    def bomRequest(self, bomName):
        mock.Mock.__getattr__(self, 'bomRequest')(bomName)
        return self.bom[bomName]
    def configRequest(self):
        mock.Mock.__getattr__(self, 'configRequest')()
        return self.configData
    def packageRequest(self, pkn):
        mock.Mock.__getattr__(self, 'packageRequest')(pkn)
        return self.packageData[pkn]

class MockConfig:
    def __init__(self):
        self.repository = {"address":"http://127.0.0.1"}
        self.sections = []
        self.console = False
        self.automated = False
        self.data = {}
        self.savedYamlData = {}
    def reset(self):
        pass
    def __repr__(self):
        return "MOCK-CONFIG"
    def has_section(self, sectionName):
        if sectionName in self.sections:
            return True
        return False
    def freshen(self):
        return OK
    def keys(self):
        return self.data.keys()
    def get(self, section, option, default=""):
        try:
            return str(self.data[section][option])
        except KeyError:
            if default:
                return default
            raise ConfigParser.NoOptionError
    def get_dict(self, section, option, default={}):
        try:
            result = self.data[section][option]
            if not result.__class__ == {}.__class__:
                raise TypeError
            return result
        except KeyError:
            if default:
                return default
            raise ConfigParser.NoOptionError
    def __setitem__(self, key, item):
        self.data[key] = item

    def get_instance(self):
        return "TEST_INSTANCE"

    def saveHash(self, path):
        return OK

    def checkHash(self, path):
        # Same as real object, except we load from a saved
        # dictionary instead of a json file
        oldConfig = {}
        for key in self.savedYamlData.keys():
            if key in path:
                oldConfig = self.savedYamlData[key]
                break
        newConfig = mini_utility.hash_dictionary(self.data)
        oldConfig = mini_utility.hash_dictionary(oldConfig)
        return mini_utility.diff_dicts(oldConfig, newConfig, checkValues=True)

    def getPackageGroups(self): # same as real class
        groups   = []
        packages = []
        if self.data.has_key("bom"):
            groups = self.data["bom"]
        if self.data.has_key("packages"):
            packages = self.data["packages"]
        return groups, packages
    def autoLogin(self):
        return OK

