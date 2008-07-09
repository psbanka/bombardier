import mock, sys
import ConfigParser, StringIO
from bombardier.staticData import *
import bombardier.Exceptions as Exceptions
import bombardier.miniUtility as miniUtility
import yaml

class InvalidConfigData(Exception):
    def __init__(self, section, t1, t2):
        e = Exception()
        Exception.__init__(e)
        self.section = section
        self.t1      = t1
        self.t2      = t2
    def __repr__(self):
        return "Unable to read configuration data: %s. [expected %s, got %s]" % (self.section, self.t1, self.t2)
    def __str__(self):
        return "Unable to read configuration data: %s. [expected %s, got %s]" % (self.section, self.t1, self.t2)

class MockPackage(mock.Mock):
    def __init__(self):
        mock.Mock.__init__(self)
        self.dependencies = []
        self.priority = 0
        self.console = False
        self.preboot = False
        self.processResults = OK
        self.fullName = ''
    def process(self, installList):
        mock.Mock.__getattr__(self, 'process')(installList)
        return self.processResults
        
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
    def getMetaData(self, pkgName):
        mock.Mock.__getattr__(self, 'getMetaData')(pkgName)
        metaData = MockMetaData(self.packages.get(pkgName))
        assert metaData.alive() == "YES"
        return metaData
    def getPackage(self, packageName, checksum=""):
        mock.Mock.__getattr__(self, 'getPackage')(packageName, checksum)
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
        
class NonClosingStringIO(StringIO.StringIO):
    def close(self):
        pass
    
class MockFilesystem(mock.Mock):
    def __init__(self):
        mock.Mock.__init__(self, {"getBinaryDataFromFilePath": "data"})
        self.directories = []
        self.files = []
        self.globPaths = {}
        self.badcmds = []
        self.writeFile = {}
        self.readFile = {}
        self.gzipFiles  = StringIO.StringIO()
        self.tarObject  = MockTar()
        self.environ    = {}
        self.yamlData   = {}
        self.getAllIndex = -1
        self.getAllData = [None] * 10
        self.stats = []
        self.packageGroupsIndex = -1
        self.packageGroups = []
        self.packagesFromFileIndex = -1
        self.packagesFromFile = {}
        self.status = {}
        self.hostname = "testsystem"
    def make_wfrable(self):
        for file_name in self.writeFile:
            data = self.writeFile[file_name].getvalue()
            self.readFile[file_name] = NonClosingStringIO(data)

    def glob(self, path):
        mock.Mock.__getattr__(self, 'glob')(path)
        #if not path in self.globPaths:
            #print "I AM BEING ASKED FOR:", path
            #print "I DON'T HAVE IT"
            #raise Exception
        output = self.globPaths.get(path, [])
        return output
    def mkYaml(self, data):
        mock.Mock.__getattr__(self, 'mkYaml')(data)
        self.readFiles = [StringIO.StringIO(yaml.dump(data))]
    def reset(self):
        mock.Mock.__getattr__(self, 'reset')()
        self.getAllIndex = -1
        self.packageGroupsIndex = -1
        self.packagesFromFileIndex = 1
        self.writeFileIndex = 0
        self.readFileIndex = 0
        self.writeFiles = [StringIO.StringIO()]
    def getHostname(self):
        mock.Mock.__getattr__(self, 'getHostname')()
        return self.hostname
    def updateProgress(self, dictionary, server, overwrite=False):
        mock.Mock.__getattr__(self, 'updateProgress')(dictionary, server, overwrite)
        self.status = miniUtility.integrate(self.status, dictionary, overwrite)
        return
    def watchForTermination(self, sleepTime):
        mock.Mock.__getattr__(self, 'watchForTermination')(sleepTime)
        return OK
    def getcwd(self):
        return "C:\\"
    def gzipOpen(self, path):
        mock.Mock.__getattr__(self, 'gzipOpen')(path)
        return self.gzipfile
    def tarOpen(self, path, mode):
        mock.Mock.__getattr__(self, 'tarOpen')(path, mode)
        return self.tarObject
    def open(self, path, mode='r'):
        mock.Mock.__getattr__(self, 'open')(path, mode)
        if mode.startswith('w'):
            self.writeFile[path] = NonClosingStringIO()
            return self.writeFile[path]
        else:
            if not path in self.readFile:
                self.readFile[path] = NonClosingStringIO()
            return self.readFile[path]
    def isdir(self, path):
        mock.Mock.__getattr__(self, 'isdir')(path)
        if path in self.directories:
            return True
        return False
    def isfile(self, path):
        mock.Mock.__getattr__(self, 'isfile')(path)
        #print ">>>",path
        if path in self.files:
            return True
        if path in self.writeFile:
            return True
        if path in self.readFile:
            return True
        if path in self.yamlData:
            return True
        return False
    def execute(self, cmd, errorString='', dieOnExit=0,
                captureOutput=False, workingDirectory='.'):
        mock.Mock.__getattr__(self, 'execute')(cmd, workingDirectory)
        for badcmd in self.badcmds:
            if cmd.startswith(badcmd):
                return FAIL
        return OK
    def listdir(self, path):
        mock.Mock.__getattr__(self, 'isfile')(path)
        return self.directoryData
    def mkdir(self, path):
        mock.Mock.__getattr__(self, 'mkdir')(path)
        self.directories.append(path)
        return None
    def rmtree(self, path):
        mock.Mock.__getattr__(self, 'rmtree')(path)
        return None
    def chdir(self, path):
        mock.Mock.__getattr__(self, 'chdir')(path)
        return None
    def createTar(self, filename, path):
        mock.Mock.__getattr__(self, 'createTar')(filename, path)
        return None
    def loadYaml(self, filepath):
        mock.Mock.__getattr__(self, 'loadYaml')(filepath)
        if not self.yamlData.has_key(filepath):
            raise Exceptions.NoYamlData, filepath
        return self.yamlData.get(filepath)
    def getAllFromFile(self, patternStr, fileName):
        mock.Mock.__getattr__(self, 'getAllFromFile')(patternStr, fileName)
        self.getAllIndex += 1
        return self.getAllData[self.getAllIndex]
    def stat(self, path):
        mock.Mock.__getattr__(self, 'stat')(path)
        return self.stats
    def getPackageGroups(self, config):
        mock.Mock.__getattr__(self, 'getPackageGroups')(config)
        self.packageGroupsIndex += 1
        return self.packageGroups[self.packageGroupsIndex]
    def getPackagesFromFile(self, filename, stripVersionFromName = False):
        mock.Mock.__getattr__(self, 'getPackageFromFile')(filename, stripVersionFromName)
        return self.packagesFromFile[filename]
    def getProgressData(self, stripVersionFromName = False):
        mock.Mock.__getattr__(self, 'getProgressData')(stripVersionFromName)
        if self.status.has_key("install-progress"):
            progressData = self.status.get("install-progress")
            if stripVersionFromName:
                return miniUtility.stripVersionFromKeys(progressData)
            return progressData
        return {}
    def moveToDestination(self, destDir, filename):
        mock.Mock.__getattr__(self, 'moveToDestination')(destDir, filename)
        return OK
    
class MockYaml(mock.Mock):
    def __init__(self):
        mock.Mock.__init__(self)
    def next(self):
        mock.Mock.__getattr__(self, 'next')
        return self.data

class MockWindows(mock.Mock):
    def __init__(self):
        mock.Mock.__init__(self)
        self.queryIndex = -1
        self.registryQueries = [""]
        self.credentialIndex = -1
        self.credentialResults = []
        self.makeUserIndex = -1
        self.makeUserResults = []
        self.multipleObjectsIndex = -1
        self.multipleObjects = []
        self.serviceStatusIndex = -1
        self.serviceStatusResults = []
        self.readFiles = []
        self.readFileIndex = -1
        self.testConsoleValue = OK
        self.readPipeIndex = -1
        self.readPipe = []
        self.installablePackages = []
        self.uninstallablePackages = []
        self.verifiablePackages = []
    def reset(self):
        mock.Mock.__getattr__(self, 'reset')()
        self.queryIndex = -1
        self.credentialIndex = -1
        self.makeUserIndex = -1
        self.multipleObjectsIndex = -1
        self.serviceStatusIndex = -1
        self.readFileIndex = -1
        self.readPipeIndex = -1
    def testConsole(self):
        mock.Mock.__getattr__(self, "testConsole")()
        return self.testConsoleValue
    def connectPipe(self, pipeName, event):
        mock.Mock.__getattr__(self, "connectPipe")(pipeName, event)
        return 'a', 'b'
    def run(self, fullCmd, workingDirectory, console = False):
        mock.Mock.__getattr__(self, "run")(fullCmd, workingDirectory, console)
        packageName = fullCmd.split(' ')[0].split(os.sep)[-3]
        script = fullCmd.split(' ')[0].split(os.sep)[-1]
        action = None
        if script == "installer.py":
            if packageName in self.installablePackages:
                return OK
            else:
                return FAIL
        if script == "uninstaller.py":
            if packageName in self.uninstallablePackages:
                return OK
            else:
                return FAIL
        if script == "verify.py":
            if packageName in self.verifiablePackages:
                return OK
            else:
                return FAIL
        return FAIL

    def ReadPipe(self, pipeName, timeout, waitHandles, overlappedHe):
        mock.Mock.__getattr__(self, "ReadPipe")(pipeName, timeout, waitHandles, overlappedHe)
        self.readPipeIndex += 1
        command = None
        try:
            command = self.readPipe[self.readPipeIndex]
        except IndexError:
            raise Exceptions.ServiceShutdown
        return command

    def ReadFilePlus(self, handle, bytes):
        mock.Mock.__getattr__(self, "ReadFile")(handle, bytes)
        self.readFileIndex += 1
        return OK, self.readFiles[self.readFileIndex]
        
    def queryKey(self, path):
        mock.Mock.__getattr__(self, 'queryValue')(path)
        self.queryIndex += 1
        if self.queryIndex > len(self.registryQueries):
            self.registryQueries.append("")
        return self.registryQueries[self.queryIndex], 1

    def testCredentials(self, username, domain, password):
        mock.Mock.__getattr__(self, 'testCredentials')(username, domain, password)
        self.credentialIndex += 1
        return self.credentialResults[self.credentialIndex]

    def mkServiceUser(self, username, domain, password, comment = "", local=False):
        mock.Mock.__getattr__(self, 'mkServiceUser')(username, domain, password, comment, local)
        self.makeUserIndex += 1
        results = self.makeUserResults[self.makeUserIndex]
        return results

    def WaitForMultipleObjects(self, readers, writers, timeout):
        mock.Mock.__getattr__(self, 'WaitForMultipleObjects')(readers, writers, timeout)
        self.multipleObjectsIndex += 1
        results = self.multipleObjects[self.multipleObjectsIndex]
        return results
    def stopService(self, serviceName="BombardierClient"):
        mock.Mock.__getattr__(self, "stopService")(serviceName)
        return OK
    def serviceStatus(self, serviceName="BombardierClient"):
        mock.Mock.__getattr__(self, "serviceStatus")(serviceName)
        self.serviceStatusIndex += 1
        results = self.serviceStatusResults[self.serviceStatusIndex]
        return results
        

class MockServer(mock.Mock):
    def __init__(self):
        mock.Mock.__init__(self, {"serverLog": OK})
        self.bom = {}
        self.output = {}
        self.packageData = {}
        self.configData = {}
    def reset(self):
        mock.Mock.__getattr__(self, 'reset')()
        self.yamlIndex = -1
        self.serviceRequestIndex = -1
    def bomRequest(self, bomName):
        mock.Mock.__getattr__(self, 'bomRequest')(bomName)
        return self.bom[bomName]
    def configRequest(self):
        mock.Mock.__getattr__(self, 'configRequest')()
        return self.configData
    def packageRequest(self, packageName):
        mock.Mock.__getattr__(self, 'packageRequest')(packageName)
        return self.packageData[packageName]

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

    def parseSection(self, sectionString, default, optional):
        sections = sectionString.split('.')
        d = self.data
        for section in sections:
            try:
                d = d[section]
            except:
                d = None
                break
        if d == None:
            if not optional:
                raise InvalidConfigData("Option %s not found" % sectionString, None, None)
            d = default
        return d

    def getobj(self, sectionString, default, expType, optional):
        value = self.parseSection(sectionString, default, optional)        
        if type(expType) == type("string"):
            if type(value) == type(1234) or type(value) == type(123.32):
                value = str(value)
        if type(value) == type(expType):
            return value
        print "value: ", value
        raise InvalidConfigData(sectionString, type(value), type(expType))

    def listobj(self, sectionString, default=[], optional=True):
        return self.getobj(sectionString, default, [], optional)

    def string(self, sectionString, default='', optional=True):
        return self.getobj(sectionString, default, "string", optional)

    def integer(self, sectionString, default=1, optional=True):
        return self.getobj(sectionString, default, 1, optional)

    def dictionary(self, sectionString, default={}, optional=True):
        return self.getobj(sectionString, default, {}, optional)


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

    def saveHash(self, path):
        return OK

    def checkHash(self, path):
        # Same as real object, except we load from a saved
        # dictionary instead of a yaml file
        oldConfig = {}
        for key in self.savedYamlData.keys():
            if key in path:
                oldConfig = self.savedYamlData[key]
                break
        newConfig = miniUtility.hashDictionary(self.data)
        oldConfig = miniUtility.hashDictionary(oldConfig)
        return miniUtility.diffDicts(oldConfig, newConfig, checkValues=True)

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

class MockBombardierServer(MockConfig):
    def __init__(self, hostName):
        MockConfig.__init__(self)
        self.hostName = hostName
    def process(self, action, packageList, scriptName, debug):
        return OK, []
    def rsync(self, local_replica, remote_path, action):
        return OK

class MockSmtp(mock.Mock):
    def __init__(self):
        mock.Mock.__init__(self)
        self.messages = []
        self.hosts = []

    def connect(self, host):
        mock.Mock.__getattr__(self, 'connect')(host)
        self.hosts.append(host)

    def sendmail(self, source, dest_list, message):
        mock.Mock.__getattr__(self, 'sendmail')(source, dest_list, message)
        self.messages.append({"source":source, "dest":dest_list, "message":message})
        return OK

class MockSql(mock.Mock):
    def __init__(self):
        mock.Mock.__init__(self)
        self.badQueries = []
        self.regexOutput = {}

    def query(self, qstr, database = None, dedicated = False):
        mock.Mock.__getattr__(self, 'query')(qstr, database, dedicated)
        for q in self.badQueries:
            if qstr.startswith(q):
                return FAIL
        return OK

    def queryWithOutput(self, qstr, database=None, dedicated=False, noHeader=False):
        mock.Mock.__getattr__(self, 'queryWithOUtput')(qstr, database, dedicated)
        return OK, ''

    def queryRegex(self, qstr, regex):
        return self.regexOutput


class MockLogger:
    def __init__(self, debug=False):
        self.data = {"debug":[], "info":[],"warning":[], "error":[], "critical":[]}
        self.debugSetting = debug
    def __repr__(self):
        return "MOCK-LOGGER"
    def debug(self, string):
        self.data["debug"].append(string)
        if self.debugSetting: 
            print "DEBUG:",string
            sys.stdout.flush()
    def info(self, string):
        self.data["info"].append(string)
        if self.debugSetting: 
            print "INFO:",string
            sys.stdout.flush()
    def warning(self, string):
        self.data["warning"].append(string)
        if self.debugSetting: 
            print "WARNING:",string
            sys.stdout.flush()
    def error(self, string):
        self.data["error"].append(string)
        if self.debugSetting: 
            print "ERROR:",string
            sys.stdout.flush()
    def critical(self, string):
        self.data["critical"].append(string)
        if self.debugSetting: 
            print "CRITICAL:",string
            sys.stdout.flush()
    def dump(self):
        types = ["debug", "info", "warning", "error", "critical"] 
        print "\n======================LOG======================="
        for type in types:
            for line in self.data[type]:
                print "%s: %s" % (type, line)
        print "======================LOG======================="


if __name__ == "__main__":
    filesystem = MockFilesystem()
    filesystem.directories = ["there"]
    filesystem.files = []
    assert filesystem.isdir("hello") == False
    assert filesystem.isdir("there") == True
    assert filesystem.isfile("hello") == False
