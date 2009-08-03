import re, os, yaml
from bombardier_core.miniUtility import getSpkgPath
import sys
import Config
from bombardier_core.Filesystem import Filesystem
from bombardier_core.static_data import OK, FAIL, LAST_REPORT
import Repository

def doubleEscape(oldString):
    outString = ''
    for i in oldString:
        if i == '\\':
            outString += "\\\\"
        else:
            outString += i
    return outString

class SpkgException( Exception ):
    def __init__(self, errorMessage=""):
        self.errorMessage = str(errorMessage)
        e = Exception()
        Exception.__init__(e)
    def __str__(self):
        return self.errorMessage
    def __repr__(self):
        return self.errorMessage

def getInstance():
    path = os.getcwd()
    spkgPath = getSpkgPath()
    subDir = path.split(spkgPath)[1]
    instanceName = subDir.split(os.path.sep)[1]
    return instanceName

def getConfig():
    instanceName = getInstance()
    filesystem = Filesystem()
    _repository = Repository.Repository(filesystem, instanceName)
    config = Config.Config(filesystem, instanceName)
    return config

def mainBody(pkgVersion, cls):
    from bombardier_core.Logger import logger, addStdErrLogging
    config = getConfig()
    ha = None
    if pkgVersion < 4:
        ha = cls(config, logger=logger)
    else:
        ha = cls(config, logger)
    addStdErrLogging()
    action = sys.argv[-1].lower()
    status = OK
    if action == "install":
        status = ha.installer()
    elif action == "uninstall":
        status = ha.uninstaller()
    elif action == "configure":
        status = ha.configure()
    elif action == "verify":
        status = ha.verify()
    else:
        print "Unknown action %s" % action
        status = FAIL
    sys.exit(status)

def main(cls):
    mainBody(3, cls)

def mainV4(cls):
    mainBody(4, cls)

def dumpReportV4(report, logger):
    instanceName = getInstance()
    outputPath = os.path.join(getSpkgPath(), instanceName, "output")
    if not os.path.isdir(outputPath):
        os.makedirs(outputPath)
    scriptName = sys.argv[-1].split(".py")[0]
    outputFile = "%s-output.yml" % scriptName
    yamlString = yaml.dump(report)
    open(os.path.join(outputPath, outputFile), 'w').write(yamlString)
    for line in yamlString.split('\n'):
        logger.info("==REPORT==:%s" % line)

def dumpReport(report, config, logger):
    dumpReportV4(report, logger)

class SpkgV4:

    def __init__(self, config, logger = None, filesystem = Filesystem()):
        self.thisPackagesName = self._getname()
        self.filesystem = filesystem
        self.stderr     = True
        self.server     = None
        self.dbInstance = None
        self.port       = None
        if logger == None:
            import Logger
            self.logger = Logger.logger
        else:
            self.logger = logger
            self.stderr = False
        self.lastReport = {}

    def loadLastReport(self):
        self.info("Loading last report data...")
        self.report = {"pending-changes": [], "executed-changes":[], "authorized": {},
                       "unauthorized": {}, "ignoredUsers": {}}
        self.lastReport = {"pending-changes": []}
        if os.path.isfile(LAST_REPORT):
            self.lastReport = yaml.load(open(LAST_REPORT, 'r').read())
        pendingChanges = len(self.lastReport["pending-changes"])
        if pendingChanges == 0:
            self.info("There are no pending changes.")
        else:
            self.warning("There are %d pending changes." % pendingChanges)

    def checkAction(self, actionString):
        if actionString in self.lastReport["pending-changes"]:
            self.info("PERFORMING: %s" % actionString)
            self.report["executed-changes"].append(actionString)
            return OK
        else:
            self.info("QUEUING: %s" % actionString)
            self.report["pending-changes"].append(actionString)
        return FAIL

    def writeReport(self):
        self.info("Writing report...")
        open(LAST_REPORT, 'w').write(yaml.dump(self.report))

    def checkStatus(self, status, errMsg="FAILED"):
        if status != OK:
            raise SpkgException(errMsg)

    def system(self, command, errMsg=""):
        errMsg = command
        self.checkStatus( os.system( command ), errMsg )

    def debug(self, string):
        self.logger.debug("[%s]|%s" % (self.thisPackagesName, string))
    def info(self, string):
        self.logger.info("[%s]|%s" % (self.thisPackagesName, string))
    def warning(self, string):
        self.logger.warning("[%s]|%s" % (self.thisPackagesName, string))
    def error(self, string):
        self.logger.error("[%s]|%s" % (self.thisPackagesName, string))
    def critical(self, string):
        self.logger.critical("[%s]|%s" % (self.thisPackagesName, string))

    def _getname(self):
        cwd = os.getcwd()
        path = cwd.split(os.sep)
        if path[-1] == "injector" or path[-1] == "scripts":
            return path[-2]
        return path[-1]

    def makeConnectionString(self):
        dataSource = ''
        if hasattr(self, "server") and type(self.server) == type("string"):
            dataSource = self.server.strip()
            if hasattr(self, "dbInstance") and type(self.dbInstance) == type("string"):
                dbInstance = self.dbInstance.strip()
                if dbInstance != '':
                    dataSource += "\\"+dbInstance
                if hasattr(self, "port") and type(self.port) == type("string"):
                    port = self.port.strip()
                    if port != '':
                        dataSource += ","+port
        return dataSource

    def _abstracty(self):
        self.error("Attempting to call an abstract method")
        return FAIL

    def configure(self):
        return self._abstracty()

    def verify(self):
        return self._abstracty()

    def installer(self):
        return self._abstracty()

    def uninstaller(self):
        return self._abstracty()

    def modifyTemplateString(self, inputString, outputFile, encoding=None, processEscape = False):
        varMatch = re.compile("\%\((.*?)\)s")
        variables = varMatch.findall(inputString)
        status = OK
        output = []
        for line in inputString.split('\n'):
            variables = varMatch.findall(line)
            configDict = {}
            if len(variables) == 0:
                output.append(line)
                continue
            for variable in variables:
                if hasattr(self, variable):
                    configValue = getattr(self, variable)
                    if processEscape:
                        configDict[variable] = doubleEscape(configValue)
                    else:
                        configDict[variable] = configValue
                else:
                    self.error('A variable was found in template "%s" for which there'\
                               " is no configuration value (variable: %s)" % (outputFile, variable))
                    configDict[variable] = 'UNKNOWN_TEMPLATE_VALUE'
                    status = FAIL
            if configDict == {}:
                output.append(line)
            else:
                output.append(line % configDict)
        outputData = '\n'.join(output)

        if encoding == None:
            self.filesystem.open(outputFile, 'w').write(outputData)
        else:
            self.filesystem.open(outputFile, 'wb').write(outputData.encode( encoding ))
        return status

    
    def modifyTemplate(self, inputFile, outputFile, encoding=None, processEscape = False):
        if encoding == None:
            inputString = self.filesystem.open(inputFile, 'r').read()
        else:
            inputString = unicode( self.filesystem.open(inputFile, 'rb').read(), encoding )
        self.info("Template: " + inputFile )
        self.info("Created: " + outputFile )
        return self.modifyTemplateString(inputString, outputFile, encoding, processEscape)

class Spkg(SpkgV4):
    def __init__(self, config, filesystem = Filesystem(), logger = None):
        SpkgV4.__init__(self, config, logger, filesystem)

