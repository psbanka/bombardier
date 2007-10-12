import re, os, yaml
import Filesystem
from staticData import *

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
        self.errorMessage = errorMessage
        e = Exception()
        Exception.__init__(e)
    def __str__(self):
        print self.errorMessage
    def __repr__(self):
        print self.errorMessage

def main(cls):
    import sys
    import bombardier.Config as Config
    import bombardier.Filesystem as Filesystem
    import bombardier.Server as Server
    import bombardier.OperatingSystem as OperatingSystem
    import bombardier.Logger as Logger
    from bombardier.staticData import OK, FAIL

    filesystem      = Filesystem.Filesystem()
    server          = Server.Server()
    operatingSystem = OperatingSystem.OperatingSystem()
    config          = Config.Config(filesystem, server, operatingSystem)

    Logger.addStdErrLogging()
    config.freshen()

    ha = cls(config, futurePackages = [], logger=Logger.logger)
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

class Spkg:

    def __init__(self, config, filesystem = Filesystem.Filesystem(), futurePackages = [], logger = None):
        self.thisPackagesName = self._getname()
        self.filesystem = filesystem
        self.futurePackages = futurePackages
        self.stderr = True
        if logger == None:
            import Logger
            self.logger = Logger.logger
        else:
            self.logger = logger
            self.stderr = False
        
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

    def setFuturePackages(self, packageList):
        self.futurePackages = packageList

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
        if hasattr(self, "server"):
            dataSource = self.server.strip()
            if hasattr(self, "instance"):
                instance = self.instance.strip()
                if instance != '':
                    dataSource += "\\"+instance
                if hasattr(self, "port"):
                    port = self.port.strip()
                    if port != '':
                        dataSource += ","+port
        return dataSource
    
    def modifyTemplate(self, inputFile, outputFile, encoding=None, processEscape = False):
        status = OK
        varMatch = re.compile("\%\((.*?)\)s")
        if encoding == None:
            configData = self.filesystem.open(inputFile, 'r').read()
        else:
            configData = unicode( self.filesystem.open(inputFile, 'rb').read(), encoding )

        variables = varMatch.findall(configData)
        output = []
        for line in configData.split('\n'):
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
        self.info( "\nCreated: " + outputFile + "\nTemplate: " + inputFile + "\n" )
        return status
