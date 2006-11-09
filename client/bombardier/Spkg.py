import re, os
import Filesystem
import Logger
from staticData import *

def doubleEscape(oldString):
    outString = ''
    for i in oldString:
        if i == '\\':
            outString += "\\\\"
        else:
            outString += i
    return outString

class Spkg:

    def __init__(self, config, filesystem = Filesystem.Filesystem()):
        Logger.addStdErrLogging()
        self.thisPackagesName = self._getname()
        self.filesystem = filesystem
        
    def debug(self, string):
        Logger.debug("[%s]|%s" % (self.thisPackagesName, string))
    def info(self, string):
        Logger.info("[%s]|%s" % (self.thisPackagesName, string))
    def warning(self, string):
        Logger.warning("[%s]|%s" % (self.thisPackagesName, string))
    def error(self, string):
        Logger.error("[%s]|%s" % (self.thisPackagesName, string))
    def critical(self, string):
        Logger.critical("[%s]|%s" % (self.thisPackagesName, string))

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
