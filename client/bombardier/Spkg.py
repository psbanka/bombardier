import re, string
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

    def __init__(self, config, logger = Logger.Logger(), filesystem = Filesystem.Filesystem()):
        self.filesystem   = filesystem
        self.logger       = logger

    def makeConnectionString(self):
        dataSource = ''
        if hasattr(self, "server"):
            dataSource = self.server.strip()
            if hasattr(self, "instance"):
                instance = self.instance.strip()
                dataSource += "\\"+instance
                if hasattr(self, "port"):
                    port=self.port.strip()
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
                    self.logger.error('A variable was found in template "%s" for which there'\
                                      " is no configuration value (variable: %s)" % (outputFile, variable))
                    configDict[variable] = 'UNKNOWN_TEMPLATE_VALUE'
                    status = FAIL
            if configDict == {}:
                output.append(line)
            else:
                output.append(line % configDict)
        outputData = string.join(output, '\n')

        if encoding == None:
            self.filesystem.open(outputFile, 'w').write(outputData)
        else:
            self.filesystem.open(outputFile, 'wb').write(outputData.encode( encoding ))
        self.logger.info( "\nCreated: " + outputFile + "\nTemplate: " + inputFile + "\n" )
        return status
