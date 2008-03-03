import glob
import PinshCmd
import BomHostField
from commonUtil import *


class ScpHostField(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, name = "scpHostName")
        self.helpText = "<path>\tA path to a remote location. Follows pattern: <hostname>:/path/to/file"
        self.bomHostField = BomHostField.BomHostField()
        self.cmdOwner = 0
        self.pathCache = {}
        self.tokenDelimiter = ''

    def checkPossiblePathCache(self, hostName, checkPath):
        basePath = '/'.join(checkPath.split('/')[:-1])
        if not basePath in self.pathCache:
            p = mode.getBomConnection(hostName)
            p.freshen()
            remotePaths = p.checkPossiblePaths(basePath+'/')
            self.pathCache[basePath] = remotePaths
        returnValue = []
        for path in self.pathCache[basePath]:
            if checkPath in path:
                returnValue.append(hostName+":"+path)
        return returnValue
        
    def getPossibleHostNames(self, dest):
        if ':' in dest:
            target = dest.split(':')[0]
        else:
            target = dest
        possibleHostNames = self.bomHostField.name([target], 0)
        return possibleHostNames

    def name(self, tokens, index):
        dest = tokens[index]
        hostNames = self.getPossibleHostNames(dest)
        if ':' in dest:
            if len(hostNames) != 1:
                return []
            hostName = hostNames[0]
            path = dest.split(':')[1]
            if not path:
                return [hostName+':']
            return self.checkPossiblePathCache(hostName, path)
        else:
            if hostNames:
                return hostNames
            else:
                possibleLocalNames = glob.glob(dest+"*")
                if dest == '.':
                    possibleLocalNames.append('.')
                if os.path.isdir(dest):
                    possibleLocalNames.append(dest)
                return possibleLocalNames
        return []

    def match(self, tokens, index):
        possibleMatches = self.name(tokens, index)
        if not possibleMatches:
            return NO_MATCH, 1
        if len(possibleMatches) > 1:
            return PARTIAL, 1
        return COMPLETE, 1

if __name__ == "__main__":
    # A server must be enabled for the tests to work properly.
    from libTest import startTest, runTest, endTest
    hostField = ScpHostField()
    status = OK
    startTest()
    status = runTest(hostField.name, [["bigap:/usr/l"], 0], ['bigap:/usr/lib/', 'bigap:/usr/libexec/', 'bigap:/usr/local/'], status)
    status = runTest(hostField.name, [["biga"], 0], ['bigap'], status)
    status = runTest(hostField.name, [["/usr/l"], 0], ['/usr/lib', '/usr/lib64', '/usr/local'], status)
    status = runTest(hostField.name, [["/tmp/2.ovpn"], 0], ['/tmp/2.ovpn'], status)
    status = runTest(hostField.name, [["bigap:/tmp/"], 0], ['bigap:/tmp/sudoers', 'bigap:/tmp/'], status)
    #status = runTest(hostField.name, [["/tmp/"], 0], ['/tmp/'], status)
    endTest(status)

