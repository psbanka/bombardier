import sys, time, random
from BombardierRemoteClient import *
import Client
import PinshCmd
import BomHostField, PackageField, ScriptField
from commonUtil import *
from commands import getstatusoutput

class PushConfig(PinshCmd.PinshCmd):
    def __init__(self, name = "pushConfig"):
        PinshCmd.PinshCmd.__init__(self, name)
        self.bomHostField = BomHostField.BomHostField()
        self.children = [self.bomHostField]
        self.logCommand = True
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        start = time.time()
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        hostNames = self.bomHostField.name(tokens, 1)
        if len(hostNames) == 0:
            return FAIL, ["Unknown host %s" % tokens[1]]
        if len(hostNames) > 1:
            return FAIL, ["Ambiguous host %s" % tokens[1]]
        hostName = hostNames[0]
        r = mode.getBomConnection(hostName)

        if noFlag:
            status, output = r.runCmd("shred -uf config.yml")
            if status == FAIL:
                return FAIL, ["Unable to delete config.yml from %s. (%s)" % (hostName, output.strip().replace('\n', ' '))]
            return OK, ["Remote config file removed."]
        client = Client.Client(hostName, mode.password)
        status = client.get()
        if status == FAIL:
            return FAIL ["Could not find valid configuration data for this host."]
        if mode.password:
            client.decryptConfig()
        tmpFile = "%s.yml" % (''.join(random.sample("abcdefghijklmnopqrstuvwxyz", 10)))
        open(tmpFile, 'w').write(yaml.dump( client.data ))
        r.scp(tmpFile, r.spkgDir+"/config.yml")
        status, output = getstatusoutput("shred -uf %s" % tmpFile)
        if status == FAIL:
            return FAIL, ["Unable to remove file %s. May have sensitive data left on the operating system."]
        else:
            return OK, ['Command took %5.2f seconds' % (time.time() - start)]


