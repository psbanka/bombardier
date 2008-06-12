import sys, time, random
from BombardierRemoteClient import *
import Client
import PinshCmd
import BomHostField
from commonUtil import *
from commands import getstatusoutput
from Mode import HostNotEnabledException

class Push(PinshCmd.PinshCmd):
    def __init__(self, name = "push"):
        PinshCmd.PinshCmd.__init__(self, name)
        self.helpText = "push\tcopy the client's configuration file to the client (use 'no' to remove)"
        self.bomHostField = BomHostField.BomHostField()
        self.children = [self.bomHostField]
        self.logCommand = True
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        start = time.time()
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]
        hostName = tokens[1]
        try:
            r = mode.getBomConnection(hostName)
        except HostNotEnabledException:
            return FAIL, ["Host not enabled for this user."]
        if noFlag:
            status, output = r.runCmd("shred -uf %s/config.yml" % hostName)
            if status == FAIL:
                return FAIL, ["Unable to delete config.yml from %s. (%s)" % (hostName, output.strip().replace('\n', ' '))]
            return OK, ["Remote config file removed."]
        client = Client.Client(hostName, mode.password, mode.dataPath)
        status = client.get()
        if status == FAIL:
            return FAIL ["Could not find valid configuration data for this host."]
        if not mode.password:
            output = ["Did NOT decrypt configuration before sending"] 
        else:
            output = ["Decrypted configuration before sending."]
        client.decryptConfig()
        tmpFile = mode.dataPath+"/%s.yml" % (''.join(random.sample("abcdefghijklmnopqrstuvwxyz", 10)))
        open(tmpFile, 'w').write(yaml.dump( client.data ))
        r.scp(tmpFile, "%s/%s/config.yml" % (r.spkgDir, hostName))
        status, cmdoutput = getstatusoutput("shred -uf %s" % tmpFile)
        if status == FAIL:
            return FAIL, output + ["Unable to remove file %s. May have sensitive data left on the local operating system."]
        else:
            return OK, output + ['Command took %5.2f seconds' % (time.time() - start)]


