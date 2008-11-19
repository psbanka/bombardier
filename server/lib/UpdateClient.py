#!/usr/bin/python

import sys, re, os, commands, glob
import PinshCmd, BomHostField, FileNameField
from commonUtil import *
from bombardier.staticData import OK, FAIL, REBOOT, PREBOOT
from BombardierRemoteClient import BombardierRemoteClient
from RemoteClient import EnableRequiredException

class ClientUpdateException(Exception):
    def __init__(self, reason):
        Exception.__init__(self)
        self.reason = reason
    def __repr__(self):
        return "Client failed to update (%s)" % self.reason
    def __str__(self):
        return self.__repr__()

class UpdateRemoteClient(BombardierRemoteClient):

    def __init__(self, hostName, fileName, mode, outputHandle):
        BombardierRemoteClient.__init__(self, hostName, mode.password, mode.serverHome,
                                        mode.termwidth, mode.termcolor, mode.defaultGroup, outputHandle)
        self.base_file_name = fileName
        self.tarFile  = os.path.join(mode.serverHome, "bombardier-client", fileName)
        self.cmdDebug = False
        self.release  = self.base_file_name.split('.tar.gz')[0]

    def setupSpkgDir(self):
        self.debugOutput("Setting system configuration...")
        if self.platform == "win32":
            spkgDosDir = self.gso("cygpath -w %s" % self.spkgDir)
            cmd = 'regtool add -v "/HKEY_LOCAL_MACHINE/SOFTWARE/Bombardier"'
            self.gso(cmd)
            cmd = 'regtool set -v "/HKEY_LOCAL_MACHINE/SOFTWARE/Bombardier/InstallPath" "%s"' % spkgDosDir
            self.gso(cmd)
        else:
            cmd = 'echo spkgPath: %s > /etc/bombardier.yml' % self.spkgDir
            self.gso(cmd)
        self.debugOutput("Testing client...")
        status, info = self.process(INIT, '', '', True)
        if status != OK:
            raise ClientUpdateException("Client failed to run (%s)" % info)

    def unpackAndSetupClient(self):
        self.debugOutput("Unpacking %s on the client" % self.base_file_name)
        self.gso("pushd ~")
        cmd = "tar -xzf %s" % self.base_file_name
        self.gso(cmd)
        cmd = "cd %s" % self.release
        self.gso(cmd)
        self.debugOutput("Installing client libraries...")
        self.gso("%s setup.py install" % self.python)

    def update(self):
        try:
            self.connect()
            self.scp(self.tarFile, self.base_file_name)
            self.unpackAndSetupClient()
            self.setupSpkgDir()
        except ClientUpdateException, e:
            return FAIL, ["ERROR INSTALLING: %s" % e.reason]
        return OK,["Client has been upgraded to %s" % self.release]

class UpdateClient(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "update")
        self.helpText = "update\tupdate the bombardier software on a remote system"
        self.bomHostField = BomHostField.BomHostField()
        self.children = [self.bomHostField]
        clientDeploymentDirectory = os.path.join(mode.serverHome, "bombardier-client")
        self.fileNameField = FileNameField.FileNameField(clientDeploymentDirectory)
        self.bomHostField.children = [self.fileNameField]
        self.level = 0
        self.cmdOwner = 1
        self.logCommand = True

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        if noFlag:
            return FAIL, []
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        hostName = tokens[1]
        fileName = tokens[2]
        try:
            r = UpdateRemoteClient(hostName, fileName, mode, slash.fpOut)
        except EnableRequiredException:
            return FAIL, ["Must be in enable mode to connect to this server"]
        return r.update()


if __name__ == "__main__":
    from commonUtil import *
    import optparse
    NO_USER=0
    USER=1
    svnUser = None

    parser = optparse.OptionParser("usage: %prog server-name [server]")
    parser.add_option("-u", "--svnUser", dest="svnUser",
                      action="store_const", const=USER,
                      help="Don't decrypt and send any sensitive data")
    (options, args) = parser.parse_args()

    if len(args) == 0:
        print "==> Need to provide a system name"
        parser.print_help()
        sys.exit(1)
    servers = args[0]
    if options.svnUser == USER:
        if len(args) < 2:
            print "==> Need to provide a user name with the -u option."
            parser.print_help()
            sys.exit(1)
        svnUser = args[0]
        servers = args[1]

    serverNames = [s for s in servers.split(' ') if len(s) ]
    for serverName in serverNames:
        r = UpdateRemoteClient(serverName, fileName, mode, sys.stdout)
        r.update()

