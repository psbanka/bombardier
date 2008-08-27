#!/usr/bin/env python

import sys, re, optparse, os, commands, glob
from bombardier.staticData import OK, FAIL, REBOOT, PREBOOT
from BombardierRemoteClient import BombardierRemoteClient

class UpdateRemoteClient(BombardierRemoteClient):

    def __init__(self, hostName, password, dataPath, outputHandle, svnPath = '', svnUser=''):
        BombardierRemoteClient.__init__(self, hostName, password, dataPath, outputHandle)
        self.svnUser = svnUser
        if svnPath:
            self.svnPath = svnPath
        else:
            self.svnPath = 'svn'

    def srcUpdate(self):
        self.debugOutput("running srcUpdate")
        self.s.sendline("%s srcUpdate.py %s" % (self.python, self.hostName))
        self.s.prompt()
        self.debugOutput(self.s.before)

    def updateSource(self):
        self.debugOutput("updating client")
        userStr = ''
        if self.svnUser:
            userStr = " --no-auth-cache --username %s"%self.svnUser
        status, output = commands.getstatusoutput("%s %s update"% (self.svnPath, userStr))
        if status != OK:
            self.debugOutput("failed to update bombardier client")
            raise Exception

    def prepareBombardierClient(self):
        cwd = os.getcwd()
        os.chdir(self.dataPath+"/../client")
        #self.updateSource()
        status, output = commands.getstatusoutput("%s info" % self.svnPath)
        if status != OK:
            self.debugOutput("failed to determine version of bombardier client (%s)" % output)
            raise Exception
        version = int(re.compile("Last Changed Rev\: (\d+)").findall(output)[0])
        self.fileName = "bomClient-%d.tar.gz" % version
        cmd = 'tar -czvf /tmp/%s --exclude "*.pyc" .svn *' % self.fileName
        self.debugOutput("creating tarball...")
        status, output = commands.getstatusoutput(cmd)
        if status != OK:
            self.debugOutput("failed to create tarball")
            raise Exception
        os.chdir(cwd)

    def unpackClient(self):
        self.debugOutput("cleaning out old files")
        self.s.sendline("rm -rf client")
        self.s.prompt()
        self.s.sendline("mkdir client")
        self.s.prompt()
        self.s.sendline("cd client")
        self.s.prompt()
        self.debugOutput("copying in new files")
        self.s.sendline("tar -xzf ../%s" % self.fileName)
        self.s.prompt()

    def sendFile(self, fileNamePattern):
        startDir = os.getcwd()
        os.chdir("%s/deploy/packages" % self.dataPath)
        fileNames = glob.glob(fileNamePattern)
        if len(fileNames) > 1:
            message = "Unknown file to deploy"
            self.debugOutput(message, message)
            raise Exception
        self.scp(fileNames[0], "/tmp/%s" % fileNames[0])
        os.chdir(startDir)

    def update(self):
        self.prepareBombardierClient()
        self.connect()
        self.scp("/tmp/%s" % self.fileName, self.fileName)
        self.unpackClient()
        self.srcUpdate()
        self.disconnect()
        os.unlink("/tmp/%s" % self.fileName)

if __name__ == "__main__":
    from commonUtil import *
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
        r = UpdateRemoteClient(serverName, '', mode.dataPath, sys.stdout, mode.packageData, svnUser)
        r.update()
