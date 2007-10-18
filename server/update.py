#!/usr/bin/env python

import sys, re, optparse, os, commands
from bombardier.staticData import OK, FAIL, REBOOT, PREBOOT
from RemoteClient import RemoteClient

class UpdateRemoteClient(RemoteClient):

    def __init__(self, hostname):
        RemoteClient.__init__(self, hostname)
        if self.platform == 'win32':
            self.python  = '/cygdrive/c/Python25/python.exe'
        else:
            self.python  = '/usr/local/bin/python2.4'

    def unpackClient(self, filename):
        print "==> cleaning out old files"
        self.s.sendline("rm -rf client")
        self.s.prompt()
        self.s.sendline("mkdir client")
        self.s.prompt()
        self.s.sendline("cd client")
        self.s.prompt()
        print "==> copying in new files"
        self.s.sendline("tar -xzf ../%s" % filename)
        self.s.prompt()

    def srcUpdate(self):
        print "==> running srcUpdate"
        self.s.sendline("%s srcUpdate.py" % self.python)
        self.s.prompt()
        print self.s.before

def prepareBombardierClient():
    cwd = os.getcwd()
    os.chdir("../client")
    print "==> updating client"
    status, output = commands.getstatusoutput("svn update")
    if status != OK:
        print "==> failed to update bombardier client"
        sys.exit(1)
    status, output = commands.getstatusoutput("svn info")
    if status != OK:
        print "==> failed to determine version of bombardier client"
        sys.exit(1)
    version = int(re.compile("Last Changed Rev\: (\d+)").findall(output)[0])
    filename = "bomClient-%d.tar.gz" % version
    #cmd = 'tar -czvf /tmp/%s --exclude "*.pyc" --exclude "*.svn" *' % filename
    cmd = 'tar -czvf /tmp/%s --exclude "*.pyc" *' % filename
    print "==> creating tarball..."
    status, output = commands.getstatusoutput(cmd)
    if status != OK:
        print "==> failed to create tarball"
        sys.exit(1)
    os.chdir(cwd)
    return filename

if __name__ == "__main__":
    parser = optparse.OptionParser("usage: %prog server-name [server]")
    (options, args) = parser.parse_args()

    if len(args) == 0:
        print "==> Need to provide a system name"
        parser.print_help()
        sys.exit(1)

    serverName = args[0]
    filename = prepareBombardierClient()
    r = UpdateRemoteClient(serverName)
    r.scp("/tmp/%s" % filename, filename)
    os.unlink("/tmp/%s" % filename)

    r.connect()
    r.unpackClient(filename)
    r.srcUpdate()
    r.disconnect()
