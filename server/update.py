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

def prepareBombardierClient(svnUser):
    cwd = os.getcwd()
    os.chdir("../client")
    print "==> updating client"
    userStr = ''
    if svnUser:
        userStr = " --no-auth-cache --username %s"%svnUser
    status, output = commands.getstatusoutput("svn %s update"%userStr)
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
    cmd = 'tar -czvf /tmp/%s --exclude "*.pyc" .svn *' % filename
    print "==> creating tarball..."
    status, output = commands.getstatusoutput(cmd)
    if status != OK:
        print "==> failed to create tarball"
        sys.exit(1)
    os.chdir(cwd)
    return filename

if __name__ == "__main__":
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

    filename = prepareBombardierClient(svnUser)
    serverNames = [s for s in servers.split(' ') if len(s) ]
    for serverName in serverNames:
        r = UpdateRemoteClient(serverName)
        r.scp("/tmp/%s" % filename, filename)
        os.unlink("/tmp/%s" % filename)

        r.connect()
        r.unpackClient(filename)
        r.srcUpdate()
        r.disconnect()
