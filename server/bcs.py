#!/usr/bin/env python

import pxssh, pexpect
import sys, re, glob, sys, optparse, os, getpass, base64
import yaml
import Client
from bombardier.staticData import OK, FAIL
import StringIO
import traceback

DEFAULT_PASSWORD = "defaultPassword.b64"
TMP_FILE = "tmp.yml"
DOT_LENGTH = 20

UNINSTALL = 0
CONFIGURE = 1
INSTALL   = 2
VERIFY    = 3
RECONCILE = 4
STATUS    = 5

ACTION_DICT = {UNINSTALL: '-u', CONFIGURE:'-c', INSTALL:'-i', 
               VERIFY: '-v', RECONCILE: '-r', STATUS: '-s'}

def getClient(serverName):
    clientFile = "deploy/client/%s.yml" % serverName
    if not os.path.isfile(clientFile):
        print "==> Client config file doesn't exist."
    yamlString = yaml.load(open(clientFile, 'r').read())
    return yamlString

class ClientUnavailableException(Exception):
    def __init__(self, server, errmsg):
        e = Exception()
        Exception.__init__(e)
        self.server = server
        self.errmsg = errmsg
    def __repr__(self):
        return "Unable to connect to %s (%s)" % (self.server, self.errmsg)

def scp(source, dest, hostname, username, password):
    sshNewkey = 'Are you sure you want to continue connecting'
    s = pexpect.spawn('scp %s %s@%s:%s' % (source, username, hostname, dest))
    i = s.expect([pexpect.TIMEOUT, sshNewkey, '[pP]assword: '])
    if i == 0:
        raise ClientUnavailableException(dest, s.before+'|'+s.after)
    if i == 1:
        s.sendline('yes')
        s.expect('[pP]assword: ')
        i = s.expect([pexpect.TIMEOUT, '[pP]assword: '])
        if i == 0:
            raise ClientUnavailableException(dest, s.before+'|'+s.after)
        s.sendline(password)
    if i == 2:
        s.sendline(password)
    s.expect(pexpect.EOF)
    s.close()
    #print "Sent %s to %s:%s" % (source, hostname, dest)
    return OK

class remoteClient:

    def __init__(self, hostname, action, packageNames):
        self.hostname     = hostname
        info = getClient(self.hostname)
        self.username     = info["defaultUser"]
        self.action       = action
        self.ipAddress    = info["ipAddress"]
        self.packageNames = packageNames
        if os.path.isfile("defaultPassword.b64"):
            print "==> Using default password"
            self.password = base64.decodestring(open(DEFAULT_PASSWORD).read())
        else:
            self.password  = getpass.getpass( "Enter password for %s: "% self.username )
        self.stateMachine = []
        self.stateMachine.append([re.compile("\=\=REQUEST-CONFIG\=\="), self.sendConfig])
        self.stateMachine.append([re.compile("\=\=REQUEST-BOM\=\=:(.+)\|"), self.sendBom])
        self.stateMachine.append([re.compile("\=\=REQUEST-PACKAGE\=\=:(.+)\|(.+)\|"), self.sendPackage])
        self.logMatcher = re.compile( "\d+\-\d+\-\d+\s\d+\:\d+\:\d+\,\d+\|([A-Z]+)\|(.+)" )
        self.traceMatcher = re.compile( "\|\|\>\>\>(.+)" )
        self.s = pxssh.pxssh()
        self.s.timeout = 6000

    def sendPackage(self, data):
        package, path = data
        filename = "deploy/packages/"+package
        if not os.path.isfile(filename):
            print "Client requested a file that is not on this server: %s" % filename
            self.s.send(`FAIL`)
        scp(filename, path, self.hostname, self.username, self.password)
        self.s.send("OK\n")

    def streamData(self, filename):
        base64.encode(open(filename, 'rb'), open(filename+".b64", 'w'))
        self.s.setecho(False)
        handle = open(filename+'.b64', 'r')
        BLOCK_SIZE = 77
        lines = 0
        totalLines = os.stat(filename+".b64")[6] / BLOCK_SIZE
        printFrequency = totalLines / DOT_LENGTH
        if filename == "tmp.yml":
            print "==> Sending configuration:"
        else:
            print "==> Sending "+filename+": ",
        if printFrequency < 1:
            printFrequency = 1
        while True:
            chunk = handle.read(BLOCK_SIZE)
            lines += 1
            if chunk == '':
                chunk = ' '*(BLOCK_SIZE-1)+'\n'
                self.s.send(chunk)
                break
            if len(chunk) < BLOCK_SIZE:
                pad = ' '*(BLOCK_SIZE-len(chunk))
                chunk = chunk[:-1] + pad + '\n'
            if lines % printFrequency == 0:
                sys.stdout.write('.')
                sys.stdout.flush()
            self.s.send(chunk)
        print "\n"

    def sendConfig(self, data):
        client = Client.Client(self.hostname)
        status = config.downloadConfig()
        if status == FAIL:
            print "==> Could not find valid configuration data for this host. Exiting."
            sys.exit()
        open(TMP_FILE, 'w').write(yaml.dump( config.data ))
        self.streamData(TMP_FILE)
        os.unlink(TMP_FILE)

    def sendBom(self, data):
        filename = "deploy/bom/%s.yml" % data
        if not os.path.isfile(filename):
            print "==> Could not find valid bom data for this %s. Exiting." % filename
            sys.exit()
        self.streamData(filename)

    def processMessage(self, message):
        for state in self.stateMachine:
            match, function = state
            grepInfo = match.findall(message)
            if grepInfo:
                if function:
                    function(grepInfo[0])

    def reconcile(self):
        print "==> Connecting..."
        try:
            if not self.s.login (self.ipAddress, self.username, self.password):
                print "==> SSH session failed on login."
                print str(self.s)
                sys.exit(1)
            self.s.sendline ('cd /cygdrive/c/spkg/')
            self.s.prompt()
            print "==> Reconciling system..."
            self.s.sendline('stty -echo')
            self.s.prompt()
            packageString = ' '.join(self.packageNames)
            cmd = '/cygdrive/c/Python25/python.exe bc.py %s %s' % (ACTION_DICT[self.action], packageString)
            self.s.sendline(cmd)
            print "==> Ran %s on the server" % cmd
            foundIndex = 0
            returnCode = 0
            while True:
                foundIndex = self.s.expect([self.s.PROMPT, self.traceMatcher, self.logMatcher], timeout=600)
                if foundIndex == 1:
                    print "==> CLIENT TRACEBACK: ", self.s.match.groups()[0]
                    #print self.s.read_nonblocking(100,1)
                    continue
                if foundIndex == 0:
                    print "==> BOMBARDIER HAS EXITED"
                    self.s.setecho(False)
                    self.s.sendline("echo $?")
                    self.s.prompt()
                    try:
                        returnCode = int(self.s.before.strip())
                        print "==> RETURN CODE: %s" % returnCode
                    except:
                        print "==> invalid returncode: %s" % self.s.before
                    break
                type, message = self.s.match.groups()
                print "[FROM %s]: %s" % (self.hostname, message)
                if type == "DEBUG":
                    continue
                message=message.strip()
                if type == "INFO":
                    self.processMessage(message)
                else:
                    print message
            if returnCode == 2:
                print "==========REBOOT"
        except Exception, e:
            e = StringIO.StringIO()
            traceback.print_exc(file=e)
            e.seek(0)
            data = e.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            print ermsg
        finally:
            self.s.logout()


if __name__ == "__main__":
    parser = optparse.OptionParser("usage: %prog server-name [options] [package-names]")
    parser.add_option("-s", "--status", dest="action",
                      action="store_const", const=STATUS,
                      help="display the status of the system")
    parser.add_option("-c", "--configure", dest="action",
                      action="store_const", const=CONFIGURE,
                      help="configure a package")
    parser.add_option("-v", "--verify", dest="action",
                      action="store_const", const=VERIFY,
                      help="verify a package")
    parser.add_option("-i", "--install", dest="action",
                      action="store_const", const=INSTALL,
                      help="install a package")
    parser.add_option("-r", "--reconcile", dest="action",
                      action="store_const", const=RECONCILE,
                      help="reconcile the system")
    parser.add_option("-u", "--uninstall", dest="action",
                      action="store_const", const=UNINSTALL,
                      help="uninstall a package")

    (options, args) = parser.parse_args()
    if len(args) == 0:
        print "==> Need to provide a system name"
        parser.print_help()
        sys.exit(1)

    if not (options.action == STATUS or options.action == RECONCILE):
        if len(args) < 2:
            print "==> Need to provide one or more package names with this option."
            parser.print_help()
            sys.exit(1)
        packageNames = args[1:]
    else:
        packageNames = []

    serverName = args[0]
    r = remoteClient(serverName, options.action, packageNames)
    r.reconcile()
