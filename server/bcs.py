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
EXECUTE   = 6

ACTION_DICT = {UNINSTALL: '-u', CONFIGURE:'-c', INSTALL:'-i', 
               VERIFY: '-v', RECONCILE: '-r', STATUS: '-s', 
               EXECUTE: '-x'}

def getClient(serverName):
    client = Client.Client(serverName)
    status = client.downloadClient()
    if status == FAIL:
        print "==> Could not find valid configuration data for this host. Exiting."
        sys.exit()
    return client.data

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
    s = pexpect.spawn('scp %s %s@%s:%s' % (source, username, hostname, dest), timeout=30)
    i = s.expect([pexpect.TIMEOUT, sshNewkey, '[pP]assword: '], timeout=30)
    if i == 0:
        raise ClientUnavailableException(dest, s.before+'|'+s.after)
    if i == 1:
        s.sendline('yes')
        s.expect('[pP]assword: ', timeout=30)
        i = s.expect([pexpect.TIMEOUT, '[pP]assword: '], timeout=30)
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

    def __init__(self, hostname, action, packageNames, scriptName):
        self.hostname     = hostname
        info = getClient(self.hostname)
        self.username     = info["defaultUser"]
        self.action       = action
        self.scriptName   = scriptName
        self.ipAddress    = info["ipAddress"]
        self.platform     = info["platform"]
        if self.platform == 'win32':
            self.python  = '/cygdrive/c/Python25/python.exe'
            self.spkgDir = '/cygdrive/c/spkg'
        else:
            self.python  = '/usr/local/bin/python2.4'
            self.spkgDir = '/opt/spkg'
        self.packageNames = packageNames
        if os.path.isfile("defaultPassword.b64"):
            print "==> Using default password"
            self.password = base64.decodestring(open(DEFAULT_PASSWORD).read())
        else:
            self.password  = getpass.getpass( "Enter password for %s: "% self.username )
        self.stateMachine = []
        self.stateMachine.append([re.compile("\=\=REQUEST-CONFIG\=\="), self.sendClient])
        self.stateMachine.append([re.compile("\=\=REQUEST-BOM\=\=:(.+)"), self.sendBom])
        self.stateMachine.append([re.compile("\=\=REQUEST-PACKAGE\=\=:(.+):(.+)"), self.sendPackage])
        self.stateMachine.append([re.compile("Beginning installation of \((\S+)\)"), self.install])
        self.stateMachine.append([re.compile("Install result: (\d)"), self.installResult])
        self.stateMachine.append([re.compile("Verify result: (\d)"), self.verifyResult])
        self.logMatcher = re.compile( "\d+\-\d+\-\d+\s\d+\:\d+\:\d+\,\d+\|([A-Z]+)\|(.+)\|" )
        self.traceMatcher = re.compile( "\|\|\>\>\>(.+)" )
        self.s = pxssh.pxssh()
        self.s.timeout = 6000

    def verifyResult(self, result):
        print "============================================="
        if result == '0':
            print "VERIFY SUCCESSFUL"
        print "============================================="

    def installResult(self, result):
        print "============================================="
        if result == '0':
            print "INSTALL SUCCESSFUL"
        print "============================================="

    def install(self, packageName):
        print 
        print "============================================="
        print " CLIENT INSTALLING %s" % packageName
        print "============================================="

    def sendPackage(self, data):
        package, path = data
        filename = "deploy/packages/"+package
        if not os.path.isfile(filename):
            print "Client requested a file that is not on this server: %s" % filename
            self.s.send(`FAIL`)
        scp(filename, path, self.ipAddress, self.username, self.password)
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
            print "==> Sending configuration:",
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

    def sendClient(self, data):
        client = Client.Client(self.hostname)
        status = client.downloadClient()
        if status == FAIL:
            print "==> Could not find valid configuration data for this host. Exiting."
            sys.exit()
        open(TMP_FILE, 'w').write(yaml.dump( client.data ))
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
            if not self.s.login (self.ipAddress, self.username, self.password, login_timeout=30):
                print "==> SSH session failed on login."
                print str(self.s)
                sys.exit(1)
            self.s.sendline ('cd %s' %self.spkgDir)
            self.s.prompt()
            self.s.sendline('stty -echo')
            self.s.prompt()
            packageString = ' '.join(self.packageNames)
            cmd = '%s bc.py %s %s %s' % (self.python, ACTION_DICT[self.action], packageString, self.scriptName)
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
                    print "==> Remaining output: %s" % self.s.before
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
                if type != "DEBUG":
                    self.processMessage(message)
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
    parser.add_option("-x", "--execute", dest="action",
                      action="store_const", const=EXECUTE,
                      help="execute a maintenance script.")

    (options, args) = parser.parse_args()

    scriptName = '' 
    if len(args) == 0:
        print "==> Need to provide a system name"
        parser.print_help()
        sys.exit(1)

    if options.action == EXECUTE:
        if len(args) < 3:
            print "==> Need to provide a package name and a script name with this option."
            parser.print_help()
            sys.exit(1)
        packageNames = [args[1]]
        scriptName   = args[2]
        
    elif options.action not in [STATUS, RECONCILE]:
        if len(args) < 2:
            print "==> Need to provide one or more package names with this option."
            parser.print_help()
            sys.exit(1)
        packageNames = args[1:]
    else:
        packageNames = []

    serverName = args[0]
    r = remoteClient(serverName, options.action, packageNames, scriptName)
    r.reconcile()
