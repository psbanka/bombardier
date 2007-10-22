#!/usr/bin/env python

import pxssh, pexpect
import sys, re, glob, sys, optparse, os, getpass, base64
import yaml
import Client
from bombardier.staticData import OK, FAIL, REBOOT, PREBOOT
import StringIO
import traceback
from RemoteClient import RemoteClient

TMP_FILE = "tmp.yml"
DOT_LENGTH = 20

DEBUG     = False
UNINSTALL = 0
CONFIGURE = 1
INSTALL   = 2
VERIFY    = 3
RECONCILE = 4
STATUS    = 5
EXECUTE   = 6
FIX       = 7
PURGE     = 8

ACTION_DICT = {UNINSTALL: '-u', CONFIGURE:'-c', INSTALL:'-i', 
               VERIFY: '-v', RECONCILE: '-r', STATUS: '-s', 
               EXECUTE: '-x', FIX: '-f', PURGE: '-p'}

RETURN_DICT = {OK: 'OK', FAIL: 'FAIL', REBOOT: 'REBOOT', PREBOOT: 'PREBOOT'}

class BombardierRemoteClient(RemoteClient):

    def __init__(self, hostname, action, packageNames, scriptName, configPasswd):
        RemoteClient.__init__(self, hostname)
        self.action       = action
        self.scriptName   = scriptName
        self.packageNames = packageNames
        self.configPasswd = configPasswd
        self.stateMachine = []
        self.stateMachine.append([re.compile("\=\=REQUEST-CONFIG\=\="), self.sendClient])
        self.stateMachine.append([re.compile("\=\=REQUEST-BOM\=\=:(.+)"), self.sendBom])
        self.stateMachine.append([re.compile("\=\=REQUEST-PACKAGE\=\=:(.+):(.+)"), self.sendPackage])
        self.stateMachine.append([re.compile("Beginning installation of \((\S+)\)"), self.install])
        self.stateMachine.append([re.compile("(\S+) result for (\S+) : (\d)"), self.actionResult])
        self.logMatcher = re.compile( "\d+\-\d+\-\d+\s\d+\:\d+\:\d+\,\d+\|([A-Z]+)\|(.+)\|" )
        self.traceMatcher = re.compile( "\|\|\>\>\>(.+)" )
        if self.platform == 'win32':
            self.python  = '/cygdrive/c/Python25/python.exe'
            self.spkgDir = '/cygdrive/c/spkg'
        else:
            self.python  = '/usr/local/bin/python2.4'
            self.spkgDir = '/opt/spkg'

    def actionResult(self, data):
        action, packageName, result = data
        print "============================================="
        print "%s %s: %s" %(action.upper(), RETURN_DICT[int(result)], packageName)
        print "============================================="

    def install(self, packageName):
        print 
        print "============================================="
        print " %s INSTALLING %s" %(self.hostname, packageName)
        print "============================================="

    def sendPackage(self, data):
        package, path = data
        filename = "deploy/packages/"+package
        if not os.path.isfile(filename):
            print "Client requested a file that is not on this server: %s" % filename
            self.s.send(`FAIL`)
        self.scp(filename, path)
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
        print

    def sendClient(self, data):
        client = Client.Client(self.hostname, self.configPasswd)
        status = client.downloadClient()
        client.decryptConfig()
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
                    return True
        return False

    def reconcile(self):
        self.connect()
        returnCode = OK
        try:
            self.s.sendline ('cd %s' %self.spkgDir)
            self.s.prompt()
            packageString = ' '.join(self.packageNames)
            cmd = '%s bc.py %s %s %s' % (self.python, ACTION_DICT[self.action], packageString, self.scriptName)
            self.s.sendline(cmd)
            #print "==> Ran %s on the server" % cmd
            foundIndex = 0
            while True:
                foundIndex = self.s.expect([self.s.PROMPT, self.traceMatcher, self.logMatcher], timeout=600)
                if foundIndex == 1:
                    print "==> CLIENT TRACEBACK: ", self.s.match.groups()[0]
                    #print self.s.read_nonblocking(100,1)
                    continue
                if foundIndex == 0:
                    if DEBUG:
                        print "==> BOMBARDIER HAS EXITED"
                    if self.s.before.strip():
                        print "==> Remaining output: %s" % self.s.before.strip()
                    self.s.setecho(False)
                    self.s.sendline("echo $?")
                    self.s.prompt()
                    try:
                        returnCode = int(self.s.before.strip())
                        print "\n\n==> RETURN CODE: %s\n" % RETURN_DICT[returnCode]
                    except:
                        print "==> invalid returncode: %s" % self.s.before
                    break
                type, message = self.s.match.groups()
                if type == "DEBUG":
                    if DEBUG:
                        print "[FROM %s]: %s" % (self.hostname, message)
                    continue
                message=message.strip()
                if self.processMessage(message) == False:
                    print "[FROM %s]: %s" % (self.hostname, message)
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
            self.disconnect()
        return returnCode


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
    parser.add_option("-f", "--fix", dest="action",
                      action="store_const", const=FIX,
                      help="set a package status to INSTALLED without doing anything")
    parser.add_option("-p", "--purge", dest="action",
                      action="store_const", const=PURGE,
                      help="Remove a package from the client's status")
    parser.add_option("-k", "--insecure", dest="insecure",
                      action="store_true", default=False,
                      help="Don't decrypt and send any sensitive data")

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
        print "ARGS:", args
        if len(args) < 2:
            print "==> Need to provide one or more package names with this option."
            parser.print_help()
            sys.exit(1)
        packageNames = args[1:]
    else:
        packageNames = []

    if options.insecure:
        configPasswd = ''
    else:
        configPasswd = getpass.getpass("Enter client configuration password: ")

    serverNames = [s for s in args[0].split(' ') if len(s) ]
    for serverName in serverNames:
        r = BombardierRemoteClient(serverName, options.action, packageNames, scriptName, configPasswd)
        r.reconcile()
