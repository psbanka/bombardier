#!/usr/bin/env python

import pxssh, pexpect
import sys, re, os, getpass, base64, time
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

    def __init__(self, hostName, configPasswd):
        RemoteClient.__init__(self, hostName)
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
        print " %s INSTALLING %s" %(self.hostName, packageName)
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
        import time
        start = time.time()
        import zlib
        plain = open(filename, 'rb').read()
        compressed = zlib.compress(plain)
        open(filename+".z", 'wb').write(compressed)
        base64.encode(open(filename+".z", 'rb'), open(filename+".b64", 'w'))
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
        print "==> Configuration sent in : %3.2f seconds" %(time.time() - start)

    def sendClient(self, data):
        if data:
            pass
        client = Client.Client(self.hostName, self.configPasswd)
        status = client.get()
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

    def process(self, action, packageNames, scriptName):
        if self.freshen() != OK:
            print "==> UNABLE TO CONNECT TO %s. No actions are available." % self.hostName
            return FAIL
        returnCode = OK
        try:
            self.s.sendline ('cd %s' %self.spkgDir)
            self.s.prompt()
            packageString = ' '.join(packageNames)
            cmd = '%s bc.py %s %s %s' % (self.python, ACTION_DICT[action], packageString, scriptName)
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
                        returnCode = int(str(self.s.before.split()[0].strip()))
                        print "\n\n==> RETURN CODE: %s\n" % RETURN_DICT[returnCode]
                    except Exception, e:
                        print e
                        print "==> invalid returncode: ('%s')" % self.s.before
                    break
                messageType, message = self.s.match.groups()
                if messageType == "DEBUG":
                    if DEBUG:
                        print "[FROM %s]: %s" % (self.hostName, message)
                    continue
                message=message.strip()
                if not self.processMessage(message):
                    print "[FROM %s]: %s" % (self.hostName, message)
        except Exception, e:
            e = StringIO.StringIO()
            traceback.print_exc(file=e)
            e.seek(0)
            data = e.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            print ermsg
            return FAIL
        self.getStatusYml()
        if action == EXECUTE:
            self.getScriptOutput(scriptName)
        return returnCode

    def getScriptOutput(self, scriptName):
        remoteFilename = "%s-output.yml" % (scriptName)
        localFilename  = "%s-%s.yml" % (self.hostName, scriptName)
        self.get("%s/output/%s" % (self.spkgDir, remoteFilename))
        if os.path.isfile(remoteFilename):
            os.system("mv %s output/%s" % (remoteFilename, localFilename) )
        return 

    def getStatusYml(self):
        statusDir = 'status'
        if not os.path.isdir( statusDir ):
            os.makedirs( statusDir )

        self.s.sendline ('cd %s && cat status.yml' %self.spkgDir)
        self.s.prompt()
        statusYml = str(self.s.before).split("status:")[0]
        try:
            yaml.load(statusYml)
        except:
            print "==> status.yml could not be parsed (writing to error.yml)"
            open( "%s/error.yml" %(statusDir), 'w' ).write(statusYml)
            return
        open( "%s/%s.yml" %(statusDir, self.hostName), 'w' ).write(statusYml)

