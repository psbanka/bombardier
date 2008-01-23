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
        self.localFilename = ''
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
        if self.debug:
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
        if self.debug:
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

    def runCmd(self, commandString):
        if self.freshen() != OK:
            print "==> UNABLE TO CONNECT TO %s. No actions are available." % self.hostName
            return FAIL, ''
        returnCode = OK
        self.s.sendline ('cd %s' %self.spkgDir)
        self.s.prompt()
        self.s.sendline(commandString)
        self.s.prompt()
        output = self.s.before
        self.s.setecho(False)
        self.s.sendline("echo $?")
        self.s.prompt()
        try:
            returnCode = int(str(self.s.before.split()[0].strip()))
            return returnCode, output
        except Exception, e:
            print e
            print "==> invalid returncode: ('%s')" % self.s.before
            return FAIL, output

    def dumpTrace(self):
        traceback = []
        foundIndex = 1
        while foundIndex == 1:
            traceback.append(self.s.match.groups()[0])
            foundIndex = self.s.expect([self.s.PROMPT, self.traceMatcher, self.logMatcher], timeout=600)
        tString = '\n'.join(traceback)

        data = re.compile("NoOptionError\: No option \'(\w+)\' in section\: \'(\w+)\'").findall(tString)
        if data:
            print "==> ERROR IN CONFIGURATION"
            print "==> Need option '%s' in section '%s'." % (data[0], data[1])
        data = re.compile("NoSectionError\: No section\: \'(\w+)\'").findall(tString)
        if data:
            print "==> ERROR IN CONFIGURATION"
            print "==> Need section '%s'." % (data[0])
        else:
            for line in traceback:
                print "==> CLIENT TRACEBACK: ", line

    def process(self, action, packageNames, scriptName, debug):
        msg = []
        if action == EXECUTE:
            self.clearScriptOutput(scriptName)
        if self.freshen() != OK:
            msg.append("==> UNABLE TO CONNECT TO %s. No actions are available." % self.hostName)
            return FAIL, msg
        returnCode = OK
        try:
            self.s.sendline ('cd %s' %self.spkgDir)
            self.s.prompt()
            packageString = ' '.join(packageNames)
            cmd = '%s bc.py %s %s %s' % (self.python, ACTION_DICT[action], packageString, scriptName)
            self.s.sendline(cmd)
            #print "==> Ran %s on the server" % cmd
            foundIndex = 0
            status = OK
            while True:
                foundIndex = self.s.expect([self.s.PROMPT, self.traceMatcher, self.logMatcher], timeout=600)
                if foundIndex == 1:
                    self.dumpTrace()
                    self.s.prompt()
                    returnCode = FAIL
                    break
                if foundIndex == 0:
                    if debug:
                        print "==> BOMBARDIER HAS EXITED"
                        if self.s.before.strip():
                            print "==> Remaining output: %s" % self.s.before.strip()
                    self.s.setecho(False)
                    self.s.sendline("echo $?")
                    self.s.prompt()
                    try:
                        returnCode = int(str(self.s.before.split()[0].strip()))
                        if debug:
                            print "\n\n==> RETURN CODE: %s\n" % RETURN_DICT[returnCode]
                    except Exception, e:
                        msg.append( str(e) )
                        msg.append( "invalid returncode: ('%s')" % self.s.before)
                        returnCode = FAIL
                    break
                messageType, message = self.s.match.groups()
                if messageType == "DEBUG":
                    if DEBUG and debug:
                        print "[FROM %s]: %s" % (self.hostName, message)
                    continue
                message=message.strip()
                if not self.processMessage(message):
                    if debug:
                        print "[FROM %s]: %s" % (self.hostName, message)
                    else:
                        sys.stdout.write('.')
        except Exception, e:
            e = StringIO.StringIO()
            traceback.print_exc(file=e)
            e.seek(0)
            data = e.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            msg.append( ermsg )
            print
            return FAIL, msg
        if returnCode == OK:
            self.getStatusYml()
            if action == EXECUTE:
                self.getScriptOutput(scriptName)
        else:
            if debug:
                print "Skipping status file collection due to error."
        print
        return returnCode, msg

    def clearScriptOutput(self, scriptName):
        localFilename  = "%s-%s.yml" % (self.hostName, scriptName)
        if os.path.isfile(localFilename):
            os.unlink(localFilename)
        return 

    def getScriptOutput(self, scriptName):
        remoteFilename = "%s-output.yml" % (scriptName)
        self.localFilename  = "%s-%s.yml" % (self.hostName, scriptName)
        self.get("%s/output/%s" % (self.spkgDir, remoteFilename))
        if os.path.isfile(remoteFilename):
            os.system("mv -f %s output/%s" % (remoteFilename, self.localFilename) )
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

