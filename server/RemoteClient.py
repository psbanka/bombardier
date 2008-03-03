#!/usr/bin/env python

import pxssh, pexpect
import sys, os, getpass, base64, time
import Client
from bombardier.staticData import OK, FAIL, REBOOT, PREBOOT

#Statuses:
DISCONNECTED = 0
CONNECTED    = 1
BROKEN       = 2

DEFAULT_PASSWORD = "defaultPassword.b64"
DOT_LENGTH = 20

CONNECTION_TIMEOUT = 90 * 3600 #90 min

class ClientConfigurationException(Exception):
    def __init__(self, server):
        e = Exception()
        Exception.__init__(e)
        self.server = server
    def __repr__(self):
        return "Could not find valid configuration data for %s" % self.server

def getClient(serverName, dataPath):
    client = Client.Client(serverName, '', dataPath)
    status = client.get()
    if status == FAIL:
        raise ClientConfigurationException(serverName)
    return client.data

class ClientUnavailableException(Exception):
    def __init__(self, server, errmsg):
        e = Exception()
        Exception.__init__(e)
        self.server = server
        self.errmsg = errmsg
    def __repr__(self):
        return "Unable to connect to %s (%s)" % (self.server, self.errmsg)

class RemoteClient:

    def __init__(self, hostName, dataPath, outputHandle=sys.stdout):
        self.debug        = True
        self.hostName     = hostName
        self.dataPath     = dataPath
        self.outputHandle = outputHandle 
        self.status       = DISCONNECTED
        self.info         = getClient(self.hostName, dataPath)
        self.username     = self.info["defaultUser"]
        self.ipAddress    = self.info["ipAddress"]
        self.platform     = self.info["platform"]
        if 'sharedKeys' not in self.info:
            if os.path.isfile("defaultPassword.b64"):
                self.debugOutput("Using default password")
                self.password = base64.decodestring(open(self.dataPath+"/"+DEFAULT_PASSWORD).read())
            else:
                self.password  = getpass.getpass( "Enter password for %s: "% self.username )
        else:
            self.password = ''
        self.connectTime = 0
        self.cursorPosition = 0
        self.savedDirectory = ''
    
    def templateOutput(self, template, debugText, noDebugText='.'):
        output = ''
        if self.debug:
            if not debugText:
                return
            try:
                output = template % debugText
            except TypeError:
                self.outputHandle.write("ERROR IN DEBUG COMMAND:" )
                self.outputHandle.write("TEMPLATE: " + template )
                self.outputHandle.write("DEBUGTEXT: " + debugText )
                raise Exception
        else:
            if noDebugText != '.':
                self.outputHandle.write( "\n ")
            output = noDebugText
        self.outputHandle.write(output)
        self.outputHandle.flush()

    def debugOutput(self, debugText, noDebugText='.'):
        self.templateOutput("==> %s\n", debugText, noDebugText)

    def fromOutput(self, fromText):
        template = "==> [From %s]: " % self.hostName
        self.templateOutput(template + "%s\n", fromText)

    def connect(self):
        self.s = pxssh.pxssh()
        self.s.timeout = 6000
        #self.debugOutput("Connecting to %s..." %self.hostName)
        try:
            if not self.s.login (self.ipAddress, self.username, self.password, login_timeout=30):
                raise Exception
            self.s.sendline('stty -echo')
            self.s.prompt()
        except:
            message = "SSH session failed on login."
            self.debugOutput(message, message)
            self.status = BROKEN
            return FAIL
        self.status = CONNECTED
        self.connectTime = time.time()
        return OK

    def connectRsync(self, direction, localPath, remotePath, dryRun = True, deleteFlag = False):
        cmd = "bash -c 'rsync --progress -a "
        if dryRun:
            cmd += "--dry-run "
        if deleteFlag:
            cmd += "--delete "
        if direction == "PUSH":
            cmd += "%s %s@%s:%s" % (localPath, self.username, self.ipAddress, remotePath)
        else:
            cmd += "%s@%s:%s %s" % (self.username, self.ipAddress, remotePath, localPath)
        cmd += "'"
        self.debugOutput("EXECUTING: %s" % cmd)
        stdout, stdin = os.popen4(cmd)
        s = pexpect.spawn(cmd, timeout=5000)
        sshNewkey = 'Are you sure you want to continue connecting'
        expectedValues = [pexpect.TIMEOUT, sshNewkey, '[pP]assword: ',
                          '(\d+) files to consider', '(\d+) file to consider']
        while True:
            i = s.expect(expectedValues, timeout=30)
            if i == 0:
                raise ClientUnavailableException(self.hostName, s.before+'|'+s.after)
            if i == 1:
                s.sendline('yes')
                continue
            if i == 2:
                s.sendline(self.password)
                continue
            if i == 3 or i == 4:
                break
        return s

    def getRsyncLine(self, s):
        while True:
            line = s.readline().strip()
            if not line:
                continue
            if " " in line and "kB/s" in line:
                continue
            if line.startswith("created "):
                continue
            if line.startswith("sent "):
                return ''
            return line

    def rsync(self, localPath, remotePath, direction, deleteFlag = False):
        s = self.connectRsync(direction, localPath, remotePath, True)
        files = []
        while True:
            line = self.getRsyncLine(s)
            if not line:
                break
            files.append(line)
        s.expect(pexpect.EOF)
        s.close()
        del s
        numberOfFiles = float(len(files))
        if direction == "PUSH":
            self.debugOutput( "%d files to push..." % numberOfFiles)
        else:
            self.debugOutput( "%d files to pull..." % numberOfFiles)
        if numberOfFiles == 0:
            return OK
        s = self.connectRsync(direction, localPath, remotePath, False, deleteFlag)
        startTime = 0
        while True:
            line = self.getRsyncLine(s)
            if not line:
                break
            if line in files:
                files.remove(line)
                if time.time() - startTime > 500:
                    current = float(len(files))
                    startTime = time.time()
                    value = 100.0 * (numberOfFiles - current) / numberOfFiles
                    self.debugOutput( "%3.1f%% done...(%s)" % (value, line))
        s.expect(pexpect.EOF)
        s.close()
        del s
        return OK

    def freshen(self):
        connectionAge = time.time() - self.connectTime
        if self.status == DISCONNECTED or connectionAge > CONNECTION_TIMEOUT or self.status == BROKEN:
            if self.status == CONNECTED:
                msg = "Assuming our connection to %s is stale after "\
                      "%4.2f minutes. Reconnecting..." % (self.hostName, connectionAge / 60.0)
                self.debugOutput(msg)
                self.disconnect()
            if self.connect() != OK:
                return FAIL
        dead = False
        try:
            self.s.sendline('echo hello')
            if not self.s.prompt(timeout = 5):
                dead = True
        except:
            dead = True

        if dead:
            self.debugOutput("Our connection handle is dead. Reconnecting...")
            try:
                self.disconnect()
            except:
                pass
            if self.connect() != OK:
                return FAIL
        return OK 

    def processScp(self, s):
        sshNewkey = 'Are you sure you want to continue connecting'
        i = s.expect([pexpect.TIMEOUT, sshNewkey, '[pP]assword: ', 'Exit status'], timeout=50)
        if i == 0:
            raise ClientUnavailableException(self.hostName, s.before+'|'+s.after)
        if i == 1:
            s.sendline('yes')
            s.expect('[pP]assword: ', timeout=30)
            i = s.expect([pexpect.TIMEOUT, '[pP]assword: '], timeout=50)
            if i == 0:
                raise ClientUnavailableException(self.hostName, s.before+'|'+s.after)
            s.sendline(self.password)
        if i == 2:
            s.sendline(self.password)
        if i == 3:
            pass
        s.expect(pexpect.EOF)
        s.close()
        return OK

    def get(self, destFile):
        self.debugOutput( "getting %s" % destFile)
        s = pexpect.spawn('scp -v %s@%s:%s .' % (self.username, self.ipAddress, destFile), timeout=30)
        return self.processScp(s)

    def scp(self, source, dest):
        self.debugOutput("sending %s to %s:%s" % (source, self.ipAddress, dest))
        s = pexpect.spawn('scp -v %s %s@%s:%s' % (source, self.username, self.ipAddress, dest), timeout=50)
        sshNewkey = 'Are you sure you want to continue connecting'
        i = s.expect([pexpect.TIMEOUT, sshNewkey, '[pP]assword: ', 'Exit status'], timeout=50)
        if i == 0:
            raise ClientUnavailableException(dest, s.before+'|'+s.after)
        if i == 1:
            s.sendline('yes')
            s.expect('[pP]assword: ', timeout=30)
            i = s.expect([pexpect.TIMEOUT, '[pP]assword: '], timeout=50)
            if i == 0:
                if type(s.before) == type("string") and type(s.after) == type("string"):
                    errMsg = s.before+'|'+s.after
                else:
                    errMsg = "s.before: (%s) s.after: (%s)" % (s.before, s.after)
                raise ClientUnavailableException(dest, errMsg)
            s.sendline(self.password)
        if i == 2:
            self.debugOutput('Using password authentication')
            s.sendline(self.password)
        if i == 3:
            pass
        s.expect(pexpect.EOF)
        s.close()
        return OK
    
    def checkResult(self):
        self.s.setecho(False)
        self.s.sendline("echo $?")
        self.s.prompt()
        returnCode = 0
        try:
            returnCode = int(str(self.s.before.split()[0].strip()))
        except Exception:
            return FAIL
        return returnCode

    def saveCwd(self):
        self.s.setecho(False)
        self.s.sendline("pwd")
        self.s.prompt()
        self.savedDirectory = self.s.before.split()[0]
        
    def returnToStart(self):
        if self.savedDirectory:
            self.s.setecho(False)
            self.s.sendline("cd %s" % self.savedDirectory)
            self.s.prompt()
            self.s.sendline("pwd")
            self.s.prompt()
            cwd = self.s.before.split()[0]
            if cwd != self.savedDirectory:
                sys.exit(1)

    def checkPossiblePaths(self, testPath):
        while len(testPath):
            self.s.sendline("ls --color=never -Fd1 %s*" %testPath)
            self.s.prompt()
            resultList = self.s.before.split()[:-1]
            if self.checkResult() != OK:
                testPath=testPath[:testPath.rfind('/')]
                continue
            output = [ result.replace('*', '') for result in resultList ]
            if testPath.endswith('/'):
                output.append(testPath)
            return output
        return [testPath]

    def disconnect(self):
        self.connectTime = 0
        try:
            self.s.logout()
        finally:
            self.status = DISCONNECTED

if __name__ == "__main__":
    from libTest import startTest, runTest, endTest
    passwd = getpass.getpass("root's password:")
    r = RemoteClient('bigap', '.')
    r.connect()
    status = OK
    status = runTest(r.checkPossiblePaths, ['/usr/l'], ['/usr/lib/', '/usr/libexec/', '/usr/local/'], status)
    status = runTest(r.checkPossiblePaths, ['/usr/l'], ['/usr/lib/', '/usr/libexec/', '/usr/local/'], status)
    status = runTest(r.checkPossiblePaths, ['/usr/x'], ['/usr/'], status)
    status = runTest(r.checkPossiblePaths, ['/tmp/sudo'], ['/tmp/sudoers'], status)
    status = runTest(r.checkPossiblePaths, ['/tmp/'], ['/tmp/'], status)
    endTest(status)
