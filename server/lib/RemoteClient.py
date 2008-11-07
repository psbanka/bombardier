#!/usr/bin/env python

import pxssh, pexpect
import struct, fcntl, termios, signal
import sys, os, getpass, base64, time
import Client
from bombardier.staticData import OK, FAIL, REBOOT, PREBOOT

#Statuses:
DISCONNECTED = 0
CONNECTED    = 1
BROKEN       = 2

DOT_LENGTH = 20

CONNECTION_TIMEOUT = 90 * 3600 #90 min

def getClient(serverName, serverHome, configPass):
    client = Client.Client(serverName, configPass, serverHome)
    status = client.get()
    if status == FAIL:
        raise ClientConfigurationException(serverName)
    if configPass:
        client.decryptConfig()
    return client.data

class EnableRequiredException(Exception):
    def __init__(self):
        e = Exception()
        Exception.__init__(e)
    def __repr__(self):
        return "Must be in enable mode to connect to this server"
    def __str__(self):
        return self.__repr__()

class IncompleteConfigurationException(Exception):
    def __init__(self, server, errmsg):
        e = Exception()
        Exception.__init__(e)
        self.server = server
        self.errmsg = errmsg
    def __repr__(self):
        return "Server configuration for %s is incomplete (%s)" % (self.server, self.errmsg)
    def __str__(self):
        return self.__repr__()

class ClientUnavailableException(Exception):
    def __init__(self, server, errmsg):
        e = Exception()
        Exception.__init__(e)
        self.server = server
        self.errmsg = errmsg
    def __repr__(self):
        return "Unable to connect to %s (%s)" % (self.server, self.errmsg)
    def __str__(self):
        return self.__repr__()

class RemoteClient:

    def __init__(self, hostName, serverHome, termwidth = 80,
                 outputHandle=sys.stdout, configPass='', enabled=True):
        self.debug        = True
        self.hostName     = hostName
        self.serverHome   = serverHome
        self.termwidth    = termwidth
        self.outputHandle = outputHandle
        self.status       = DISCONNECTED
        self.info         = {}
        self.configPass   = configPass
        self.sshPass      = ''
        self.refreshConfig()
        self.username     = self.info.get("defaultUser", None)
        if self.username == None:
            if self.info.get("enc_username"):
                raise EnableRequiredException()
            raise IncompleteConfigurationException(self.hostName, "'defaultUser' is not defined")
        self.ipAddress    = self.info.get("ipAddress", None)
        if self.ipAddress == None:
            if self.info.get("enc_ipAddress"):
                raise EnableRequiredException()
            raise IncompleteConfigurationException(self.hostName, "'ipAddress' is not defined")
        self.platform     = self.info.get("platform", None)
        if self.platform == None:
            if self.info.get("enc_platform"):
                raise EnableRequiredException()
            raise IncompleteConfigurationException(self.hostName, "'platform' is not defined")
        sharedKeys = self.info.get('sharedKeys', True)
        if not sharedKeys or not enabled:
            self.sshPass = getpass.getpass( "Enter password for %s@%s: "% (self.username, self.hostName ) )
        self.connectTime = 0
        self.cursorPosition = 0
        self.savedDirectory = ''

    def refreshConfig(self):
        self.info = getClient(self.hostName, self.serverHome, self.configPass)

    def formatOutput(self, prefix, debugText, noDebugText='.'):
        output = ''
        if self.debug:
            if not debugText:
                return
            try:
                while debugText:
                    grabLength = self.termwidth - len(prefix)
                    if grabLength >= len(debugText):
                        grabLength = len(debugText)
                    elif debugText[grabLength] != ' ':
                        for i in range(grabLength, grabLength-10, -1):
                            if debugText[i] == ' ':
                                grabLength = i
                                break
                    #if '\n' in debugText[:grabLength]:
                        #grabLength = debugText.find(' ')
                    output = prefix + debugText[:grabLength].strip()
                    debugText = debugText[grabLength:]
                    prefix = ' ' * len(prefix)
                    self.outputHandle.write(output+'\n')
                self.outputHandle.flush()
                return
            except TypeError:
                self.outputHandle.write("ERROR IN DEBUG COMMAND:" )
                self.outputHandle.write("DEBUGTEXT: " + debugText )
                raise Exception
        else:
            if noDebugText != '.':
                self.outputHandle.write( "\n ")
            output = noDebugText
        self.outputHandle.write(output)
        self.outputHandle.flush()

    def debugOutput(self, debugText, noDebugText='.'):
        self.formatOutput("==> ", debugText, noDebugText)

    def fromOutput(self, fromText):
        prefix = "==> [From %s]: " % self.hostName
        self.formatOutput(prefix, fromText)

    def sigwinch_passthrough (self, sig, data):
        s = struct.pack("HHHH", 0, 0, 0, 0)
        a = struct.unpack('hhhh', fcntl.ioctl(sys.stdout.fileno(), termios.TIOCGWINSZ , s))
        self.s.setwinsize(a[0],a[1])

    def interact(self, command=''):
        if self.freshen() == FAIL:
            return FAIL
        if not command:
            print " Press ^] to exit your session."
            signal.signal(signal.SIGWINCH, self.sigwinch_passthrough)
            self.s.sendline("stty sane")
            self.s.prompt()
            self.s.sendline('export OLD_PROMPT=$PS1')
            self.s.sendline('export ORIG_PATH=$(pwd)')
            self.s.prompt()
            self.s.sendline('export PS1="\h:\w# "')
            self.s.interact()
            print "\n ...Returning to the bombardier shell."
            try:
                self.s.sendline('export PS1=$OLD_PROMPT')
                self.s.prompt()
                self.s.sendline('cd "$ORIG_PATH"')
                self.s.prompt()
            except pexpect.EOF:
                print " %% Detected terminal disconnect."
                print " %% To preserve your session, Press ^] to exit."
        else:
            self.s.sendline(command)
            self.s.prompt()
            print "Output from command: %s" %command
            output_lines = self.s.before.split('\n')
            if not output_lines[-1].strip():
                output_lines = output_lines[:-1]
            for line in output_lines:
                print "--> %s" %line.strip()
            print
        return OK

    def terminate(self):
        result = self.s.terminate()
        if result:
            self.status = DISCONNECTED
            return OK
        return FAIL

    def connect(self):
        import pxssh, pexpect
        self.s = pxssh.pxssh()
        self.s.timeout = 6000
        msg = "Connecting to %s..." %self.hostName
        self.debugOutput(msg, msg)
        try:
            if not self.s.login(self.ipAddress, self.username, self.sshPass, login_timeout=6000):
                raise ClientUnavailableException
            self.s.sendline('stty -echo')
            self.s.prompt()
        except (ClientUnavailableException, pexpect.TIMEOUT):
            message = "SSH session failed on login."
            self.debugOutput(message, message)
            self.status = BROKEN
            return FAIL
        self.status = CONNECTED
        self.connectTime = time.time()
        return OK

    def connectRsync(self, direction, localPath, remotePath, dryRun = True, deleteFlag = False):
        import pxssh, pexpect
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
        self.debugOutput("EXECUTING: %s" % cmd, cmd) #^^ COMMENT THIS OUT
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
                s.sendline(self.sshPass)
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
        import pxssh, pexpect
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
        import pxssh, pexpect
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
            s.sendline(self.sshPass)
        if i == 2:
            s.sendline(self.sshPass)
        if i == 3:
            pass
        s.expect(pexpect.EOF)
        s.close()
        return OK

    def get(self, destFile):
        import pxssh, pexpect
        self.debugOutput( "Getting %s" % destFile)
        cmd = 'scp -v %s@%s:%s .' % (self.username, self.ipAddress, destFile)
        self.debugOutput("EXECUTING: %s" % cmd, cmd) #^^ COMMENT THIS OUT
        s = pexpect.spawn(cmd, timeout=30)
        return self.processScp(s)

    def scp(self, source, dest):
        import pxssh, pexpect
        self.debugOutput("Sending %s to %s:%s" % (source, self.hostName, dest))
        cmd = 'scp -v %s %s@%s:%s' % (source, self.username, self.ipAddress, dest)
        try:
            s = pexpect.spawn(cmd, timeout=600)
            sshNewkey = 'Are you sure you want to continue connecting'
            i = s.expect([pexpect.TIMEOUT, sshNewkey, '[pP]assword: ', 'Exit status'], timeout=600)
        except pexpect.EOF:
            errMsg = "Connection refused."
            raise ClientUnavailableException(dest, errMsg)
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
            s.sendline(self.sshPass)
        if i == 2:
            self.debugOutput('Using password authentication')
            s.sendline(self.sshPass)
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

    # FIXME: Duplicate code

    def parseSection(self, sectionString, default, optional):
        sections = sectionString.split('.')
        d = self.info
        for section in sections:
            try:
                d = d[section]
            except:
                d = None
                break
        if d == None:
            if not optional:
                raise InvalidConfigData("Option %s not found" % sectionString, None, None)
            d = default
        return d

    def getobj(self, sectionString, default, expType, optional):
        value = self.parseSection(sectionString, default, optional)
        if type(expType) == type("string"):
            if type(value) == type(1234) or type(value) == type(123.32):
                value = str(value)
        if type(value) == type(expType):
            return value
        raise InvalidConfigData(sectionString, type(value), type(expType))

    def listobj(self, sectionString, default=[], optional=True):
        return self.getobj(sectionString, default, [], optional)

    def string(self, sectionString, default='', optional=True):
        return self.getobj(sectionString, default, "string", optional)

    def integer(self, sectionString, default=1, optional=True):
        return self.getobj(sectionString, default, 1, optional)

    def dictionary(self, sectionString, default={}, optional=True):
        return self.getobj(sectionString, default, {}, optional)
if __name__ == "__main__":
    from libTest import startTest, runTest, endTest
    passwd = getpass.getpass("root's password:")
    r = RemoteClient('bigap', '.', 80)
    r.connect()
    status = OK
    status = runTest(r.checkPossiblePaths, ['/usr/l'], ['/usr/lib/', '/usr/libexec/', '/usr/local/'], status)
    status = runTest(r.checkPossiblePaths, ['/usr/l'], ['/usr/lib/', '/usr/libexec/', '/usr/local/'], status)
    status = runTest(r.checkPossiblePaths, ['/usr/x'], ['/usr/'], status)
    status = runTest(r.checkPossiblePaths, ['/tmp/sudo'], ['/tmp/sudoers'], status)
    status = runTest(r.checkPossiblePaths, ['/tmp/'], ['/tmp/'], status)
    endTest(status)
