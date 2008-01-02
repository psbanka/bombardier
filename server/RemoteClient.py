#!/usr/bin/env python

import pxssh, pexpect
import sys, os, getpass, base64, time
import yaml
import Client
from bombardier.staticData import OK, FAIL, REBOOT, PREBOOT
import StringIO
import traceback

#Statuses:
DISCONNECTED = 0
CONNECTED    = 1
BROKEN       = 2

DEFAULT_PASSWORD = "defaultPassword.b64"
DOT_LENGTH = 20

CONNECTION_TIMEOUT = 90 * 3600 #90 min

def getClient(serverName):
    client = Client.Client(serverName, '')
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

class RemoteClient:

    def __init__(self, hostName):
        self.hostName     = hostName
        self.status       = DISCONNECTED
        info = getClient(self.hostName)
        self.username     = info["defaultUser"]
        self.ipAddress    = info["ipAddress"]
        self.platform     = info["platform"]
        if 'sharedKeys' not in info:
            if os.path.isfile("defaultPassword.b64"):
                print "==> Using default password"
                self.password = base64.decodestring(open(DEFAULT_PASSWORD).read())
            else:
                self.password  = getpass.getpass( "Enter password for %s: "% self.username )
        else:
            self.password = ''
        self.connectTime = 0

    def connect(self):
        self.s = pxssh.pxssh()
        self.s.timeout = 6000
        print "==> Connecting to %s..." %self.hostName
        try:
            if not self.s.login (self.ipAddress, self.username, self.password, login_timeout=30):
                raise Exception
            self.s.sendline('stty -echo')
            self.s.prompt()
        except Exception, e:
            print "==> SSH session failed on login."
            print str(self.s)
            self.status = BROKEN
            return FAIL
        self.status = CONNECTED
        self.connectTime = time.time()
        return OK

    def connectRsync(self, direction, localPath, remotePath, dryRun = True):
        cmd = "bash -c 'rsync --progress -a "
        if dryRun:
            cmd += "--dry-run "
        if direction == "PUSH":
            cmd += "%s/* %s@%s:%s/" % (localPath, self.username, self.ipAddress, remotePath)
        else:
            cmd += "%s@%s:%s/* %s/" % (self.username, self.ipAddress, remotePath, localPath)
        cmd += "'"
        #print "EXECUTING: %s" % cmd
        print "==> Connecting to %s..." % self.hostName
        #output, input = os.popen4(cmd)
        s = pexpect.spawn(cmd, timeout=5000)
        sshNewkey = 'Are you sure you want to continue connecting'
        expectedValues = [pexpect.TIMEOUT, sshNewkey, '[pP]assword: ',
                          '(\d+) files to consider']
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
            if i == 3:
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

    def rsync(self, localPath, remotePath, direction):
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
            print "==> %d files to push..." % numberOfFiles
        else:
            print "==> %d files to pull..." % numberOfFiles
        if numberOfFiles == 0:
            return OK
        s = self.connectRsync(direction, localPath, remotePath, False)
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
                    print "==> %3.1f%% done...(%s)" % (value, line)
        s.expect(pexpect.EOF)
        s.close()
        del s
        return OK

    def freshen(self):
        if self.status == BROKEN:
            return FAIL
        connectionAge = time.time() - self.connectTime
        if self.status == DISCONNECTED or connectionAge > CONNECTION_TIMEOUT:
            if self.status == CONNECTED:
                print "==> Assuming our connection to %s is stale after "\
                      "%4.2f minutes. Reconnecting..." % (self.hostName, connectionAge / 60.0)
                self.disconnect()
            if self.connect() != OK:
                return FAIL
        dead = False
        try:
            self.s.sendline('echo hello')
            if self.s.prompt(timeout = 5) == False:
                dead = True
        except:
            dead = True

        if dead:
            print "==> Our connection handle is dead. Reconnecting..."
            try:
                print self.disconnect.__doc__
                self.disconnect(timeout = 5)
                print "ABC"
            except:
                pass
            if self.connect() != OK:
                return FAIL
        return OK 

    def processScp(self, s):
        sshNewkey = 'Are you sure you want to continue connecting'
        i = s.expect([pexpect.TIMEOUT, sshNewkey, '[pP]assword: ', 'Exit status'], timeout=30)
        if i == 0:
            raise ClientUnavailableException(self.hostName, s.before+'|'+s.after)
        if i == 1:
            s.sendline('yes')
            s.expect('[pP]assword: ', timeout=30)
            i = s.expect([pexpect.TIMEOUT, '[pP]assword: '], timeout=30)
            if i == 0:
                raise ClientUnavailableException(self.hostName, s.before+'|'+s.after)
            s.sendline(self.password)
        if i == 2:
            s.sendline(self.password)
        if i == 3:
            #print '==> Used shared key for authentication.'
            pass
        s.expect(pexpect.EOF)
        s.close()
        #print "Sent %s to %s:%s" % (source, self.hostName, dest)
        return OK

    def get(self, destFile):
        print "==> getting %s" % destFile
        s = pexpect.spawn('scp -v %s@%s:%s .' % (self.username, self.ipAddress, destFile), timeout=30)
        return self.processScp(s)

    def scp(self, source, dest):
        print "==> sending %s to %s:%s" % (source, self.ipAddress, dest)
        s = pexpect.spawn('scp -v %s %s@%s:%s' % (source, self.username, self.ipAddress, dest), timeout=30)
        sshNewkey = 'Are you sure you want to continue connecting'
        i = s.expect([pexpect.TIMEOUT, sshNewkey, '[pP]assword: ', 'Exit status'], timeout=30)
        if i == 0:
            raise ClientUnavailableException(dest, s.before+'|'+s.after)
        if i == 1:
            s.sendline('yes')
            s.expect('[pP]assword: ', timeout=30)
            i = s.expect([pexpect.TIMEOUT, '[pP]assword: '], timeout=30)
            if i == 0:
                if type(s.before) == type("string") and type(s.after) == type("string"):
                    errMsg = s.before+'|'+s.after
                else:
                    errMsg = "s.before: (%s) s.after: (%s)" % (s.before, s.after)
                raise ClientUnavailableException(dest, errMsg)
            s.sendline(self.password)
        if i == 2:
            print '==> Using password authentication'
            s.sendline(self.password)
        if i == 3:
            #print '==> Used shared key for authentication.'
            pass
        s.expect(pexpect.EOF)
        s.close()
        #print "Sent %s to %s:%s" % (source, self.hostName, dest)
        return OK

    def disconnect(self):
        self.connectTime = 0
        try:
            self.s.logout()
        finally:
            self.status = DISCONNECTED
