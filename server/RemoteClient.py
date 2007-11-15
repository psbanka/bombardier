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
        self.s = pxssh.pxssh()
        self.s.timeout = 6000
        self.connectTime = 0

    def connect(self):
        print "==> Connecting to %s..." %self.hostName
        try:
            if not self.s.login (self.ipAddress, self.username, self.password, login_timeout=30):
                print "==> SSH session failed on login."
                print str(self.s)
                self.status = BROKEN
                return FAIL
            self.s.sendline('stty -echo')
            self.s.prompt()
        except Exception, e:
            e = StringIO.StringIO()
            traceback.print_exc(file=e)
            e.seek(0)
            data = e.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            print ermsg
            self.s.logout()
            self.status = BROKEN
            return FAIL
        self.status = CONNECTED
        self.connectTime = time.time()
        return OK

    def freshen(self):
        if self.status == BROKEN:
            return FAIL
        connectionAge = time.time() - self.connectTime
        if self.status == DISCONNECTED or connectionAge > CONNECTION_TIMEOUT:
            if self.status == CONNECTED:
                print "==> Our connection to %s is stale after "\
                      "%4.2f minutes. Reconnecting..." % (self.hostName, connectionAge / 60.0)
                self.disconnect()
            if self.connect() != OK:
                return FAIL
        try:
            self.s.sendline('echo hello')
            self.s.prompt()
        except:
            print "==> Our connection to %s has been cut off after "\
                  "%4.2f minutes." % (self.hostName, connectionAge / 60.0)
            self.disconnect()
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
        print "==> sending %s" % source
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
                raise ClientUnavailableException(dest, s.before+'|'+s.after)
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

    def disconnect(self):
        self.connectTime = 0
        try:
            self.s.logout()
        finally:
            self.status = DISCONNECTED
