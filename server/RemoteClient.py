#!/usr/bin/env python

import pxssh, pexpect
import sys, re, glob, sys, optparse, os, getpass, base64
import yaml
import Client
from bombardier.staticData import OK, FAIL, REBOOT, PREBOOT
import StringIO
import traceback


DEFAULT_PASSWORD = "defaultPassword.b64"
DOT_LENGTH = 20

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

class RemoteClient:

    def __init__(self, hostname):
        self.hostname     = hostname
        info = getClient(self.hostname)
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

    def connect(self):
        print "==> Connecting..."
        returnCode = OK
        try:
            if not self.s.login (self.ipAddress, self.username, self.password, login_timeout=30):
                print "==> SSH session failed on login."
                print str(self.s)
                sys.exit(1)
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

    def scp(self, source, dest):
        print "==> sending %s" % source
        sshNewkey = 'Are you sure you want to continue connecting'
        s = pexpect.spawn('scp -v %s %s@%s:%s' % (source, self.username, self.ipAddress, dest), timeout=30)
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
        #print "Sent %s to %s:%s" % (source, self.hostname, dest)
        return OK

    def disconnect(self):
        self.s.logout()
