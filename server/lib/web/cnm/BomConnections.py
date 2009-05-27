#!/usr/bin/env python

import sys, os
import stat
import yaml, syck
from BombardierRemoteClient import BombardierRemoteClient

class HostNotEnabledException(Exception):
    def __init__(self, server):
        e = Exception()
        Exception.__init__(e)
        self.server = server
    def __repr__(self):
        return "Host %s is not enabled" % self.server

class BomConnections:
    def __init__(self):
        self.password = ''
        self.bomConnections = {}
        self.defaultGroup = "root"
        self.debug = True
        self.serverHome = None

    def getBomConnection(self, hostName, outputHandle=sys.stdout, ignoreConfig=False, enabled=True):
        if not hostName in self.bomConnections:
            brc = BombardierRemoteClient(hostName, self.password, self.serverHome,
                                         self.termwidth, self.termcolor, self.defaultGroup,
                                         outputHandle, enabled=enabled)
            self.bomConnections[hostName] = brc
        else:
            if self.password:
                self.bomConnections[hostName].setConfigPass(self.password)
            self.bomConnections[hostName].debug = self.debug
            self.bomConnections[hostName].termwidth = self.termwidth
            self.bomConnections[hostName].termcolor = self.termcolor
            self.bomConnections[hostName].refreshConfig()
        return self.bomConnections[hostName]

    def clearBomConnections(self):
        for hostName in self.bomConnections:
            try:
                self.bomConnections[hostName].disconnect()
                self.bomConnections[hostName] = None
            except:
                print "%% Could not gracefully disconnect from %s." % hostName

if __name__ == "__main__":
    from libtest import starttest, runtest, endtest
    starttest()

    #status = runtest(mode.reprompt, [], none, status)

    endTest(status)
