#!/usr/bin/env python

import readline, socket, sys, os, yaml
from BombardierRemoteClient import BombardierRemoteClient

CONFIG_FILE = "%s/.bomsh_config" % os.environ.get("HOME")

OK = 0
FAIL = 1
UNKNOWN = -1

# Authorization levels
USER = 0
ADMIN = 1

# possible modes
ENABLE = 0
C0 = 1
C1 = 2
C2 = 3
C3 = 4
F0 = 10
F1 = 11
F2 = 12
ALL = 99
FREE_TEXT = 100

REPROMPT = 1

class HostNotEnabledException(Exception):
    def __init__(self, server):
        e = Exception()
        Exception.__init__(e)
        self.server = server
    def __repr__(self):
        return "Host %s is not enabled for this user" % self.server

class Mode:
    def __init__(self, state, prompt):
        self.state = [state]
        self.exitMethods = []
        self.prompt = [prompt]
        self.commandBuffer = {F0:[], F1:[], F2:[]}
        self.variables = {F0:[], F1:[], F2:[]}
        self.globals = {}
        self.newClasses = []
        self.auth = USER
        self.fullPrompt = ""
        self.password = ''
        self.commentCommands = []
        self.bomConnections = {}
        self.getTermInfo()
        self.debug = True
        self.config = {}
        self.username = os.environ["USER"]
        self.autoEnable = False
        if self.username == "root":
            try:
                self.username = raw_input("Logging in as common user account. \nPlease provide your user name: ")
            except:
                sys.exit(0)
        self.setPrompt()

    def addConfigList(self, option, value):
        if self.config.get(option):
            self.config[option].append(value)
        else:
            self.config[option] = [value]
        open(CONFIG_FILE, 'w').write(yaml.dump(self.config))

    def writeConfig(self, option, value):
        self.config[option] = value
        open(CONFIG_FILE, 'w').write(yaml.dump(self.config))

    def loadConfig(self):
        if os.path.isfile(CONFIG_FILE+".yml"):
            print "%% Configuration file should be called %s, not %s.yml. Please rename it." % (CONFIG_FILE, CONFIG_FILE)
        if os.path.isfile(CONFIG_FILE):
            self.config=yaml.load(open(CONFIG_FILE).read())
        debug = self.config.get("debug")
        if type(debug) == type(True):
            self.debug = debug
            #print "  Setting debugging to %s" % self.debug
        if not self.config.has_key("enabledSystems"):
            self.config["enabledSystems"] = []
        self.autoEnable = self.config.get("autoEnable")

    def getTermInfo(self):
        try:
            vars = os.environ["TERMCAP"].split(':')
            co = [ x for x in vars if x.startswith("co") ][0]
            self.termwidth = int(co.split("#")[-1])
            li = [ x for x in vars if x.startswith("li") ][0]
            self.termlen = int(li.split("#")[-1]) - 4
        except:
            self.termlen = 23
            self.termwidth = 80

    def getBomConnection(self, hostName, ignoreConfig=False):
        if not ignoreConfig and not hostName in self.config["enabledSystems"]:
            raise HostNotEnabledException(hostName)
        if not hostName in self.bomConnections:
            brc = BombardierRemoteClient(hostName, self.password)
            self.bomConnections[hostName] = brc
        if self.password:
            self.bomConnections[hostName].configPasswd = self.password
        self.bomConnections[hostName].debug = self.debug
        return self.bomConnections[hostName]

    def clearBomConnections(self):
        for hostName in self.bomConnections:
            try:
                self.bomConnections[hostName].disconnect()
                self.bomConnections[hostName] = None
            except:
                print "%% Could not gracefully disconnect from %s." % hostName
   
    def cleanMode(self, state):
        self.commandBuffer[state] = []
        self.variables[state] = []
        self.exitMethods = self.exitMethods[:-1]

    def setPrompt(self):
        if self.currentState() != FREE_TEXT:
            self.fullPrompt = "%s (%s)%s " %(socket.gethostname(),self.username, self.prompt[-1])
        else:
            self.fullPrompt = ''

    def getPrompt(self):
        commentString = ''
        if self.commentCommands:
            commentString = "(*)"
        return commentString+self.fullPrompt

    def pushPrompt(self, slash, newPrompt, newState, newClasses = 0):
        # pop out of any equal or higher levels
        while self.state[-1]+1 > newState:
            if len(self.state) > 1:
                extraClasses = self.newClasses[-1]
                for i in range(0,extraClasses):
                    slash.children.pop()
            else:
                self.state = [ENABLE]
                break
            self.popPrompt()
        self.state.append(newState)
        self.prompt.append(newPrompt)
        self.newClasses.append(newClasses)
        self.setPrompt()

    def popPrompt(self):
        if len(self.state) > 1:
            self.prompt.pop()
            self.state.pop()
            self.newClasses.pop()
            self.setPrompt()
            return OK
        else:
            return FAIL

    def reprompt(self):
        if REPROMPT: #^ FIXME
            print >>sys.stderr
            print >>sys.stderr,self.getPrompt()+readline.get_line_buffer(),

    def currentState(self):
        return self.state[-1]
    
if __name__ == "__main__":
    from libTest import *
    status = OK
    startTest()
    mode = Mode(1,"#")
    
    status = runTest(getPromptFile, [], "testing", status)
    status = runTest(reprompt, [], None, status)
    status = runTest(mode.setPrompt, [], None, status)
    status = runTest(getPromptFile, [], "gruyere# ", status)
    status = runTest(mode.getPrompt, [], "gruyere# ", status)
    status = runTest(mode.pushPrompt, ["-->", 5], None, status)
    status = runTest(mode.getPrompt, [], "gruyere--> ", status)
    status = runTest(mode.popPrompt, [], OK, status)
    status = runTest(mode.getPrompt, [], "gruyere# ", status)
    status = runTest(mode.currentState, [], 1, status)
    
    endTest(status)
