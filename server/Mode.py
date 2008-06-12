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
        self.batch = False
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
        self.username = os.environ.get("USER")
        self.autoEnable = False
        self.setPrompt()
        self.dataPath = os.getcwd() #! FIXME!!
        self.childProcesses = []

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
        import stat
        st = os.stat(CONFIG_FILE)
        mode = st[stat.ST_MODE]
        permission = stat.S_IMODE(mode)
        if mode & stat.S_IRWXG or mode & stat.S_IRWXO:
            print "%% The permissions on your configuration file (%s) are too liberal (%d)" % (CONFIG_FILE, permission)
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
        if not self.config.has_key("tmpPath"):
            self.config["tmpPath"] = "/tmp"
        self.autoEnable = self.config.get("autoEnable")

    def getTermInfo(self):
        try:
            varList = os.environ["TERMCAP"].split(':')
            co = [ x for x in varList if x.startswith("co") ][0]
            self.termwidth = int(co.split("#")[-1])
            li = [ x for x in varList if x.startswith("li") ][0]
            self.termlen = int(li.split("#")[-1]) - 4
        except:
            self.termlen = 23
            self.termwidth = 80

    def getBomConnection(self, hostName, outputHandle=sys.stdout, ignoreConfig=False):
        if not ignoreConfig and not hostName in self.config["enabledSystems"]:
            raise HostNotEnabledException(hostName)
        if not hostName in self.bomConnections:
            brc = BombardierRemoteClient(hostName, self.password, self.dataPath, outputHandle)
            self.bomConnections[hostName] = brc
        else:
            if self.password:
                self.bomConnections[hostName].configPasswd = self.password
            self.bomConnections[hostName].debug = self.debug
            self.bomConnections[hostName].refreshConfig()
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
                while extraClasses > 0:
                    slash.children.pop()
                    extraClasses -= 1
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
    from libTest import startTest, runTest, endTest 
    from Slash import slash
    status = OK
    startTest()
    mode = Mode(1,"#")
    PROMPTY="vmwareserver (root)"
    
    status = runTest(mode.reprompt, [], None, status)
    status = runTest(mode.setPrompt, [], None, status)
    status = runTest(mode.getPrompt, [], PROMPTY + "# ", status)
    status = runTest(mode.pushPrompt, [slash, "-->", 5], None, status)
    status = runTest(mode.getPrompt, [], PROMPTY + "--> ", status)
    status = runTest(mode.popPrompt, [], OK, status)
    status = runTest(mode.getPrompt, [], PROMPTY + "# ", status)
    status = runTest(mode.currentState, [], 1, status)
    
    endTest(status)
