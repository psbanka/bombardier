#!/usr/bin/env python

import readline, socket, sys, os
import stat
import yaml, syck
from BombardierRemoteClient import BombardierRemoteClient

PERSONAL_CONFIG_FILE = "%s/.bomsh_config" % os.environ.get("HOME")

NO_COLOR  = 'none'
DARK      = "dark"
LIGHT     = "light"

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

class ConfigFileException(Exception):
    def __init__(self, message, fileName):
        e = Exception()
        Exception.__init__(e)
        self.fileName = fileName
        self.message = message
    def __repr__(self):
        return "%% Error processing config file %s: %s" \
               %(self.fileName, self.message)
    def __str__(self):
        return self.__repr__()

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
        self.username = ''
        self.password = ''
        self.commentCommands = []
        self.bomConnections = {}
        self.defaultGroup = "root"
        self.enabledSystems = []
        self.getTermInfo()
        self.debug = True
        self.global_config = {}
        self.personal_config = {}
        self.username = os.environ.get("USER")
        self.autoEnable = False
        self.editor = "/usr/bin/vim"
        self.setPrompt()
        self.childProcesses = []
        self.termwidth = 80
        self.termlen   = 23
        self.termcolor = NO_COLOR

    def addPersonalConfigList(self, option, value):
        if self.personal_config.get(option):
            self.personal_config[option].append(value)
        else:
            self.personal_config[option] = [value]
        open(PERSONAL_CONFIG_FILE, 'w').write(yaml.dump(self.personal_config))

    def loadConfig(self):

        if os.path.isfile(PERSONAL_CONFIG_FILE):
            self.personal_config=syck.load(open(PERSONAL_CONFIG_FILE).read())
            self.enabledSystems = self.personal_config.get("enabledSystems", [])
            self.autoEnable = self.personal_config.get("autoEnable")
            if self.personal_config.get("termwidth"):
                self.termwidth = self.personal_config.get("termwidth")
            termcolor = self.personal_config.get("termcolor")
            if termcolor:
                if termcolor.upper().startswith("D"):
                    self.termcolor = DARK
                elif termcolor.upper().startswith("L"):
                    self.termcolor = LIGHT
            debug = self.personal_config.get("debug")
            if type(debug) == type(True):
                self.debug = debug
            self.editor = self.personal_config.get("editor", "/usr/bin/vim")

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

    def getBomConnection(self, hostName, outputHandle=sys.stdout, ignoreConfig=False, enabled=True):
        if not ignoreConfig and not hostName in self.enabledSystems:
            raise HostNotEnabledException(hostName)
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
