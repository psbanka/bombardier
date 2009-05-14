#!/usr/bin/env python

import readline, socket, sys, os
import stat
import yaml, syck
from BombardierRemoteClient import BombardierRemoteClient
from bombardier_core.Logger import Logger
from bombardier_core.static_data import OK, FAIL

PERSONAL_CONFIG_FILE = "%s/.bomsh_config" % os.environ.get("HOME")

NO_COLOR  = 'none'
DARK      = "dark"
LIGHT     = "light"

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

class SystemState:

    class __impl:

        def __init__(self):
            self.logger = None
            self.batch = False
            self.state = [USER]
            self.exit_methods = []
            self.prompt = []
            self.command_buffer = {F0:[], F1:[], F2:[]}
            self.variables = {F0:[], F1:[], F2:[]}
            self.globals = {}
            self.new_classes = []
            self.auth = USER
            self.full_prompt = ""
            self.username = None
            self.password = None
            self.comment_commands = []
            self.bom_connections = {}
            self.default_group = "root"
            self.debug = True
            self.global_config = {}
            self.personal_config = {}
            self.editor = "/usr/bin/vim"
            self.child_processes = []
            self.termwidth = 80
            self.termlen   = 23
            self.termcolor = NO_COLOR
            self.cnm_connector = None


    __instance = None

    def __init__(self):
        """ Create singleton instance """
        # Check whether we already have an instance
        if SystemState.__instance is None:
            # Create and remember instance
            SystemState.__instance = SystemState.__impl()

        # Store instance reference as the only member in the handle
        self.__dict__['_Singleton__instance'] = SystemState.__instance

    def __getattr__(self, attr):
        """ Delegate access to implementation """
        return getattr(self.__instance, attr)

    def __setattr__(self, attr, value):
        """ Delegate access to implementation """
        return setattr(self.__instance, attr, value)

    def add_personal_config_list(cls, option, value):
        if SystemState.__instance.personal_config.get(option):
            SystemState.__instance.personal_config[option].append(value)
        else:
            SystemState.__instance.personal_config[option] = [value]
        open(PERSONAL_CONFIG_FILE, 'w').write(yaml.dump(SystemState.__instance.personal_config))

    def load_config(cls):
        if os.path.isfile(PERSONAL_CONFIG_FILE):
            config = syck.load(open(PERSONAL_CONFIG_FILE).read())
            if config.get("username"):
                SystemState.__instance.username = config.get("username")
            if config.get("termwidth"):
                SystemState.__instance.termwidth = config.get("termwidth")
            termcolor = config.get("termcolor")
            if termcolor:
                if termcolor.upper().startswith("D"):
                    SystemState.__instance.termcolor = DARK
                elif termcolor.upper().startswith("L"):
                    SystemState.__instance.termcolor = LIGHT
            debug = config.get("debug")
            if type(debug) == type(True):
                SystemState.__instance.debug = debug
            SystemState.__instance.editor = config.get("editor", "/usr/bin/vim")
            SystemState.__instance.personal_config=config

    def get_term_info(cls):
        try:
            var_list = os.environ["TERMCAP"].split(':')
            co = [ x for x in var_list if x.startswith("co") ][0]
            SystemState.__instance.termwidth = int(co.split("#")[-1])
            li = [ x for x in var_list if x.startswith("li") ][0]
            SystemState.__instance.termlen = int(li.split("#")[-1]) - 4
        except:
            SystemState.__instance.termlen = 23
            SystemState.__instance.termwidth = 80

    def clean_mode(cls, state):
        SystemState.__instance.command_buffer[state] = []
        SystemState.__instance.variables[state] = []
        SystemState.__instance.exit_methods = SystemState.__instance.exit_methods[:-1]

    def set_prompt(cls):
        if cls.current_state() != FREE_TEXT:
            SystemState.__instance.full_prompt = "%s (%s)%s " %(socket.gethostname(),SystemState.__instance.username, SystemState.__instance.prompt[-1])
        else:
            SystemState.__instance.full_prompt = ''

    def get_prompt(cls):
        comment_string = ''
        if SystemState.__instance.comment_commands:
            comment_string = "(*)"
        return comment_string+SystemState.__instance.full_prompt

    def push_prompt(cls, slash, new_prompt, new_state, new_classes = 0):
        # pop out of any equal or higher levels
        while SystemState.__instance.state[-1]+1 > new_state:
            if len(SystemState.__instance.state) > 1:
                extra_classes = SystemState.__instance.new_classes[-1]
                while extra_classes > 0:
                    slash.children.pop()
                    extra_classes -= 1
            else:
                SystemState.__instance.state = [ENABLE]
                break
            cls.pop_prompt()
        SystemState.__instance.state.append(new_state)
        SystemState.__instance.prompt.append(new_prompt)
        SystemState.__instance.new_classes.append(new_classes)
        cls.set_prompt()

    def pop_prompt(cls):
        if len(SystemState.__instance.state) > 1:
            SystemState.__instance.prompt.pop()
            SystemState.__instance.state.pop()
            SystemState.__instance.new_classes.pop()
            cls.set_prompt()
            return OK
        else:
            return FAIL

    def reprompt(cls):
        if REPROMPT: #^ FIXME
            print >>sys.stderr
            print >>sys.stderr,cls.get_prompt()+readline.get_line_buffer(),

    def current_state(cls):
        return SystemState.__instance.state[-1]

if __name__ == "__main__":
    from libTest import startTest, runTest, endTest
    from Slash import slash
    status = OK
    startTest()
    mode = Mode(1,"#")
    PROMPTY="vmwareserver (root)"

    status = runTest(mode.reprompt, [], None, status)
    status = runTest(mode.set_prompt, [], None, status)
    status = runTest(mode.get_prompt, [], PROMPTY + "# ", status)
    status = runTest(mode.push_prompt, [slash, "-->", 5], None, status)
    status = runTest(mode.get_prompt, [], PROMPTY + "--> ", status)
    status = runTest(mode.pop_prompt, [], OK, status)
    status = runTest(mode.get_prompt, [], PROMPTY + "# ", status)
    status = runTest(mode.current_state, [], 1, status)

    endTest(status)
