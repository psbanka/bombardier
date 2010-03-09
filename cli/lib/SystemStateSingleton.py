#!/usr/bin/env python

# BSD License
# Copyright (c) 2009, Peter Banka et al
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the name of the GE Security nor the names of its contributors may
#   be used to endorse or promote products derived from this software without
#   specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

'''This thing keeps track of the state of the bombardier shell. This module
is a design compomise because readline() requires it, really.'''


import readline, socket, sys, os
import yaml
from bombardier_core.static_data import OK, FAIL, USER, ADMIN, INFO, LOG_LEVEL_LOOKUP
from bombardier_core.static_data import LOG_LEVEL_LOOKUP

PERSONAL_CONFIG_FILE = "%s/.bdr_config" % os.environ.get("HOME")

NO_COLOR  = 'none'
DARK      = "dark"
LIGHT     = "light"


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

class SystemState:
    '''A Singleton class that represents the state of the terminal'''

    class __impl:

        def __init__(self):
            self.logger = None
            self.batch = False
            self.state = []
            self.sub_prompt = None
            self.prompt = []
            self.globals = {}
            self.new_classes = []
            self.auth = USER
            self.full_prompt = ""
            self.username = None
            self.password = None
            self.comment_counter = 0
            self.bom_connections = {}
            self.default_group = "root"
            self.debug = True
            self.global_config = {}
            self.personal_config = {}
            self.editor = "/usr/bin/vim"
            self.child_processes = []
            self.termwidth = 80
            self.termlen   = 23
            self.termcolor = LIGHT
            self.cnm_url = "http://127.0.0.1:8000"
            self.cnm_connector = None
            self.fp_out = sys.stdout
            self.fp_err = sys.stderr
            self.log_level = INFO
            self.ask_to_disconnect = False

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

    def set_auth(cls, auth_level):
        "Set the current level of authorization for the user"
        SystemState.__instance.auth = auth_level

    def add_personal_config_list(cls, option, value):
        '''The 'config_list' is the set of all configuration
        values in the user's .bomsh_config YAML file.'''
        if SystemState.__instance.personal_config.get(option):
            SystemState.__instance.personal_config[option].append(value)
        else:
            SystemState.__instance.personal_config[option] = [value]
        open(PERSONAL_CONFIG_FILE, 'w').write(yaml.dump(SystemState.__instance.personal_config))

    def load_config(cls):
        '''Loads configuration options from the user's .bomsh_config file'''
        if os.path.isfile(PERSONAL_CONFIG_FILE):
            config = yaml.load(open(PERSONAL_CONFIG_FILE).read())
            if config.get("ask_to_disconnect"):
                if type(config.get("ask_to_disconnect")) == type(True):
                    SystemState.__instance.ask_to_disconnect = \
                        config.get("ask_to_disconnect")
            if config.get("editor"):
                editor = config.get("editor")
                status = os.system("which %s" % editor)
                if status == OK:
                    SystemState.__instance.editor = editor
                else:
                    msg = "%% Editor %s cannot be found (using %s)."
                    print msg % (editor, self.editor)
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
                else:
                    SystemState.__instance.termcolor = NO_COLOR
            log_level = config.get("log_level", 'info').upper()
            if log_level in LOG_LEVEL_LOOKUP:
                SystemState.__instance.log_level = LOG_LEVEL_LOOKUP[log_level]
            cnm_url = config.get("cnm_url")
            if type(cnm_url) == type(''):
                SystemState.__instance.cnm_url = cnm_url
            debug = config.get("debug")
            if type(debug) == type(True):
                SystemState.__instance.debug = debug
            SystemState.__instance.editor = config.get("editor", "/usr/bin/vim")
            SystemState.__instance.personal_config = config

    def set_output(cls, fp_out):
        'set the output file handler'
        SystemState.__instance.fp_out = fp_out

    def set_err(cls, fp_err):
        'set the error file handler'
        SystemState.__instance.fp_err = fp_err

    def get_term_info(cls):
        '''tries to get infomration about the user's terminal'''
        try:
            var_list = os.environ["TERMCAP"].split(':')
            co = [ x for x in var_list if x.startswith("co") ][0]
            SystemState.__instance.termwidth = int(co.split("#")[-1])
            li = [ x for x in var_list if x.startswith("li") ][0]
            SystemState.__instance.termlen = int(li.split("#")[-1]) - 4
        except:
            SystemState.__instance.termlen = 23
            SystemState.__instance.termwidth = 80

    def set_prompt(cls):
        'used to set the current prompt'
        new_prompt = ''
        for state in SystemState.__instance.state:
            new_prompt += ('( %s )' % ' '.join(state))
        if not new_prompt:
            new_prompt =  "%s " % socket.gethostname()
            new_prompt += "(%s)" % (SystemState.__instance.username)
        new_prompt += "%s " % SystemState.__instance.prompt[-1]
        SystemState.__instance.full_prompt = new_prompt

    def push_prompt(cls, new_tokens):
        '''Used to enter a new sub-mode
        new_tokens -- completed tokens to be added to the prompt
        '''
        SystemState.__instance.state.append(new_tokens)
        cls.set_prompt()

    def set_comment_counter(self):
        'Set the number of comments I am tracking'
        cnm_connector = SystemState.__instance.cnm_connector
        SystemState.__instance.comment_counter = len(cnm_connector.get_uncommented_jobs())

    def get_prompt(cls):
        'used to find what the current prompt should look like'
        comment_string = ''
        if SystemState.__instance.comment_counter:
            comment_string = "(*%d) " % SystemState.__instance.comment_counter
        return comment_string+SystemState.__instance.full_prompt

    def pop_prompt(cls):
        'used to exit a sub-mode'
        if len(SystemState.__instance.state) > 0:
            SystemState.__instance.state.pop()
            cls.set_prompt()
            return OK
        else:
            return FAIL

    def get_state_tokens(cls, tokens):
        'this will return a modified list of tokens based on the current state'
        new_tokens = []
        for token_list in SystemState.__instance.state:
            new_tokens.extend(token_list)
        if new_tokens and tokens == []:
            tokens = ['']
        return new_tokens + tokens

    def reprompt(cls):
        'This is used under some weird conditions that I forget'
        print >> sys.stderr
        print >> sys.stderr, cls.get_prompt() + readline.get_line_buffer(),

    def current_state(cls):
        return SystemState.__instance.state[-1]

