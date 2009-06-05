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

'''A set of routines that handles user input/output on the terminal'''

import sys, termios, tty
from CnmConnector import CnmConnector, UnexpectedDataException, ServerException
from SystemStateSingleton import SystemState
system_state = SystemState()
from bombardier_core.static_data import FAIL, WARNING, ERROR
from bombardier_core.static_data import USER, ADMIN
import re


ONE_LINE = 0
QUIT = 1
BACKUP = 2
PAGE = 3
NEUTRAL = 5
YES = 1
NO = 0

def login(username, logger, password=None):
    'Prompt for a password and initialize connection to the server'
    system_state.get_term_info()
    system_state.logger = logger
    if username:
        system_state.username = username
    if not system_state.username:
        system_state.username = get_default("username", "root")
    system_state.cnm_connector = CnmConnector("http://127.0.0.1:8000",
                                 system_state.username, system_state.logger)
    tries = 0
    while tries < 3:
        if not password:
            password = pwd_input("password: ")
        try:
            superuser_status = system_state.cnm_connector.login(password)
            if superuser_status:
                system_state.set_auth(ADMIN)
            else:
                system_state.set_auth(USER)
            break
        except UnexpectedDataException:
            user_output(["Bad username or password."], FAIL)
            tries += 1
        except ServerException, sex:
            user_output(["Server not accessible (%s)" % sex], FAIL)
            sys.exit(1)
        password = ''
    if tries >= 3:
        user_output(["Access denied."], FAIL)
        sys.exit(1)
    system_state.prompt = ['>']
    system_state.set_prompt()

def motd(output_handle):
    'Print out the message of the day after login'
    from bombardier_server.cli.banner import banner
    for line in banner:
        output_handle.write(line)
        output_handle.flush()

def append_not_blank(current_token, tokens):
    '''append something to the list if it isn't blank. Handles users typing
    in more than one space between tokens'''
    current_token = current_token.strip()
    if current_token:
        tokens.append(current_token)
    return tokens

def tokenize(input_str):
    '''Takes an input string and divides it into string tokens. Handles
    the word "no" in front of the string specially, handles a '?' specially
    handles quoted text as one unit, and handles comment markers'''
    tokens = []
    quote_mode = False
    current_token = ''
    append_last = False
    comment = ''
    for i in range(0, len(input_str)):
        char = input_str[i]
        if not quote_mode:
            if char not in ['"', '#', ' ']:
                current_token += char
                append_last = True
                continue
            tokens = append_not_blank(current_token, tokens)
            if char == '"': # start quote
                quote_mode = True
            elif char == '#': # discard the rest, it's a real comment
                if i != 0 and input_str[i-1] == ' ':
                    append_last = True
                else:
                    append_last = False
                comment = input_str[i+1:]
                break
            elif char == ' ': # tokenize on spaces if not quoted
                current_token = ''
                append_last = True
        else:
            if char == '"': # end quote
                tokens = append_not_blank(current_token, tokens)
                current_token = ''
                append_last = False
                quote_mode = False
            else:
                current_token += char
    if quote_mode: # unbalanced quotes
        raise Exception
    if append_last:
        tokens.append(current_token.lstrip()) # grab the last
    return tokens, comment

def process_input(string):
    '''take a line of input from the user and convert it into an argv
    type structure'''
    if len(string) == 0:
        return 0, 0, [], ''
    # determine if the help_flag is there (cheap hack)
    help_flag = 0
    strlen = len(string)
    if strlen >= 1 and string[-1] == '?':
        help_flag = 1
        string = string[:-1]
    if len(string) == 0:
        return 0, 1, [], ''
    string.lstrip() # left space is unimportant, right space is important
    tokens, comment = tokenize(string)
    # handle a preceding 'no'
    no_flag = 0
    if not tokens:
        return 0, 0, [], comment
    if tokens[0] == 'no':
        no_flag = 1
        if len(tokens) > 1:
            tokens = tokens[1:]
        else:
            return 0, 0, [], comment
    # get rid of extra spaces, except at the end!
    processed_data = []
    for token in tokens:
        if token != '':
            processed = False
            if token.startswith('$'):
                var_name = token[1:]
                if system_state.globals.has_key(var_name):
                    value = system_state.globals.get(var_name)
                    if type(value) == type(["list"]):
                        processed_data.append('[')
                        processed_data += value
                        processed_data.append(']')
                    else:
                        token = str(value)
                    processed = True
            if token.startswith('[') and len(token) > 1:
                processed_data.append('[')
                token = token[1:]
            if token.endswith(']') and len(token) > 1:
                processed_data.append(token[:-1])
                processed_data.append(']')
                processed = True
            if not processed:
                processed_data.append(token)
    if tokens[-1] != '':
        tokens = processed_data
    else:
        tokens = processed_data + ['']
    if len(tokens) == 0:
        return 0, 0, [], comment
    if tokens[0] == "ls" and len(tokens) == 1:
        tokens[0] = ''
        help_flag = True
    return no_flag, help_flag, tokens, comment

# User Input
def get_default(prompt, default):
    'prompts the user for a question and accepts a default value'
    instr = raw_input(prompt+" ["+default+"]: ")
    if instr == "":
        instr = default
    return instr

def pwd_input(prompt):
    'ask for a password, providing asterisks for output'
    file_handle = sys.stdin.fileno()
    old_settings = termios.tcgetattr(file_handle)
    try:
        tty.setraw(sys.stdin.fileno())
        print prompt,
        passwd = ''
        while 1 == 1:
            char = sys.stdin.read(1)
            if char == chr(13):
                break
            if char == chr(8) or char == chr(127):
                if len(passwd) > 0:
                    sys.stdout.write("\b")
                    sys.stdout.write(" ")
                    sys.stdout.write("\b")
                    passwd = passwd[:-1]
                continue
            if ord(char) > 31 and ord(char) < 128:
                sys.stdout.write("*")
                sys.stdout.flush()
                passwd += char
    except KeyboardInterrupt:
        sys.exit(1)
    finally:
        termios.tcsetattr(file_handle, termios.TCSADRAIN, old_settings)
    redaction = "\b" * len(passwd) + " " * len(passwd)
    sys.stdout.write(redaction)
    print
    return passwd

def get_password(prompt = "enter password:"):
    'have the user enter a password and verify it'
    while 1 == 1:
        passwd1 = pwd_input(prompt)
        passwd2 = pwd_input("Please re-enter password:")
        if passwd1 == passwd2:
            return passwd1
        print "% Passwords do not match\n\r"

def ask_yes_no(prompt, default = NEUTRAL):
    'ask a yes or no question from the user'
    if default == NEUTRAL:
        prompt += "? (y/n): "
    elif default == YES:
        prompt += "? (Y/n): "
    elif default == NO:
        prompt += "? (y/N): "
    else:
        print prompt+": YES"
        return YES
    result = NEUTRAL
    while result == NEUTRAL:
        result = default
        instr = raw_input(prompt)
        if len(instr) == 0:
            continue
        if instr.lower()[0] == 'y':
            result = YES
        elif instr.lower()[0] == 'n':
            result = NO
    return result

def page_it(output_handle):
    "asks for the user to hit enter or 'b' to backup"
    output_handle.write("--- more ---")
    output_handle.flush()
    file_handle = sys.stdin.fileno()
    old_settings = termios.tcgetattr(file_handle)
    try:
        tty.setraw(sys.stdin.fileno())
        char = sys.stdin.read(1)
    finally:
        termios.tcsetattr(file_handle, termios.TCSADRAIN, old_settings)
    output_handle.write("\r                \r")
    output_handle.flush()
    if char == chr(13):
        return ONE_LINE
    if char == chr(113) or char == chr(81):
        return QUIT
    if char == chr(98) or char == chr(66):
        return BACKUP
    return PAGE

def pager_out(print_data, output_handle):
    "pages output to the screen if there's more than the terminal will handle"
    if system_state.termlen == 0:
        print print_data
        return
    current_line = 0
    pager_line = 0
    backup = 0
    print_data = print_data.split("\n")
    while current_line < len(print_data):
        output_handle.write(print_data[current_line]+"\n")
        output_handle.flush()
        current_line += 1
        pager_line += 1
        if not system_state.batch and pager_line > system_state.termlen:
            action = page_it(output_handle)
            if action == QUIT:
                break
            elif action == ONE_LINE:
                backup = 0
                continue
            elif action == BACKUP:
                backup_len = (backup + 1) * system_state.termlen + 1
                if backup_len > current_line:
                    backup_len = current_line
                output_handle.write("\n\n-- backing up %d lines -- \n\r" % backup_len)
                output_handle.flush()
                current_line -= backup_len
                backup += 1
                pager_line = 0
                continue
            else:
                backup = 0
                pager_line = 0

def prepender(prepend, obj, depth):
    "output formatter for indented text"
    print_data = ''
    if type(obj) == type(['list']):
        for entry in obj:
            print_data += prepender(prepend, entry, depth+1)
    elif type(obj) == type({}):
        for entry in obj:
            if type(obj[entry]) == type(['list']) and len(obj[entry]) == 0:
                print_data += '%s%s%s: []\n' % (prepend, ' '*depth, entry)
            elif type(obj[entry]) == type('string'):
                print_data += '%s%s%s: \'%s\'\n' % (prepend, ' '*depth, entry, obj[entry])
            elif type(obj[entry]) in [ type(0), type(0.1) ]:
                print_data += '%s%s%s: %d\n' % (prepend, ' '*depth, entry, obj[entry])
            elif type(obj[entry]) == type({}) and len(obj[entry]) == 0:
                print_data += '%s%s%s: {}\n' % (prepend, ' '*depth, entry)
            else:
                print_data += '%s%s%s:\n' % (prepend, ' '*depth, entry)
                print_data += prepender(prepend, obj[entry], depth+1)
    elif type(obj) in [ type('string'), type(u'unicode') ]:
        print_data += "%s%s%s\n" % (prepend, ' '*depth, obj)
    elif type(obj) in [ type(0), type(0.1) ]:
        print_data += "%s%s%d\n" % (prepend, ' '*depth, obj)
    return print_data

# FIXME: CONSOLIDATE LOGGING HERE

def info(msg):
    'logging function'
    system_state.fp_out.write(">>> %s\n" % msg)

def debug(msg):
    'logging function'
    if system_state.debug == True:
        system_state.fp_out.write(">>> DEBUG: %s\n" % msg)

def warning(msg):
    'logging function'
    system_state.fp_out.write(">>> WARNING: %s\n" % msg)

def error(msg):
    'logging function'
    system_state.fp_out.write(">>> ERROR: %s\n" % msg)

def process_cnm(text_stream):
    'Handles output from the CNM and pretty-prints to the screen'
    output = re.compile("\<\<(\d)\|(\d)\|(.*?)\>\>").findall(text_stream)
    for match in output:
        _source, level, msg = match
        if level == WARNING:
            warning(msg)
        elif level == ERROR:
            error(msg)
        else:
            info(msg)

def user_output(output, status, output_handle = sys.stdout,
                _err_handle = sys.stderr, prepend = '', test = False):
    "Generic function for printing return values from a command"
    if output == []:
        return
    if prepend == '':
        if status == FAIL:
            prepend = '% '
        else:
            prepend = ' '
    if test:
        return prepender(prepend, output, 0)
    pager_out(prepender(prepend, output, 0), output_handle)
    return

def user(string):
    "just print some text to stderr for user feedback"
    print >> sys.stderr
    print >> sys.stderr, string
