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
from bombardier_core.static_data import OK, FAIL
from bombardier_core.static_data import WARNING, ERROR, CRITICAL, DEBUG, INFO
from bombardier_core.static_data import USER, ADMIN, LOG_LEVEL_LOOKUP
from bombardier_core.static_data import GOOD_COLOR, WARNING_COLOR, STRONG_COLOR
from bombardier_core.static_data import WEAK_COLOR, NO_COLOR

import re


ONE_LINE = 0
QUIT = 1
BACKUP = 2
PAGE = 3
NEUTRAL = 5
YES = 1
NO = 0

def login(username, password=None):
    'Prompt for a password and initialize connection to the server'
    try:
        system_state.get_term_info()
        if username:
            system_state.username = username
        if not system_state.username:
            try:
                system_state.username = get_default("username", "root")
            except KeyboardInterrupt:
                print "\n%% Aborted login"
                sys.exit(1)
        else:
            print "Logging in as %s" % system_state.username
        system_state.cnm_connector = CnmConnector(system_state.cnm_url,
                                     system_state.username)
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
            except UnexpectedDataException, ude:
                print ude
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
    except Exception, ste:
        process_traceback(ste)

def motd():
    'Print out the message of the day after login'
    from banner import banner
    for line in banner:
        system_state.fp_out.write(line)
        system_state.fp_out.flush()

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
        password = ''
        while 1 == 1:
            char = sys.stdin.read(1)
            if char == chr(3): # ^C was pressed
                raise KeyboardInterrupt
            if char == chr(13): # Enter was pressed
                break
            if char == chr(8) or char == chr(127): # backspace
                if len(password) > 0:
                    sys.stdout.write("\b")
                    sys.stdout.write(" ")
                    sys.stdout.write("\b")
                    password = password[:-1]
                continue
            if ord(char) > 31 and ord(char) < 128: # Valid character
                sys.stdout.write("*")
                sys.stdout.flush()
                password += char
    except KeyboardInterrupt:
        sys.exit(1)
    finally:
        termios.tcsetattr(file_handle, termios.TCSADRAIN, old_settings)
    redaction = "\b" * len(password) + " " * len(password)
    sys.stdout.write(redaction)
    print
    return password

def get_password(prompt = "enter password:"):
    'have the user enter a password and verify it'
    while 1 == 1:
        password1 = pwd_input(prompt)
        password2 = pwd_input("Please re-enter password:")
        if password1 == password2:
            return password1
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
        try:
            instr = raw_input(prompt)
        except EOFError:
            continue
        if len(instr) == 0:
            continue
        if instr.lower()[0] == 'y':
            result = YES
        elif instr.lower()[0] == 'n':
            result = NO
    return result

def page_it():
    "asks for the user to hit enter or 'b' to backup"
    system_state.fp_out.write("--- more ---")
    system_state.fp_out.flush()
    file_handle = sys.stdin.fileno()
    old_settings = termios.tcgetattr(file_handle)
    try:
        tty.setraw(sys.stdin.fileno())
        char = sys.stdin.read(1)
    finally:
        termios.tcsetattr(file_handle, termios.TCSADRAIN, old_settings)
    system_state.fp_out.write("\r                \r")
    system_state.fp_out.flush()
    if char == chr(13):
        return ONE_LINE
    if char == chr(113) or char == chr(81):
        return QUIT
    if char == chr(98) or char == chr(66):
        return BACKUP
    return PAGE

def pager_out(print_data):
    "pages output to the screen if there's more than the terminal will handle"
    if system_state.termlen == 0:
        print print_data
        return
    current_line = 0
    pager_line = 0
    backup = 0
    print_data = print_data.split("\n")
    while current_line < len(print_data):
        system_state.fp_out.write(print_data[current_line]+"\n")
        system_state.fp_out.flush()
        current_line += 1
        pager_line += 1
        if not system_state.batch and pager_line > system_state.termlen:
            action = page_it()
            if action == QUIT:
                break
            elif action == ONE_LINE:
                backup = 0
                continue
            elif action == BACKUP:
                backup_len = (backup + 1) * system_state.termlen + 1
                if backup_len > current_line:
                    backup_len = current_line
                system_state.fp_out.write("\n\n-- backing up %d lines -- \n\r" % backup_len)
                system_state.fp_out.flush()
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
    if system_state.termcolor != NO_COLOR:
        color_code = STRONG_COLOR[system_state.termcolor]
        msg = "\033%s%s\033[m" % (color_code, msg)
    system_state.fp_out.write(">>> WARNING: %s\n" % msg)

def error(msg):
    'logging function'
    system_state.fp_out.write(">>> ERROR: %s\n" % msg)

def process_cnm(server_output_lines):
    'Handles output from the CNM and pretty-prints to the screen'
    for line in server_output_lines:
        line = line.strip()
        if line.endswith('|'):
            line = line[:-1]
        if not line:
            continue
        components = line.split('|')
        if len(components) >= 4:
            log_level_string = components[1]
            log_level = LOG_LEVEL_LOOKUP.get(log_level_string, CRITICAL)
            if log_level >= system_state.log_level:
                date = components[0]
                job_name = components[2]
                if system_state.termcolor != NO_COLOR:
                    color_code = None
                    if log_level == DEBUG:
                        color_code = WEAK_COLOR[system_state.termcolor]
                    elif log_level == WARNING:
                        color_code = STRONG_COLOR[system_state.termcolor]
                    elif log_level > WARNING:
                        color_code = WARNING_COLOR[system_state.termcolor]
                    elif log_level != INFO:
                        print "=== UNKNOWN log level: ", log_level
                        color_code = WARNING_COLOR[system_state.termcolor]
                    if color_code != None:
                        prefix = "  | %20s | " % (job_name)
                        message = "%s\033%s%s\033[m\n" % (prefix, color_code, '|'.join(components[3:]))
                    else:
                        message = "  | %20s | %s\n" % (job_name, '|'.join(components[3:]))
                else:
                    message = "  | %20s | %s\n" % (job_name, '|'.join(components[3:]))
                system_state.fp_out.write(message)
        else:
            system_state.fp_out.write("-- %s\n" % line )


def user_output(output, status, prepend = '', test = False):
    "Generic function for printing return values from a command"
    if output == []:
        return
    if type(output) == type({}):
        if "command_status" in output:
            status = output["command_status"]
            del output["command_status"]
    if prepend == '':
        if status == FAIL:
            prepend = '% '
        else:
            prepend = ' '
    if test:
        return prepender(prepend, output, 0)
    pager_out(prepender(prepend, output, 0))
    return

def process_traceback(machine_exception):
    output = {"Exception": str(machine_exception),
              "data": machine_exception.traceback}
    user_output(output, FAIL)

def user(string):
    "just print some text to stderr for user feedback"
    print >> sys.stderr
    print >> sys.stderr, string
