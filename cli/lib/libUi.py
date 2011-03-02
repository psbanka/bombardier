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
from bombardier_core.static_data import FAIL
from bombardier_core.static_data import WARNING, CRITICAL, DEBUG, INFO
from bombardier_core.static_data import USER, ADMIN, LOG_LEVEL_LOOKUP
from bombardier_core.static_data import WARNING_COLOR, STRONG_COLOR
from bombardier_core.static_data import WEAK_COLOR, NO_COLOR
import CommandLine


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
        sys.exit(1)

def motd():
    'Print out the message of the day after login'
    from banner import banner
    for line in banner:
        system_state.fp_out.write(line)
        system_state.fp_out.flush()

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
        termios.tcsetattr(file_handle, termios.TCSADRAIN, old_settings)
        sys.exit(1)
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
        except KeyboardInterrupt:
            print
            instr = 'n'
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
    "Pretty-print a machine traceback to the screen"
    output = {"Exception": str(machine_exception)}
    if hasattr(machine_exception, "traceback"):
        output["data"] = machine_exception.traceback
    user_output(output, FAIL)

def user(string):
    "just print some text to stderr for user feedback"
    print >> sys.stderr
    print >> sys.stderr, string
