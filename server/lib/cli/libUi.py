#!/usr/bin/env python

import sys, termios, tty
from commands import getstatusoutput
from CnmConnector import CnmConnector, UnexpectedDataException
from bombardier_server.cli.SystemStateSingleton import SystemState, ENABLE, USER, F0
system_state = SystemState()
from bombardier_core.static_data import OK, FAIL, SERVER, TRACEBACK
from bombardier_core.static_data import DEBUG, INFO, WARNING, ERROR, CRITICAL
import re



ONE_LINE = 0
QUIT = 1
BACKUP = 2
PAGE = 3
NEUTRAL = 5
YES = 1
NO = 0

def login(username, logger):
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
        password = pwd_input("password: ")
        try:
            superuser_status = system_state.cnm_connector.login(password)
            break
        except UnexpectedDataException:
            user_output(["Bad username or password."], FAIL)
            tries += 1
    if tries >= 3:
        user_output(["Access denied."], FAIL)
        sys.exit(1)
    system_state.prompt = ['>']
    system_state.set_prompt()

def motd(output_handle):
    from bombardier_server.cli.banner import banner
    output = []
    for line in banner:
        output_handle.write(line)
        output_handle.flush()

def append_not_blank(current_token, tokens):
    current_token = current_token.strip()
    if current_token:
        tokens.append(current_token)
    return tokens

def tokenize(str):
    tokens = []
    quote_mode = False
    current_token = ''
    append_last = False
    comment = ''
    for i in range(0, len(str)):
        c = str[i]
        if not quote_mode:
            if c not in ['"', '#', ' ']:
                current_token += c
                append_last = True
                continue
            tokens = append_not_blank(current_token, tokens)
            if c == '"': # start quote
                quote_mode = True
            elif c == '#': # discard the rest, it's a real comment
                if i != 0 and str[i-1] == ' ':
                    append_last = True
                else:
                    append_last = False
                comment = str[i+1:]
                break
            elif c == ' ': # tokenize on spaces if not quoted
                current_token = ''
                append_last = True
        else:
            if c == '"': # end quote
                tokens = append_not_blank(current_token, tokens)
                current_token = ''
                append_last = False
                quote_mode = False
            else:
                current_token += c
    if quote_mode: # unbalanced quotes
        raise Exception
    if append_last:
        tokens.append(current_token.lstrip()) # grab the last
    return tokens, comment

def process_input(string):
    "take a line of input from the user and convert it into an argv type structure"
    if len(string) == 0:
        return 0, 0, [], ''
    # determine if the help_flag is there (cheap hack)
    help_flag = 0
    strlen = len(string)
    if strlen >= 1 and string[-1]=='?':
        help_flag = 1
        string = string[:-1]
    if len(string) == 0:
        return 0, 1, [], ''
    string.lstrip() # left space is unimportant, right space is important
    tokens, comment = tokenize(string)
    # handle a preceding 'no'
    no_flag = 0
    if not tokens:
        return 0,0,[],comment
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
    instr = raw_input(prompt+" ["+default+"]: ")
    if instr == "":
        instr = default
    return instr

def pwd_input(prompt):
    fd = sys.stdin.fileno()
    old_settings = termios.tcgetattr(fd)
    try:
        tty.setraw(sys.stdin.fileno())
        print prompt,
        passwd = ''
        while 1 == 1:
            ch = sys.stdin.read(1)
            if ch == chr(13):
                break
            if ch == chr(8) or ch == chr(127):
                if len(passwd) > 0:
                    sys.stdout.write("\b")
                    sys.stdout.write(" ")
                    sys.stdout.write("\b")
                    passwd = passwd[:-1]
                continue
            if ord(ch) > 31 and ord(ch) < 128:
                sys.stdout.write("*")
                sys.stdout.flush()
                passwd += ch
    except KeyboardInterrupt:
        sys.exit(1)
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
    redaction = "\b" * len(passwd) + " " * len(passwd)
    sys.stdout.write(redaction)
    print
    return passwd

def get_password(prompt = "enter password:"):
    while 1 == 1:
        passwd1 = pwd_input(prompt)
        passwd2 = pwd_input("Please re-enter password:")
        if passwd1 == passwd2:
            return passwd1
        print "% Passwords do not match\n\r"

def ask_yes_no(prompt, default = NEUTRAL):
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
    output_handle.write("--- more ---")
    output_handle.flush()
    fd = sys.stdin.fileno()
    old_settings = termios.tcgetattr(fd)
    try:
        tty.setraw(sys.stdin.fileno())
        ch = sys.stdin.read(1)
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
    output_handle.write("\r                \r")
    output_handle.flush()
    if ch == chr(13):
        return ONE_LINE
    if ch == chr(113) or ch == chr(81):
        return QUIT
    if ch == chr(98) or ch == chr(66):
        return BACKUP
    return PAGE

def pager_out(print_data, output_handle, errHandle):
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

def prepender(prepend, object, depth):
    print_data = ''
    if type(object) == type(['list']):
        for entry in object:
            print_data += prepender(prepend, entry, depth+1)
    elif type(object) == type({}):
        for entry in object:
            if type(object[entry]) == type(['list']) and len(object[entry]) == 0:
                print_data += '%s%s%s: []\n' % (prepend, ' '*depth, entry)
            elif type(object[entry]) == type('string'):
                print_data += '%s%s%s: \'%s\'\n' % (prepend, ' '*depth, entry, object[entry])
            elif type(object[entry]) in [ type(0), type(0.1) ]:
                print_data += '%s%s%s: %d\n' % (prepend, ' '*depth, entry, object[entry])
            elif type(object[entry]) == type({}) and len(object[entry]) == 0:
                print_data += '%s%s%s: {}\n' % (prepend, ' '*depth, entry)
            else:
                print_data += '%s%s%s:\n' % (prepend, ' '*depth, entry)
                print_data += prepender(prepend, object[entry], depth+1)
    elif type(object) in [ type('string'), type(u'unicode') ]:
        print_data += "%s%s%s\n" % (prepend, ' '*depth, object)
    elif type(object) in [ type(0), type(0.1) ]:
        print_data += "%s%s%d\n" % (prepend, ' '*depth, object)
    return print_data

# FIXME: CONSOLIDATE LOGGING HERE

def info(msg):
    print ">>>",msg

def debug(msg, system_status):
    if system_status.debug == True:
        print ">>> DEBUG:",msg

def warning(msg):
    print ">>> WARNING:",msg

def error(msg):
    print ">>> ERROR:",msg

def process_cnm(text_stream):
    output = re.compile("\<\<(\d)\|(\d)\|(.*?)\>\>").findall(text_stream)
    for match in output:
        source, level, msg = match
        if level == WARNING:
            warning(msg)
        elif level == ERROR:
            error(msg)
        else:
            info(msg)


def user_output(output, status, output_handle=sys.stdout, errHandle=sys.stderr, prepend = '', test=False):
    if output == []:
        return
    if prepend == '':
        if status == FAIL:
            prepend = '% '
        else:
            prepend = ' '
    if test:
        return prepender(prepend, output, 0)
    pager_out(prepender(prepend, output, 0), output_handle, errHandle)
    return

def user(string):
    "just print some text to stderr for user feedback"
    print >>sys.stderr
    print >>sys.stderr,string


if __name__ == "__main__":
    from libTest import startTest, runTest, endTest
    status = OK
    startTest()
    #status = runTest(user,["hello"],None, status)
    #status = runTest(get_password, ["hello>"], "", status)
    #status = runTest(process_input,["no ip packet foo"],(1, 0, ["ip","packet","foo"]), status)
    #status = runTest(process_input,[" ip packet foo"],(0, 0, ["ip","packet","foo"]), status)
    #status = runTest(process_input,["ip  packet  foo "],(0, 0, ["ip","packet","foo",""]), status)
    #status = runTest(process_input,["ip packet foo"],(0, 0, ["ip","packet","foo"]), status)
    #status = runTest(ask_yes_no, ["enter yes", YES], YES, status)
    #status = runTest(ask_yes_no, ["enter no", NO], NO, status)
    #status = runTest(ask_yes_no, ["enter yes"], YES, status)
    #status = runTest(user_output, [[],OK], None, status)
    #status = runTest(user_output, ["abc",OK], None, status)
    #status = runTest(user_output, [["def"],OK], None, status)
    #status = runTest(user_output, [[["hij"]],OK], None, status)
    #status = runTest(user_output, [["klm", "nop"],OK], None, status)
    #status = runTest(user_output, ["abc", FAIL], None, status)
    #status = runTest(user_output, [["abc", "def"], OK, '', True], '  abc\n  def\n', status)
    #status = runTest(user_output, [[["abc"], "def"], OK, '', True], '   abc\n  def\n', status)
    #status = runTest(user_output, [{"abc": "def"}, OK, '', True], 'abc:\n  def\n', status)
    status = runTest(user_output, [[{"abc": ["def", 'ghi']}, 'foo'], OK, '', True], '  abc:\n    def\n    ghi\n  foo\n', status)
    #status = runTest(user_output, [{"abc", "def"}, OK, '', True], '  abc\n  def\n', status)
    endTest(status)
