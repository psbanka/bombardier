#!/usr/bin/env python

import sys, termios, tty
from commands import getstatusoutput
from commonUtil import *


ONE_LINE = 0
QUIT = 1
BACKUP = 2
PAGE = 3
NEUTRAL = 5
YES = 1
NO = 0
MOTD = ''

def motd():
    output = []
    try:
        f = open(MOTD, 'r')
        l = f.readlines()
        for line in l:
            output.append(line.strip())
        userOutput(output, OK, sys.stdout, sys.stderr)
    except:
        pass


def appendNotBlank(currentToken, tokens):
    currentToken = currentToken.strip()
    if currentToken:
        tokens.append(currentToken)
    return tokens

def tokenize(str):
    tokens = []
    quoteMode = False
    currentToken = ''
    appendLast = False
    comment = ''
    for i in range(0, len(str)):
        c = str[i]
        if not quoteMode:
            if c not in ['"', '#', ' ']:
                currentToken += c
                appendLast = True
                continue
            tokens = appendNotBlank(currentToken, tokens)
            if c == '"': # start quote
                quoteMode = True
            elif c == '#': # discard the rest, it's a real comment
                if i != 0 and str[i-1] == ' ':
                    appendLast = True
                else:
                    appendLast = False
                comment = str[i+1:]
                break
            elif c == ' ': # tokenize on spaces if not quoted
                currentToken = ''
                appendLast = True
        else:
            if c == '"': # end quote
                tokens = appendNotBlank(currentToken, tokens)
                currentToken = ''
                appendLast = False
                quoteMode = False
            else:
                currentToken += c
    if quoteMode: # unbalanced quotes
        raise Exception
    if appendLast:
        tokens.append(currentToken.lstrip()) # grab the last
    return tokens, comment
    
def processInput(string):
    "take a line of input from the user and convert it into an argv type structure"
    if len(string) == 0:
        return 0, 0, [], ''
    # determine if the helpflag is there (cheap hack)
    helpFlag = 0
    strlen = len(string)
    if strlen >= 1 and string[-1]=='?':
        helpFlag = 1
        string = string[:-1]
    if len(string) == 0:
        return 0, 1, [], ''
    string.lstrip() # left space is unimportant, right space is important
    tokens, comment = tokenize(string)
    # handle a preceding 'no'
    noFlag = 0
    if not tokens:
        return 0,0,[],comment
    if tokens[0] == 'no':
        noFlag = 1
        if len(tokens) > 1:
            tokens = tokens[1:]
        else:
            return 0, 0, [], comment
    # get rid of extra spaces, except at the end!
    processedData = []
    for token in tokens:
        if token != '':
            processed = False
            if token.startswith('$'):
                varName = token[1:]
                if mode.globals.has_key(varName):
                    value = mode.globals.get(varName)
                    if type(value) == type(["list"]):
                        processedData.append('[')
                        processedData += value
                        processedData.append(']')
                    else:
                        token = str(value)
                    processed = True
            if token.startswith('[') and len(token) > 1:
                processedData.append('[')
                token = token[1:]
            if token.endswith(']') and len(token) > 1:
                processedData.append(token[:-1])
                processedData.append(']')
                processed = True
            if not processed:
                processedData.append(token)
    if tokens[-1] != '':
        tokens = processedData
    else:
        tokens = processedData + ['']
    if len(tokens) == 0:
        return 0, 0, [], comment
    if tokens[0] == "ls" and len(tokens) == 1:
        tokens[0] = ''
        helpFlag = True
    return noFlag, helpFlag, tokens, comment

# User Input
def getDefault(prompt, default):
    instr = raw_input(prompt+" ["+default+"]: ")
    if instr == "":
        instr = default
    return instr

def pwdInput(prompt):
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
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
    redaction = "\b" * len(passwd) + " " * len(passwd)
    sys.stdout.write(redaction)
    print 
    return passwd
    
def getPassword(prompt = "enter password:"):
    while 1 == 1:
        passwd1 = pwdInput(prompt)
        passwd2 = pwdInput("Please re-enter password:")
        if passwd1 == passwd2:
            return passwd1
        print "% Passwords do not match\n\r"

def askYesNo(prompt, default = NEUTRAL):
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

def pageIt(outputHandle):
    outputHandle.write("--- more ---")
    outputHandle.flush()
    fd = sys.stdin.fileno()
    old_settings = termios.tcgetattr(fd)
    try:
        tty.setraw(sys.stdin.fileno())
        ch = sys.stdin.read(1)
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
    outputHandle.write("\r                \r")
    outputHandle.flush()
    if ch == chr(13):
        return ONE_LINE
    if ch == chr(113) or ch == chr(81):
        return QUIT
    if ch == chr(98) or ch == chr(66):
        return BACKUP
    return PAGE

def pagerOut(printData, outputHandle, errHandle):
    if mode.termlen == 0:
        print printData
        return
    currentLine = 0
    pagerLine = 0
    backup = 0
    printData = printData.split("\n")
    while currentLine < len(printData):
        outputHandle.write(printData[currentLine]+"\n")
        outputHandle.flush()
        currentLine += 1
        pagerLine += 1
        if not mode.batch and pagerLine > mode.termlen:
            action = pageIt(outputHandle)
            if action == QUIT:
                break
            elif action == ONE_LINE:
                backup = 0
                continue
            elif action == BACKUP:
                backupLen = (backup + 1) * mode.termlen + 1
                if backupLen > currentLine:
                    backupLen = currentLine
                outputHandle.write("\n\n-- backing up %d lines -- \n\r" % backupLen)
                outputHandle.flush()
                currentLine -= backupLen
                backup += 1
                pagerLine = 0
                continue
            else:
                backup = 0
                pagerLine = 0

def prepender(prepend, object, depth):
    printData = ''
    if type(object) == type(['list']):
        for entry in object:
            printData += prepender(prepend, entry, depth+1)
    elif type(object) == type({}):
        for entry in object:
            if type(object[entry]) == type(['list']) and len(object[entry]) == 0:
                printData += '%s%s%s: []\n' % (prepend, ' '*depth, entry)
            elif type(object[entry]) == type('string'):
                printData += '%s%s%s: \'%s\'\n' % (prepend, ' '*depth, entry, object[entry])
            elif type(object[entry]) in [ type(0), type(0.1) ]:
                printData += '%s%s%s: %d\n' % (prepend, ' '*depth, entry, object[entry])
            elif type(object[entry]) == type({}) and len(object[entry]) == 0:
                printData += '%s%s%s: {}\n' % (prepend, ' '*depth, entry)
            else:
                printData += '%s%s%s:\n' % (prepend, ' '*depth, entry)
                printData += prepender(prepend, object[entry], depth+1)
    elif type(object) in [ type('string'), type(u'unicode') ]:
        printData += "%s%s%s\n" % (prepend, ' '*depth, object)
    elif type(object) in [ type(0), type(0.1) ]:
        printData += "%s%s%d\n" % (prepend, ' '*depth, object)
    return printData
            
def userOutput(output, status, outputHandle=sys.stdout, errHandle=sys.stderr, prepend = '', test=False):
    if output == []:
        return
    if prepend == '':
        if status == FAIL:
            prepend = '% '
        else:
            prepend = ' '
    if test:
        return prepender(prepend, output, 0)
    pagerOut(prepender(prepend, output, 0), outputHandle, errHandle)
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
    #status = runTest(getPassword, ["hello>"], "", status)
    #status = runTest(processInput,["no ip packet foo"],(1, 0, ["ip","packet","foo"]), status)
    #status = runTest(processInput,[" ip packet foo"],(0, 0, ["ip","packet","foo"]), status)
    #status = runTest(processInput,["ip  packet  foo "],(0, 0, ["ip","packet","foo",""]), status)
    #status = runTest(processInput,["ip packet foo"],(0, 0, ["ip","packet","foo"]), status)
    #status = runTest(askYesNo, ["enter yes", YES], YES, status)
    #status = runTest(askYesNo, ["enter no", NO], NO, status)
    #status = runTest(askYesNo, ["enter yes"], YES, status)
    #status = runTest(userOutput, [[],OK], None, status)
    #status = runTest(userOutput, ["abc",OK], None, status)
    #status = runTest(userOutput, [["def"],OK], None, status)
    #status = runTest(userOutput, [[["hij"]],OK], None, status)
    #status = runTest(userOutput, [["klm", "nop"],OK], None, status)
    #status = runTest(userOutput, ["abc", FAIL], None, status)
    #status = runTest(userOutput, [["abc", "def"], OK, '', True], '  abc\n  def\n', status)
    #status = runTest(userOutput, [[["abc"], "def"], OK, '', True], '   abc\n  def\n', status)
    #status = runTest(userOutput, [{"abc": "def"}, OK, '', True], 'abc:\n  def\n', status)
    status = runTest(userOutput, [[{"abc": ["def", 'ghi']}, 'foo'], OK, '', True], '  abc:\n    def\n    ghi\n  foo\n', status)
    #status = runTest(userOutput, [{"abc", "def"}, OK, '', True], '  abc\n  def\n', status)
    endTest(status)
