#!/usr/bin/env python

import os, sys, re, termios, tty, time, select, popen2
from commands import getstatusoutput
from commonUtil import *


ONE_LINE = 0
QUIT = 1
BACKUP = 2
PAGE = 3

def motd():
    output = []
    try:
        f = open(MOTD, 'r')
        l = f.readlines()
        for line in l:
            output.append(line.strip())
        userOutput(output, OK)
    except:
        pass
    
def processInput(string):
    "take a line of input from the user and convert it into an argv type structure"
    if len(string) == 0:
        return 0, 0, []
    # determine if the helpflag is there (cheap hack)
    helpFlag = 0
    strlen = len(string)
    if strlen >= 1 and string[-1]=='?':
        helpFlag = 1
        string = string[:-1]
    if len(string) == 0:
        return 0, 1, []
    string.lstrip() # left space is unimportant, right space is important
    # handle a preceding 'no'
    tokens = string.split(' ')
    noFlag = 0
    if tokens[0] == 'no':
        noFlag = 1
        if len(tokens) > 1:
            tokens = tokens[1:]
        else:
            return 0, 0, []
    # get rid of extra spaces, except at the end!
    processedData = []
    for token in tokens:
        if token != '':
            if token.startswith('[') and len(token) > 1:
                processedData.append('[')
                processedData.append(token[1:])
            elif token.endswith(']') and len(token) > 1:
                processedData.append(token[:-1])
                processedData.append(']')
            else:
                processedData.append(token)
    if tokens[-1] != '':
        tokens = processedData
    else:
        tokens = processedData + ['']
    if len(tokens) == 0:
        return 0, 0, []
    return noFlag, helpFlag, tokens

# User Input
def getDefault(prompt, default):
    input = raw_input(prompt+" ["+default+"]: ")
    if input == "":
        input = default
    return input

def pwdInput(prompt):
    fd = sys.stdin.fileno()
    old_settings = termios.tcgetattr(fd)
    try:
        tty.setraw(sys.stdin.fileno())
        print prompt+" ",
        passwd = ''
        while 1 == 1:
            ch = sys.stdin.read(1)
            if ch == chr(13):
                break
            if ch == chr(8) or ch == chr(127):
                if len(passwd) > 0:
                    sys.stdout("\b")
                    passwd = passwd[:-1]
                continue
            if ord(ch) > 31 and ord(ch) < 128:
                sys.stdout.write("*")
                sys.stdout.flush()
                passwd += ch
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
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
        input = raw_input(prompt)
        if len(input) == 0:
            continue
        if input.lower()[0] == 'y':
            result = YES
        elif input.lower()[0] == 'n':
            result = NO
    return result

def pageIt():
    sys.stdout.write("--- more ---")
    sys.stdout.flush()
    fd = sys.stdin.fileno()
    old_settings = termios.tcgetattr(fd)
    try:
        tty.setraw(sys.stdin.fileno())
        ch = sys.stdin.read(1)
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
    sys.stdout.write("\r                \r")
    sys.stdout.flush()
    if ch == chr(13):
        return ONE_LINE
    if ch == chr(113) or ch == chr(81):
        return QUIT
    if ch == chr(98) or ch == chr(66):
        return BACKUP
    return PAGE

def pagerOut(printData):
    if mode.termlen == 0:
        print printData
        return
    currentLine = 0
    pagerLine = 0
    backup = 0
    printData = printData.split("\n")
    while currentLine < len(printData):
        sys.stdout.write(printData[currentLine]+"\n")
        sys.stdout.flush()
        currentLine += 1
        pagerLine += 1
        if pagerLine > mode.termlen:
            action = pageIt()
            if action == QUIT:
                break
            elif action == ONE_LINE:
                backup = 0
                continue
            elif action == BACKUP:
                backupLen = (backup + 1) * mode.termlen + 1
                if backupLen > currentLine:
                    backupLen = currentLine
                sys.stdout.write("\n\n-- backing up %d lines -- \n\r" % backupLen)
                sys.stdout.flush()
                currentLine -= backupLen
                backup += 1
                pagerLine = 0
                continue
            else:
                backup = 0
                pagerLine = 0
            
def userOutput(output, status, prepend = ''):
    if output == []:
        return
    printData = ""
    if prepend == '':
        if status == FAIL:
            prepend = '% '
        else:
            prepend = ' '
    if type(output) == type(['list']):
        for entry0 in output:
            if type(entry0) == type(['list']):
                for entry1 in entry0:
                    if type(entry1) == type(['list']):
                        for entry2 in entry1:
                            if type(entry2) == type(['list']):
                                for entry3 in entry2:
                                    if type(entry3) == type("string") or type(entry3) == type(u"unicode"):
                                        printData += prepend+"    "+entry3+"\n"
                                    else:
                                        printData += prepend+"    "+`entry3`+"\n"
                            elif type(entry2) == type('string') or type(entry2) == type(u"unicode"):
                                printData += prepend+"   "+entry2+"\n"
                    elif type(entry1) == type('string') or type(entry1) == type(u"unicode"):
                        printData += prepend+"  "+entry1+"\n"
            elif type(entry0) == type('string') or type(entry0) == type(u"unicode"):
                printData += prepend+" "+entry0+"\n"
    elif type(output) == type('string') or type(output) == type(u"unicode"):
        printData += prepend+output+"\n"
    pagerOut(printData)

def user(string):
    "just print some text to stderr for user feedback"
    print >>sys.stderr
    print >>sys.stderr,string


if __name__ == "__main__":
    from libTest import *
    status = OK
    startTest()
    status = runTest(user,["hello"],None, status)
    status = runTest(getPassword, ["hello>"], "", status)
    status = runTest(processInput,["no ip packet foo"],(1, 0, ["ip","packet","foo"]), status)
    status = runTest(processInput,[" ip packet foo"],(0, 0, ["ip","packet","foo"]), status)
    status = runTest(processInput,["ip  packet  foo "],(0, 0, ["ip","packet","foo",""]), status)
    status = runTest(processInput,["ip packet foo"],(0, 0, ["ip","packet","foo"]), status)
    status = runTest(askYesNo, ["enter yes", YES], YES, status)
    status = runTest(askYesNo, ["enter no", NO], NO, status)
    status = runTest(askYesNo, ["enter yes"], YES, status)
    status = runTest(userOutput, [[],OK], None, status)
    status = runTest(userOutput, ["abc",OK], None, status)
    status = runTest(userOutput, [["def"],OK], None, status)
    status = runTest(userOutput, [[["hij"]],OK], None, status)
    status = runTest(userOutput, [["klm", "nop"],OK], None, status)
    status = runTest(userOutput, ["abc", FAIL], None, status)
    endTest(status)
