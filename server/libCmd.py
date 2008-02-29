#!/usr/bin/env python

import popen2, time, select, user, re, sys
from commonUtil import *
import os

def doubleFork():
    try:
        pid = os.fork()
        if pid > 0:
            # exit first parent
            sys.exit(0)
    except OSError, e:
        print >>sys.stderr, "fork #1 failed: %d (%s)" % (e.errno, e.strerror)
        sys.exit(1)

    # Decouple from parent environment
    os.chdir("/")
    os.setsid()
    os.umask(0)

    # Do second fork
    try:
        pid = os.fork()
        if pid > 0:
            # exit from second parent; print eventual PID before exiting
            print >>sys.stderr, "Daemon PID %d" % pid
            sys.exit(0)
    except OSError, e:
        print >>sys.stderr, "fork #2 failed: %d (%s)" % (e.errno, e.strerror)
        sys.exit(1)


def watchCmd(cmd, dieOnError = 0, timeout = 0):
    stout, stin = popen2.popen4(cmd, 1)
    timeouts = 0
    output = ''
    if dieOnError:
        error = ".*(error)"
        e = re.compile(error)

    mark = time.time() + 5
    while 1 == 1:
        ready = select.select([stout], [stin], [], 0.5)
        readers = ready[0]
        writers = ready[1]
        if writers:
            stin.write("\n")
        if readers:
            outchunk = stout.read(1)
            if outchunk == '':
                break
            timeouts = 0
            output = output + outchunk
            if time.time() > mark:
                print ".",
                mark = time.time() + 5
            if dieOnError:
                if e.findall(output):
                    print "\n"
                    return FAIL, output
        else:
            if timeout > 0:
                if timeouts > timeout:
                    print "% timeout. Exiting"
                    stout.close()
                    stin.close()
                    return FAIL, output
            timeouts += 1
    print " finished."
    return OK, output


def runcmd(cmd, dumpOnError = 1):
    "run a command and print to stderr if it fails"
    o = "error"
    try:
        s,o = getstatusoutput(cmd)
        if DEBUG:
            print >>sys.stderr, ">",cmd,"<"
            print >>sys.stderr, "[",o,"]"
        if s != 0 or o.upper().rfind("ERROR") != -1:
            if dumpOnError:
                print >>sys.stderr, ">",cmd,"<"
                print >>sys.stderr, "error in command:",`o`,`s`
                return FAIL, o
            else:
                return FAIL, o 
        return OK, o
    except Exception, e:
        print "Exception"
        if dumpOnError:
            print e.__str__()
            print >>sys.stderr, ">",cmd,"<"
    return FAIL, o

def runOrError(cmd, errorString):
    status, output = runcmd(cmd,1)
    if output == "error":
        user(errorString)
        return "error"
    return output

def runUntilItWorks(cmd, tries, errorString):
    myTries = 0
    while myTries <= tries:
        status, output = runcmd(cmd,0)
        if output == "error":
            myTries += 1
        else:
            return output
    user(errorString)
    return "error"

def checkForText(cmd, text):
    # convert to regex
    reString = text.replace('*','\*').replace('.','\.').replace('+','\+').replace('-','\-')
    c = re.compile(reString, re.M)
    status, output = runcmd(cmd)
    if output == "error":
        return FAIL, output
    m = c.findall(output)
    if len(m) > 0:
        if m[0] == text:
            return OK, []
    return FAIL, output

def forkCmd(cmd, cmdVector):
    "run a command, as a child of current environment"
    try:
        pid = os.fork()
        if pid > 0:
            # exit first parent
            return
    except OSError, e:
        print >>sys.stderr, "fork #1 failed: %d (%s)" % (e.errno, e.strerror)
        return
    os.chdir("/")
    os.setsid()
    os.umask(0)
    os.execv(cmd, cmdVector)

def getAllFromProgram(regex, cmd):
    "will open a file and parse through the whole thing, looking for matches to the provided regex"
    c = re.compile(regex, re.M)
    status, output = runcmd(cmd,0)
    if output == "error":
        return ''
    matches = c.findall(output)
    if matches != None:
        if len(matches) > 0:
            return matches[0]
    return ''

if __name__ == "__main__":
    from libTest import startTest, runTest, endTest
    status = startTest()
    endTest(status)

