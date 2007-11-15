#!/usr/bin/env python

import sys, os, shutil, libUi
from commonUtil import *

#####################################################
## 
##  TEST HARNESSES
##

def startTest(restoreFiles = 1):
    if restoreFiles:
        if os.path.isdir("testfiles"):
            if not os.path.isdir("scratch"):
                os.makedirs("scratch")
            files = os.listdir("scratch")
            for file in files:
                try:
                    os.unlink("scratch/"+file)
                except:
                    pass
            files = os.listdir("testfiles")
            for file in files:
                try:
                    shutil.copyfile("testfiles/"+file, "scratch/"+file)
                except:
                    pass
    from time import localtime, strftime
    now = strftime("%a, %d %b %Y %H:%M:%S", localtime())
    print "================== TESTING STARTED: ",now,"\n\n"
    return OK

def runTest(function, args, expectation, status, verbose = 0):
    output = apply(function,args)

    if output == expectation:
        sys.stdout.write("#")
        sys.stdout.flush()
        return status
    else:
        print
        print "--------------------------------"
        print "ERROR IN",`function`
        print "Arguments:",`args`
        print "expected:",`expectation`
        print "received:",`output`
        print "--------------------------------"
        return status + 1

def runTest2(function, args, expectedRegex, status):
    output = apply(function,args)

    c = re.compile(expectedRegex, re.M)
    if type(output) == type("string"):
        if c.findall(output):
            sys.stdout.write("#")
            sys.stdout.flush()
            return status
    else:
        for chunk in output:
            if type(chunk) != type("string"):
                continue
            if c.findall(chunk):
                sys.stdout.write("#")
                sys.stdout.flush()
                return status
    print
    print "--------------------------------"
    print "ERROR IN:",`function`,"(",args,")",
    print "expectedRegex:",`expectedRegex`
    print "received:",`output`
    print "--------------------------------"
    return status + 1

def testMe(object, command, expectedState, expectedString, status):
    import PinshCmd
    slash = PinshCmd.PinshCmd("slash")
    slash.children = [object]
    mode.state = [2]
    mode.prompt = ["test>"]
    noFlag, helpFlag, tokens = libUi.processInput(command)
    if expectedString == '':
        status = runTest(slash.run, [tokens, noFlag, slash], (expectedState, []), status)
    else:
        status = runTest(slash.run, [tokens, noFlag, slash], (expectedState, [expectedString]), status)
    return status
    
def runObject(object, command, expectedState, expectedString, status):
    import PinshCmd
    slash = PinshCmd.PinshCmd("slash")
    slash.children = [object]
    noFlag, helpFlag, tokens = libUi.processInput(command)
    if expectedString == '':
        status = runTest(slash.run, [tokens, noFlag, slash], (expectedState, []), status, 0)
    else:
        status = runTest(slash.run, [tokens, noFlag, slash], (expectedState, [expectedString]), status, 0)
    return status

def huntForRegex(output, regex):
    while type(output) == type(["list"]) or type(output) == type((1, "tuple")):
        if len(output) == 0:
            return FAIL
        elif len(output) == 1:
            output = output[0]
        else:
            if huntForRegex(output[0], regex) == OK:
                return OK
            output = output[1:]

    if type(output) == type("string") or type(output) == type(u"unicode"):
        if regex.findall(output):
            return OK
    return FAIL

def runTest3(function, args, expectedRegex, status):
    output = apply(function,args)
    c = re.compile(expectedRegex, re.M)
    if huntForRegex(output, c) == OK:
        sys.stdout.write("#")
        sys.stdout.flush()
        return status
    print "\n--------------------------------"
    print "ERROR IN:",`function`,"(",args,")",
    print "expectedRegex:",`expectedRegex`
    print "received:",`output`
    print "--------------------------------"
    return status + 1

#this is better, but everyone else is using the first one...
def testMe2(object, command, expectedState, expectedRegex, status):
    import Mode
    import PinshCmd
    slash = PinshCmd.PinshCmd("slash")
    slash.children = [object]
    mode = Mode.Mode(2, "test>")
    noFlag, helpFlag, tokens = libUi.processInput(command)
    newStatus, output = slash.run(tokens, noFlag, slash)
    if newStatus == FAIL:
        print "--------------------------------"
        print "ERROR IN:",object, command
        print "command indicated failure and returned:", output
        print "--------------------------------"
        return status + 1
    c = re.compile(expectedRegex, re.M)
    newStatus = huntForRegex(output, c)
    if newStatus == OK:
        sys.stdout.write("#")
        sys.stdout.flush()
        return status
    print
    print "--------------------------------"
    print "ERROR IN::",object, command
    print "expectedRegex:",`expectedRegex`
    print "received:",`output`
    print "--------------------------------"
    return status + 1

def endTest(status):
    if status == OK:
        print "\n---------------------------------------------------"
        print "ALL TESTS PASSED"
        print "---------------------------------------------------\n\n"
    else:
        print "\n%%%%%%%%%%%% %d tests failed\n\n" % status
    sys.exit(0)

if __name__ == "__main__":
    status = OK
    startTest()
    c = re.compile("(abc)")
    status = runTest(huntForRegex, [[["def", "xyz"], ['feb', 'abcdef']], c], OK, status)
    status = runTest(huntForRegex, [[['feb', 'abcdef']], c], OK, status)
    endTest(status)
