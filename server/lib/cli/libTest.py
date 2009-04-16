#!/usr/bin/env python

import sys, os, shutil, re
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
            for filename in files:
                try:
                    os.unlink("scratch/"+filename)
                except:
                    pass
            files = os.listdir("testfiles")
            for filename in files:
                try:
                    shutil.copyfile("testfiles/"+filename, "scratch/"+filename)
                except:
                    pass
    from time import localtime, strftime
    now = strftime("%a, %d %b %Y %H:%M:%S", localtime())
    print "================== TESTING STARTED: ",now,"\n\n"
    return OK

def assertOk(function, args, status):
    return runTest(function, args, OK, status)

def assertFail(function, args, status):
    return runTest(function, args, FAIL, status)

def runTest(function, args, expectation, status):
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

def runObject(object, command, expectedState, expectedString, status):
    import PinshCmd
    slash = PinshCmd.PinshCmd("slash")
    slash.children = [object]
    noFlag, helpFlag, tokens = libUi.processInput(command)
    if expectedString == '':
        status = runTest(slash.run, [tokens, noFlag, slash], (expectedState, []), status)
    else:
        status = runTest(slash.run, [tokens, noFlag, slash], (expectedState, [expectedString]), status)
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

