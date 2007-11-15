#!/usr/bin/env python

import readline, socket, sys, os

OK = 0
FAIL = 1
UNKNOWN = -1

# Authorization levels
USER = 0
ADMIN = 1

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

REPROMPT = 1

class Mode:
    def __init__(self, state, prompt):
        self.state = [state]
        self.prompt = [prompt]
        self.commandBuffer = {F0:[], F1:[], F2:[]}
        self.variables = {F0:[], F1:[], F2:[]}
        self.globals = {}
        self.setPrompt()
        self.termlen = 23
        self.newClasses = []
        self.auth = USER
        self.fullPrompt = ""
        self.password = ''
        self.commentRequired = False

    def cleanMode(self, state):
        self.commandBuffer[state] = []
        self.variables[state] = []

    def setPrompt(self):
        if self.currentState() != FREE_TEXT:
            self.fullPrompt = socket.gethostname()+self.prompt[-1]+' '
        else:
            self.fullPrompt = ''

    def getPrompt(self):
        commentString = ''
        if self.commentRequired:
            commentString = "(*)"
        return commentString+self.fullPrompt

    def pushPrompt(self, slash, newPrompt, newState, newClasses = 0):
        # pop out of any equal or higher levels
        while self.state[-1]+1 > newState:
            if len(self.state) > 1:
                extraClasses = self.newClasses[-1]
                for i in range(0,extraClasses):
                    slash.children.pop()
            else:
                self.state = [ENABLE]
                break
            self.popPrompt()
        self.state.append(newState)
        self.prompt.append(newPrompt)
        self.newClasses.append(newClasses)
        self.setPrompt()

    def popPrompt(self):
        if len(self.state) > 1:
            self.prompt.pop()
            self.state.pop()
            self.newClasses.pop()
            self.setPrompt()
            return OK
        else:
            return FAIL

    def reprompt(self):
        if REPROMPT: #^ FIXME
            print >>sys.stderr
            commentString = ''
            if self.commentRequired:
                commentString = "(*)"
            print >>sys.stderr,self.getPrompt()+readline.get_line_buffer(),

    def currentState(self):
        return self.state[-1]
    
if __name__ == "__main__":
    from libTest import *
    status = OK
    startTest()
    mode = Mode(1,"#")
    
    status = runTest(getPromptFile, [], "testing", status)
    status = runTest(reprompt, [], None, status)
    status = runTest(mode.setPrompt, [], None, status)
    status = runTest(getPromptFile, [], "gruyere# ", status)
    status = runTest(mode.getPrompt, [], "gruyere# ", status)
    status = runTest(mode.pushPrompt, ["-->", 5], None, status)
    status = runTest(mode.getPrompt, [], "gruyere--> ", status)
    status = runTest(mode.popPrompt, [], OK, status)
    status = runTest(mode.getPrompt, [], "gruyere# ", status)
    status = runTest(mode.currentState, [], 1, status)
    
    endTest(status)
