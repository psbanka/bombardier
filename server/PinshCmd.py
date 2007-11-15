#!/usr/bin/python

import readline, sys, os
import Mode, libUi
import StringIO
import traceback
from commonUtil import *
import logging

DEBUG = 0
NO_MATCH  = 0
PARTIAL  = 1
COMPLETE = 2

# find the names of all the objects given to me
def getNames(objects, tokens, index):
    names = []
    for obj in objects:
        if DEBUG: print "obj.myName:",obj.myName
        newName = obj.name(tokens, index)
        if type(newName) == type("string"):
            names.append(newName)
        else:
            names = names + newName
    return names

class PinshCmd:

    def __init__(self, name, helpText = "<cr>", level = Mode.ENABLE, tokenDelimiter = ' '):
        self.myName = name
        self.helpText = helpText
        self.children = []
        self.level = level
        self.auth = USER
        self.cmdOwner = 0
        self.tokenDelimiter = tokenDelimiter
        self.names = []
        self.exitMode = False
        self.logCommand = False

    def __repr__(self):
        return self.myName

    def name(self, tokens, index):
        if tokens or index: pass # pychecker
        return [self.myName]

    def match(self, tokens, index):
        if self.auth > mode.auth:
            return NO_MATCH, 1
        if tokens[index] == '':
            return PARTIAL, 1
        if self.myName == tokens[index]:
            return COMPLETE, 1
        if self.myName.startswith(tokens[index]):
            return PARTIAL, 1
        else:
            return NO_MATCH, 1

    def cmd(self, tokens, noFlag, mySlash):
        if tokens or noFlag or mySlash: pass # pychecker
        if DEBUG: print "NAME:",self.myName, self.cmdOwner
        return FAIL, ["Incomplete command."]

    # This is called by complete.
    # When complete is calling this it wants a list of objects
    # that could be completions for the final token.
    def findCompletions(self, tokens, index):
        returnError = 1
        if DEBUG: print "findCompletions: self.myName:",`self.myName`, "tokens:",`tokens`, len(tokens)
        if len(tokens[index:]) == 0: # no tokens left, I must be who you want!
            return [self], index
        if tokens[index] == '':
            if len(self.children) > 0:
                return self.children, index+1
            returnError = 0
        completionObjects = []
        incompleteObjects = []
        matchLen = 0
        if DEBUG: print "CHILDREN: ", self.children
        for child in self.children:
            matchValue, length = child.match(tokens, index)
            if matchValue == INCOMPLETE:
                if DEBUG: print "findcompletions INCOMPLETE : matchValue:",matchValue, "length:",length
                incompleteObjects.append(child)
            if matchValue == PARTIAL:
                if DEBUG: print "findcompletions PARTIAL : matchValue:",matchValue, "length:",length
                if length > matchLen:
                    matchLen = length
                completionObjects.append(child)
            elif matchValue == COMPLETE: # go see if there are more tokens!
                if DEBUG: print "findcompletions COMPLETE : matchValue:",matchValue, "length:",length
                tokens[index] = child.name(tokens, index)[0]
                if DEBUG: print "NEW TOKEN:", tokens[index]
                return child.findCompletions(tokens, index+length)
        if len(completionObjects) == 1: # one partial match is as good as a complete match
            return completionObjects[0].findCompletions(tokens, index+matchLen)
        elif len(completionObjects) == 0: # No matches: go away.
            if len(incompleteObjects) > 0:
                print "\n% command cannot be completed."
                mode.reprompt()
                return [], 1
            if returnError:
                print "\n"+convertTokensToString(tokens)+": Unrecognized command"
                mode.reprompt()
            return [], index
        else: # we have a few possible matches, return them all
            return completionObjects, index

    # command line completer, called with [tab] or [?]
    def complete(self, text, status):
        if text: pass # pychecker
        try:
            if status > 0:
                if status >= len(self.names):
                    return None
                return self.names[status]
            else:
                noFlag, helpFlag, tokens = libUi.processInput(readline.get_line_buffer())
                if DEBUG: 
                    print "COMPLETE: ",`tokens`
                if helpFlag: # Process the [?] key first
                    self.findHelp(tokens, 0)
                    mode.reprompt()
                    return None
                # this is [tab] and not [?]
                index = 0
                if tokens == []:
                    if DEBUG: print "No tokens, returning children",
                    completionObjects = self.children
                else:
                    if DEBUG: print "Finding completions on",`tokens`
                    completionObjects, index = self.findCompletions(tokens, 0)
                if DEBUG: 
                    print "Found completions: ",completionObjects, index
                if len(completionObjects) == 0:
                    #mode.reprompt()
                    return None
                # status is the index of the completion that readline wants
                self.names =  getNames(completionObjects, tokens, index-1)
                if DEBUG: 
                    print "COMPLETE: tokens:",`tokens`,"index:",index,"names:",`self.names`
                if len(self.names) == 1:
                    return self.names[0] + completionObjects[0].tokenDelimiter
                return self.names[0]
        except StandardError, e:
            sys.stderr.write("Error detected in %s (%s)." % (file, e))
            e = StringIO.StringIO()
            traceback.print_exc(file=e)
            e.seek(0)
            data = e.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            sys.stderr.write(ermsg)
            sys.stderr.write("Error ocurred in %s" % file)

    # When complete is calling this, it wants help for all the 
    # possible arguments of the last token, which should be unambiguous.
    # No return value is necessary
    def findHelp(self, tokens, index):
        if DEBUG: print "findHelp:", `self.myName`, 'tokens:', `tokens`
        # no tokens left, I must be who you want!
        if len(tokens[index:]) == 0 or tokens[index] == '': 
            self.help()
            return
        matchLen = 0
        completionObjects = []
        for child in self.children:
            matchValue, length = child.match(tokens, index)
            if matchValue == PARTIAL:
                if length > matchLen:
                    matchLen = length
                completionObjects.append(child)
            elif matchValue == COMPLETE: # go see if there are more tokens!
                child.findHelp(tokens, index+length)
                return
        if len(completionObjects) == 1: # one partial matches is as good as a complete match
            return completionObjects[0].findHelp(tokens, index+matchLen)
        elif len(completionObjects) == 0: # No matches: go away.
            print "\n",convertTokensToString(tokens)+": Unrecognized command"
            return
        else: # ^ FIXME: Provide help on the few commands that are left!
            print "\n",tokens[index]+": Ambiguous command"
            return
        
    # Run is calling this method to find the last object in the chain
    # that is willing to run cmd() on the set of tokens on the list.
    # Must be unambiguous. Assume coming into the routine that the
    # current object ("self") is a cmdOwner.
    def findLastResponsibleChild(self, tokens, index):
        if DEBUG: print "findLastResponsibleChild: ",self.myName, "[",`tokens`,"], index:", index
        if len(tokens[index:]) == 0 or tokens[index] == '': # complete and no more tokens. Object takes responsibility
            return self
        owners = []
        arguments = []
        matchLen = 0
        for child in self.children:
            matchValue, length = child.match(tokens, index)
            if DEBUG: print "Match:",child.myName, matchValue
            if matchValue == PARTIAL:
                if length > matchLen:
                    matchLen = length
                if child.cmdOwner:
                    owners.append(child)
                else:
                    arguments.append(child)
            elif matchValue == COMPLETE:
                tokens[index] = child.name(tokens, index)[0]
                if child.cmdOwner:
                    return child.findLastResponsibleChild(tokens, index+length)
                else:
                    return self
        if len(owners) == 1: # one partial matches is as good as a complete match
            return owners[0].findLastResponsibleChild(tokens, index+matchLen)
        elif len(owners) == 0: 
            if len(arguments) > 0: # this is a valid argument, I will take responsibility.
                return self
            print "\n",convertTokensToString(tokens)+": Unrecognized command" # No matches: go away.
            mode.reprompt()
            return None
        else: # more than one owner-- need to be unambiguous
            print tokens[index]+": Ambiguous command"
            return None

    # finds the correct object and runs a command
    def run(self, tokens, noFlag, mySlash):
        if mode.state[-1] == Mode.F0:
            if tokens[0].lower() != 'end':
                mode.commandBuffer[Mode.F0].append([tokens, noFlag, mySlash])
            else:
                variable, values = mode.variables[Mode.F0]
                for value in values:
                    for tokens, noFlag, mySlash in mode.commandBuffer[Mode.F0]:
                        newTokens = []
                        for token in tokens:
                            if token == '$%s' % variable:
                                newTokens.append(value)
                            else:
                                newTokens.append(token)
                        owner = self.findLastResponsibleChild(newTokens, 0)
                        if not owner:
                            continue
                        returnValue = owner.cmd(newTokens, noFlag, mySlash)
                        if not (returnValue == None and len(returnValue) != 2):
                            status = returnValue[0]
                            output = returnValue[1]
                            if owner.logCommand:
                                log(tokens, status, output)
                                mode.commentRequired = True
                            libUi.userOutput(output, status)
                        mode.globals["output"] = output
                        mode.globals["status"] = status
                extraClasses = mode.newClasses[-1]
                for i in range(0,extraClasses):
                    if i: pass # pychecker
                    mySlash.children.pop()
                mode.popPrompt()
                mode.cleanMode(mode.state[-1])
            return OK, []
        else:
            owner = self.findLastResponsibleChild(tokens, 0)
            if not owner:
                return FAIL, []
            if mode.state[-1] == Mode.F0 and not owner.exitMode:
                mode.commands.append([owner.cmd, tokens, noFlag, mySlash])
                return OK, []
            else:
                returnValue = owner.cmd(tokens, noFlag, mySlash)
                if returnValue == None or len(returnValue) != 2:
                    return OK, []
                else:
                    status = returnValue[0]
                    output = returnValue[1]
                    if owner.logCommand:
                        log(tokens, status, output)
                        mode.commentRequired = True
                    mode.globals["output"] = output
                    mode.globals["status"] = status
                    return status, output

    # pretty-print the help strings of all my children on this level
    def help(self):
        print "\n"
        helpText = []
        maxLen = 0
        if len(self.children) == 0:
            print "<cr>\n"
            return
        
        for child in self.children:
            if mode.auth < child.auth:
                continue
            if child.level < mode.state[-1]:
                continue
            if child.helpText.rfind('\n') != -1:
                for helpLine in child.helpText.split('\n'):
                    cmd, doc = helpLine.split('\t')
                    helpText.append([cmd, doc])
                    if len(cmd) > maxLen:
                        maxLen = len(cmd)
            else:
                cmd, doc = child.helpText.split('\t')
                helpText.append([cmd, doc])
            if len(cmd) > maxLen:
                maxLen = len(cmd)

        helpText.sort()

        for helpLine in helpText:
            cmd = helpLine[0]
            text = helpLine[1]
            spaces = maxLen - len(cmd) + 5
            print "  ",cmd+' '*spaces+text
