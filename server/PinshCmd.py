#!/usr/bin/python

import readline, sys
import Mode, libUi
import StringIO
import traceback
from commonUtil import *

DEBUG    = 0
NO_MATCH = 0
PARTIAL  = 1
COMPLETE = 2

# find the names of all the objects given to me
def getNames(objects, tokens, index):
    names = []
    for obj in objects:
        if DEBUG: print "(GET NAMES) obj.myName:",obj.myName
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

    def printErrorMessage(self, tokens, index, message = "Unrecognized Command"):
        preamble = ' '*len(mode.getPrompt())
        okTokens = convertTokensToString(tokens[:index])
        print "%s%s" % (preamble, convertTokensToString(tokens))
        print "%s%s ^" % (preamble, " "*len(okTokens))
        print " %% %s" % message

    # This is called by complete.
    # When complete is calling this it wants a list of objects
    # that could be completions for the final token.
    def findCompletions(self, tokens, index):
        returnError = 1
        if DEBUG: print "findCompletions: self.myName:",`self.myName`, "tokens:",`tokens`, len(tokens)
        if len(tokens[index:]) == 0: # no tokens left, I must be who you want!
            if DEBUG: print "findCompletions: FOUND at TOP"
            return [self], index
        if tokens[index] == '':
            if len(self.children) > 0:
                output = [ child for child in self.children if child.auth <= mode.auth ]
                return output, index+1
            returnError = 0
        completionObjects = []
        incompleteObjects = []
        matchLen = 0
        if DEBUG: print "CHILDREN: ", self.children
        for child in self.children:
            if child.auth > mode.auth:
                continue
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
                #print ">>>", child.name(tokens, index)
                #print ">>>", child.name(tokens, index)[0]
                #print ">>>", child.name(tokens, index)[0].split(' ')
                #print ">>>", child.name(tokens, index)[0].split(' ')[-1]
                tokens[index+length-1] = child.name(tokens, index)[0].split(' ')[-1]
                #tokens[index] = child.name(tokens, index)[0]
                if DEBUG: print "NEW TOKEN:", tokens[index]
                return child.findCompletions(tokens, index+length)
        if len(completionObjects) == 1: # one partial match is as good as a complete match
            if index+matchLen >= len(tokens):  # NOTE: HIGHLY EXPERIMENTAL CHANGE
                return [completionObjects[0]], index+1  # NOTE: HIGHLY EXPERIMENTAL CHANGE
            return completionObjects[0].findCompletions(tokens, index+matchLen)
        elif len(completionObjects) == 0: # No matches: go away.
            if len(incompleteObjects) > 0:
                print
                self.printErrorMessage(tokens, index, "Command cannot be completed.")
                mode.reprompt()
                return [], 1
            if returnError:
                print
                self.printErrorMessage(tokens, index)
                mode.reprompt()
            return [], index
        else: # we have a few possible matches, return them all
            return completionObjects, index

    # command line completer, called with [tab] or [?] (if we could bind '?')
    def complete(self, text, status):
        if text: pass # pychecker
        try:
            if status > 0:
                if status >= len(self.names):
                    return None
                if DEBUG: print "COMPLETE: names (%s)" % (self.names[status])
                return self.names[status]
            else:
                noFlag, helpFlag, tokens, comment = libUi.processInput(readline.get_line_buffer())
                if DEBUG: print "COMPLETE: ",`tokens`
                # this is where we would process help if we could bind the '?' key properly
                index = 0
                if tokens == []:
                    if DEBUG: print "No tokens, returning children",
                    completionObjects = self.children
                else:
                    if DEBUG: print "Finding completions on",`tokens`
                    completionObjects, index = self.findCompletions(tokens, 0)
                if DEBUG: print "Found completions: ",completionObjects, index
                if len(completionObjects) == 0:
                    #mode.reprompt()
                    return None
                # status is the index of the completion that readline wants
                self.names =  getNames(completionObjects, tokens, index-1)
                if DEBUG: print "COMPLETE: tokens:",`tokens`,"index:",index,"names:",`self.names`
                if DEBUG: print "COMPLETE: names",self.names
                if len(self.names) == 1:
                    return self.names[0] + completionObjects[0].tokenDelimiter
                if self.names:
                    return self.names[0]
                return []
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
            print
            return []

    # When complete is calling this, it wants help for all the 
    # possible arguments of the last token, which should be unambiguous.
    # No return value is necessary
    def findHelp(self, tokens, index):
        if DEBUG: print "findHelp:", `self.myName`, 'tokens:', `tokens`
        # no tokens left, I must be who you want!
        if len(tokens[index:]) == 0 or tokens[index] == '': 
            if DEBUG: print "myName:",self.myName
            self.help()
            return
        if DEBUG: print "finding completions..."
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
            self.printErrorMessage(tokens, index)
            return
        else:
            self.printErrorMessage(tokens, index, "Ambiguous command")
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
                #print ">>>", child.name(tokens, index)
                #print ">>>", child.name(tokens, index)[0]
                #print ">>>", child.name(tokens, index)[0].split(' ')
                #print ">>>", child.name(tokens, index)[0].split(' ')[-1]
                #tokens[index+length-1] = child.name(tokens, index)[0].split(' ')[-1]
                tokens[index+length-1] = child.name(tokens, index)[0] # HIGHLY EXPERIMENTAL
                #tokens[index] = child.name(tokens, index)[0]
                if DEBUG: print "NEW TOKEN:", tokens[index]
                if child.cmdOwner:
                    return child.findLastResponsibleChild(tokens, index+length)
                else:
                    return self
        if len(owners) == 1: # one partial matches is as good as a complete match
            return owners[0].findLastResponsibleChild(tokens, index+matchLen)
        elif len(owners) == 0: 
            if len(arguments) > 0: # this is a valid argument, I will take responsibility.
                return self
            self.printErrorMessage(tokens, index)
            return None
        else: # more than one owner-- need to be unambiguous
            self.printErrorMessage(tokens, index, "Ambiguous command")
            return None

    # finds the correct object and runs a command
    def run(self, tokens, noFlag, mySlash):
        if tokens[-1] == '':
            tokens = tokens[:-1]
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
                                cmd = log(noFlag, tokens, status, output)
                                mode.commentCommands.append(cmd)
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
                        cmd = log(noFlag, tokens, status, output)
                        mode.commentCommands.append(cmd)
                    mode.globals["output"] = output
                    mode.globals["status"] = status
                    return status, output

    # pretty-print the help strings of all my children on this level
    def help(self):
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
                try:
                    cmd, doc = child.helpText.split('\t')
                    helpText.append([cmd, doc])
                except:
                    pass
            if len(cmd) > maxLen:
                maxLen = len(cmd)

        helpText.sort()

        for helpLine in helpText:
            cmd = helpLine[0]
            text = helpLine[1]
            spaces = maxLen - len(cmd) + 5
            print "  ",cmd+' '*spaces+text
