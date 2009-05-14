#!/usr/bin/python

import readline, sys
import libUi
import StringIO
import traceback
from bombardier_core.static_data import DEBUG, PARTIAL, COMPLETE, INCOMPLETE, NO_MATCH, FAIL, OK
from bombardier_server.cli.SystemStateSingleton import SystemState, ENABLE, USER, F0
system_state = SystemState()

# find the names of all the objects given to me
def convert_tokens_to_string(tokens, delimeter=' '):
    retVal = ''
    for token in tokens:
        retVal += token+delimeter
    return retVal[:-1]

def get_names(objects, tokens, index):
    names = []
    for obj in objects:
        if DEBUG: print "(GET NAMES) obj.my_name:",obj.my_name
        new_name = obj.preferred_names(tokens, index)
        if type(new_name) == type("string"):
            names.append(new_name)
        else:
            names = names + new_name
    return names

class PinshCmd:

    def __init__(self, name, help_text = "<cr>",
                 level = ENABLE, token_delimeter = ' '):
        self.my_name = name
        self.help_text = help_text
        self.children = []
        self.level = level
        self.auth = USER
        self.cmd_owner = 0
        self.token_delimeter = token_delimeter
        self.names = []
        self.exit_mode = False
        self.log_command = False

    def __repr__(self):
        return self.my_name

    def preferred_names(self, tokens, index):
        return [self.my_name]

    def acceptable_names(self, tokens, index):
        return self.preferred_names(tokens, index)

    def match(self, tokens, index):
        if self.auth > system_state.auth:
            return NO_MATCH, 1
        if tokens[index] == '':
            return PARTIAL, 1
        if self.my_name == tokens[index]:
            return COMPLETE, 1
        if self.my_name.startswith(tokens[index]):
            return PARTIAL, 1
        else:
            return NO_MATCH, 1

    def cmd(self, tokens, noFlag, my_slash):
        if tokens or noFlag or my_slash: pass # pychecker
        if DEBUG: print "NAME:",self.my_name, self.cmd_owner
        return FAIL, ["Incomplete command."]

    def print_error_msg(self, tokens, index, message = "Unrecognized Command"):
        preamble = ' '*len(system_state.get_prompt())
        ok_tokens = convert_tokens_to_string(tokens[:index])
        print "%s%s" % (preamble, convert_tokens_to_string(tokens))
        print "%s%s ^" % (preamble, " "*len(ok_tokens))
        print " %% %s" % message

    # This is called by complete.
    # When complete is calling this it wants a list of objects
    # that could be completions for the final token.
    def find_completions(self, tokens, index):
        return_error = 1
        if DEBUG: print "find_completions: self.my_name:",`self.my_name`, "tokens:",`tokens`, len(tokens)
        if len(tokens[index:]) == 0: # no tokens left, I must be who you want!
            if DEBUG: print "find_completions: FOUND at TOP"
            return [self], index
        if tokens[index] == '':
            if len(self.children) > 0:
                output = [ child for child in self.children if child.auth <= system_state.auth ]
                return output, index+1
            return_error = 0
        completion_objects = []
        incomplete_objects = []
        match_len = 0
        if DEBUG: print "CHILDREN: ", self.children
        for child in self.children:
            if child.auth > system_state.auth:
                continue
            match_value, length = child.match(tokens, index)
            if match_value == INCOMPLETE:
                if DEBUG: print "find_completions INCOMPLETE : match_value:",match_value, "length:",length
                incomplete_objects.append(child)
            if match_value == PARTIAL:
                if DEBUG: print "find_completions PARTIAL : match_value:",match_value, "length:",length
                if length > match_len:
                    match_len = length
                completion_objects.append(child)
            elif match_value == COMPLETE: # go see if there are more tokens!
                if DEBUG: print "find_completions COMPLETE : match_value:",match_value, "length:",length
                tokens[index+length-1] = child.preferred_names(tokens, index)[0].split(' ')[-1]
                if DEBUG: print "NEW TOKEN:", tokens[index]
                return child.find_completions(tokens, index+length)
        if len(completion_objects) == 1: # one partial match is as good as a complete match
            if index+match_len >= len(tokens):
                return [completion_objects[0]], index+1
            return completion_objects[0].find_completions(tokens, index+match_len)
        elif len(completion_objects) == 0: # No matches: go away.
            if len(incomplete_objects) > 0:
                print
                self.print_error_msg(tokens, index, "Command cannot be completed.")
                system_state.reprompt()
                return [], 1
            if return_error:
                print
                self.print_error_msg(tokens, index)
                system_state.reprompt()
            return [], index
        else: # we have a few possible matches, return them all
            return completion_objects, index

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
                noFlag, helpFlag, tokens, comment = libUi.process_input(readline.get_line_buffer())
                if DEBUG: print "COMPLETE: ",`tokens`
                # this is where we would process help if we could bind the '?' key properly
                index = 0
                if tokens == []:
                    if DEBUG: print "No tokens, returning children",
                    completion_objects = self.children
                else:
                    if DEBUG: print "Finding completions on",`tokens`
                    completion_objects, index = self.find_completions(tokens, 0)
                if DEBUG: print "Found completions: ",completion_objects, index
                if len(completion_objects) == 0:
                    #system_state.reprompt()
                    return None
                # status is the index of the completion that readline wants
                self.names =  get_names(completion_objects, tokens, index-1)
                if DEBUG: print "COMPLETE: tokens:",`tokens`,"index:",index,"names:",`self.names`
                if DEBUG: print "COMPLETE: names",self.names
                if len(self.names) == 1:
                    return self.names[0] + completion_objects[0].token_delimeter
                if self.names:
                    return self.names[0]
                return []
        except StandardError, e:
            sys.stderr.write(" %%%% Error detected in %s (%s)." % (file, e))
            e = StringIO.StringIO()
            traceback.print_exc(file=e)
            e.seek(0)
            data = e.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            sys.stderr.write(ermsg)
            sys.stderr.write(" %%%% Error ocurred in %s" % file)
            print
            return []

    # When complete is calling this, it wants help for all the 
    # possible arguments of the last token, which should be unambiguous.
    # No return value is necessary
    def find_help(self, tokens, index):
        if DEBUG: print "find_help:", `self.my_name`, 'tokens:', `tokens`
        # no tokens left, I must be who you want!
        if len(tokens[index:]) == 0 or tokens[index] == '': 
            if DEBUG: print "my_name:",self.my_name
            self.help()
            return
        if DEBUG: print "finding completions..."
        match_len = 0
        completion_objects = []
        for child in self.children:
            match_value, length = child.match(tokens, index)
            if match_value == PARTIAL:
                if length > match_len:
                    match_len = length
                completion_objects.append(child)
            elif match_value == COMPLETE: # go see if there are more tokens!
                child.find_help(tokens, index+length)
                return
        if len(completion_objects) == 1: # one partial matches is as good as a complete match
            return completion_objects[0].find_help(tokens, index+match_len)
        elif len(completion_objects) == 0: # No matches: go away.
            self.print_error_msg(tokens, index)
            return
        else:
            self.print_error_msg(tokens, index, "Ambiguous command")
            return

    # Run is calling this method to find the last object in the chain
    # that is willing to run cmd() on the set of tokens on the list.
    # Must be unambiguous. Assume coming into the routine that the
    # current object ("self") is a cmd_owner.
    def find_last_responsible_child(self, tokens, index):
        if DEBUG: print "find_last_responsible_child: ",self.my_name, "[",`tokens`,"], index:", index
        if len(tokens[index:]) == 0 or tokens[index] == '': # complete and no more tokens. Object takes responsibility
            return self
        owners = []
        arguments = []
        match_len = 0
        for child in self.children:
            match_value, length = child.match(tokens, index)
            if DEBUG: print "Match:",child.my_name, match_value
            if match_value == PARTIAL:
                if length > match_len:
                    match_len = length
                if child.cmd_owner:
                    owners.append(child)
                else:
                    arguments.append(child)
            elif match_value == COMPLETE:
                tokens[index+length-1] = child.acceptable_names(tokens, index)[0]
                if DEBUG: print "NEW TOKEN:", tokens[index]
                if child.cmd_owner:
                    return child.find_last_responsible_child(tokens, index+length)
                else:
                    return self
        if len(owners) == 1: # one partial matches is as good as a complete match
            return owners[0].find_last_responsible_child(tokens, index+match_len)
        elif len(owners) == 0:
            if len(arguments) > 0: # this is a valid argument, I will take responsibility.
                return self
            self.print_error_msg(tokens, index)
            return None
        else: # more than one owner-- need to be unambiguous
            self.print_error_msg(tokens, index, "Ambiguous command")
            return None

    # finds the correct object and runs a command
    def run(self, tokens, noFlag, my_slash):
        if tokens[-1] == '':
            tokens = tokens[:-1]
        if system_state.state[-1] == F0:
            if tokens[0].lower() != 'end':
                system_state.command_buffer[F0].append([tokens, noFlag, my_slash])
            else:
                variable, values = system_state.variables[F0]
                for value in values:
                    for tokens, noFlag, my_slash in system_state.command_buffer[F0]:
                        newTokens = []
                        for token in tokens:
                            if token == '$%s' % variable:
                                newTokens.append(value)
                            else:
                                newTokens.append(token)
                        owner = self.find_last_responsible_child(newTokens, 0)
                        if not owner:
                            continue
                        return_value = owner.cmd(newTokens, noFlag, my_slash)
                        if not (return_value == None and len(return_value) != 2):
                            status = return_value[0]
                            output = return_value[1]
                            if owner.log_command:
                                cmd = log(noFlag, tokens, status, output)
                                system_state.comment_commands.append(cmd)
                            libUi.userOutput(output, status)
                        system_state.globals["output"] = output
                        system_state.globals["status"] = status
                extra_classes = system_state.new_classes[-1]
                for i in range(0,extra_classes):
                    if i: pass # pychecker
                    my_slash.children.pop()
                system_state.pop_prompt()
                system_state.clean_mode(system_state.state[-1])
            return OK, []
        else:
            owner = self.find_last_responsible_child(tokens, 0)
            if not owner:
                return FAIL, []
            if system_state.state[-1] == F0 and not owner.exit_mode:
                system_state.commands.append([owner.cmd, tokens, noFlag, my_slash])
                return OK, []
            else:
                return_value = owner.cmd(tokens, noFlag, my_slash)
                if return_value == None or len(return_value) != 2:
                    return OK, []
                else:
                    status = return_value[0]
                    output = return_value[1]
                    if owner.log_command:
                        cmd = log(noFlag, tokens, status, output)
                        system_state.comment_commands.append(cmd)
                    system_state.globals["output"] = output
                    system_state.globals["status"] = status
                    return status, output

    # pretty-print the help strings of all my children on this level
    def help(self):
        help_text = []
        max_len = 0
        if len(self.children) == 0:
            print "<cr>\n"
            return

        for child in self.children:
            if system_state.auth < child.auth:
                continue
            if child.level < system_state.state[-1]:
                continue
            if child.help_text.rfind('\n') != -1:
                for help_line in child.help_text.split('\n'):
                    cmd, doc = help_line.split('\t')
                    help_text.append([cmd, doc])
                    if len(cmd) > max_len:
                        max_len = len(cmd)
            else:
                try:
                    cmd, doc = child.help_text.split('\t')
                    help_text.append([cmd, doc])
                except:
                    pass
            if len(cmd) > max_len:
                max_len = len(cmd)

        help_text.sort()

        for help_line in help_text:
            cmd = help_line[0]
            text = help_line[1]
            spaces = max_len - len(cmd) + 5
            print "  ", cmd + ' ' * spaces + text
