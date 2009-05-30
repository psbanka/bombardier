#!/usr/bin/python
# BSD License
# Copyright (c) 2009, Peter Banka et al
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the name of the GE Security nor the names of its contributors may
#   be used to endorse or promote products derived from this software without
#   specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

'''Provides the base class for all command-line token object fields
or commands'''

import readline, sys
import libUi
import StringIO
import traceback
from bombardier_core.static_data import DEBUG, PARTIAL, COMPLETE, INCOMPLETE
from bombardier_core.static_data import NO_MATCH, FAIL, OK
from Exceptions import AmbiguousCommand, UnknownCommand
from SystemStateSingleton import SystemState, ENABLE, USER, F0
system_state = SystemState()

#DEBUG = True

# find the names of all the objects given to me
def convert_tokens_to_string(tokens, delimeter=' '):
    '''Used in error reporting.
    delimeter -- typically a space, but some tokens are separated
                 using other characters'''
    retVal = ''
    for token in tokens:
        retVal += token+delimeter
    return retVal[:-1]

def get_names(objects, tokens, index):
    """
    There is a tree of objects, such as:
     slash:
        - Show:
            - machine
                - <machine_name>
            - package
                - <package_name>
        - Set
            - machine
                - <machine_name>
            - package
                - <package_name>
        - Terminal
            - width
            - color
        - etc.
    ...and there is a set of user input, such as ['s']
    It is the job of this function to return a list of name objects that can
    be used to provide command-line completion.
    objects -- a list of PinshCmd objects
    tokens -- a list of string literals that represent strings the user typed in
    index -- the current position of evaluation
    """
    names = []
    for obj in objects:
        if DEBUG:
            print "(GET NAMES) obj.my_name:", obj.my_name
        new_name = obj.preferred_names(tokens, index)
        if type(new_name) == type("string"):
            names.append(new_name)
        else:
            names = names + new_name
    return names

class PinshCmd:
    '''Base class for all command and field objects. Really should be
    called CommandToken, because it represents a single executable
    or nameable token in an input string.'''

    def __init__(self, name, help_text = "<cr>",
                 auth = ENABLE, token_delimeter = ' '):
        '''
        name -- name of the command or field
        help_text -- explanatory information about this command
        auth -- authorization required to run this command
        token_delimeter -- character that separates this command or field
                           from other ones.
        '''
        self.my_name = name
        self.help_text = help_text
        self.children = []
        self.auth = auth
        self.cmd_owner = 0
        self.token_delimeter = token_delimeter
        self.names = []
        self.exit_mode = False
        self.log_command = False

    def __repr__(self):
        return self.my_name

    def find_one_child(self, tokens, index):
        '''Sometimes it's handy to be able to locate one of your
        children, based on a token name and index.'''
        possible_choices = []
        child_names = [child.my_name for child in self.children]
        for name in child_names:
            if name.find(tokens[index]) == 0:
                possible_choices.append(name)

        if not possible_choices:
            raise UnknownCommand(tokens[index])
        if len(possible_choices) > 1:
            if tokens[index] in child_names:
                possible_choices = [tokens[index]]
            else:
                raise AmbiguousCommand(tokens[index], possible_choices)
        return possible_choices[0]

    def preferred_names(self, _tokens, _index):
        '''How is this command or field represented in the shell? Often
        it is represented as a fixed text item, such as 'show' or 'set',
        but often it is a name of a host, such as 'localhost' or 'server10'.
        If a class wants to provide a customized name, it must override this
        method.'''
        return [self.my_name]

    def acceptable_names(self, tokens, index):
        '''Because commands can be shortened, it is possible that "sh" could be
        used in place of "show", for example. preferred_names(), on the other hand
        represents the name that best matches this object.
        '''
        return self.preferred_names(tokens, index)

    def match(self, tokens, index):
        '''Used to determine if what the user has typed in could potentially
        be handled by this object. For example, if a user typed in 'sh', and
        this object is called 'show', then this object would return PARTIAL. If
        the user typed in 'show', it would return COMPLETE, and if the user
        typed in "showme" this method would return NO_MATCH.'''
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

    def cmd(self, tokens, _no_flag):
        '''If the user has hit return and wants a command to be executed, and
        if the system has determined that this object is designated by the user
        then this object's cmd() method will be run.'''
        if DEBUG:
            print "NAME:", self.my_name, self.cmd_owner, tokens
        return FAIL, ["Incomplete command."]

    def print_error_msg(self, tokens, index, message = "Unrecognized Command"):
        'Prints a customized error message, showing the position of the error'
        preamble = ' '*len(system_state.get_prompt())
        ok_tokens = convert_tokens_to_string(tokens[:index])
        print "%s%s" % (preamble, convert_tokens_to_string(tokens))
        print "%s%s ^" % (preamble, " "*len(ok_tokens))
        print " %% %s" % message

    def find_completions(self, tokens, index):
        '''This is called by complete. When complete is calling this it wants
        a list of objects that could be completions for the final token.'''
        return_error = 1
        if DEBUG:
            d_tmp = "find_completions: self.my_name: %s tokens: %s (%d)"
            print d_tmp % (self.my_name, tokens, len(tokens))
        if len(tokens[index:]) == 0: # no tokens left, I must be who you want!
            if DEBUG:
                print "find_completions: FOUND at TOP"
            return [self], index
        if tokens[index] == '':
            if len(self.children) > 0:
                output = [ child for child in self.children if child.auth <= system_state.auth ]
                return output, index+1
            return_error = 0
        completion_objects = []
        incomplete_objects = []
        match_len = 0
        if DEBUG:
            print "CHILDREN: ", self.children
        for child in self.children:
            if child.auth > system_state.auth:
                continue
            match_value, length = child.match(tokens, index)
            d_tmp = "find_completions match_value: %s length: %d"
            if match_value == INCOMPLETE:
                if DEBUG:
                    print d_tmp % (match_value, length)
                incomplete_objects.append(child)
            if match_value == PARTIAL:
                if DEBUG:
                    print d_tmp % (match_value, length)
                if length > match_len:
                    match_len = length
                completion_objects.append(child)
            elif match_value == COMPLETE: # go see if there are more tokens!
                if DEBUG:
                    print d_tmp % (match_value, length)
                tokens[index+length-1] = child.preferred_names(tokens, index)[0].split(' ')[-1]
                if DEBUG:
                    print "NEW TOKEN:", tokens[index]
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

    def complete(self, _text, status):
        "Command line completer, called with [tab] or [?] (if we could bind it)"
        try:
            if status > 0:
                if status >= len(self.names):
                    return None
                if DEBUG:
                    print "COMPLETE: names (%s)" % (self.names[status])
                return self.names[status]
            else:
                _no_flag, _help_flag, tokens, _comment = \
                    libUi.process_input(readline.get_line_buffer())
                if DEBUG:
                    print "COMPLETE: %s" % tokens
                # this is where we would process help if
                # we could bind the '?' key properly
                index = 0
                if tokens == []:
                    if DEBUG:
                        print "No tokens, returning children",
                    completion_objects = self.children
                else:
                    if DEBUG:
                        print "Finding completions on %s" % tokens
                    completion_objects, index = self.find_completions(tokens, 0)
                if DEBUG:
                    print "Found completions: ", completion_objects, index
                if len(completion_objects) == 0:
                    #system_state.reprompt()
                    return None
                # status is the index of the completion that readline wants
                self.names =  get_names(completion_objects, tokens, index-1)
                if DEBUG:
                    d_tmp = "COMPLETE: tokens: %s index: %d names: %s"
                    print d_tmp % (tokens, index, self.names)
                if DEBUG:
                    print "COMPLETE: names", self.names
                if len(self.names) == 1:
                    return self.names[0] + completion_objects[0].token_delimeter
                if self.names:
                    return self.names[0]
                return []
        except StandardError, err:
            sys.stderr.write(" %%%% Error detected in %s (%s)." % (file, err))
            tb_str = StringIO.StringIO()
            traceback.print_exc(file=tb_str)
            tb_str.seek(0)
            data = tb_str.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            sys.stderr.write(ermsg)
            sys.stderr.write(" %%%% Error ocurred in %s" % file)
            print
            return []

    def find_help(self, tokens, index):
        '''When complete is calling this, it wants help for all the
        possible arguments of the last token, which should be unambiguous.
        No return value is necessary'''
        if DEBUG:
            print "find_help: %s tokens: %s" % (self.my_name, tokens)
        # no tokens left, I must be who you want!
        if len(tokens[index:]) == 0 or tokens[index] == '':
            if DEBUG:
                print "my_name:", self.my_name
            self.help()
            return
        if DEBUG:
            print "finding completions..."
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

    def find_last_responsible_child(self, tokens, index):
        '''Run is calling this method to find the last object in the chain
        that is willing to run cmd() on the set of tokens on the list.
        Must be unambiguous. Assume coming into the routine that the
        current object ("self") is a cmd_owner.'''
        if DEBUG:
            print "find_last_responsible_child: %s %s index: %d" % (self.my_name, tokens, index)
        if len(tokens[index:]) == 0 or tokens[index] == '':
            # complete and no more tokens. Object takes responsibility
            return self
        owners = []
        arguments = []
        match_len = 0
        for child in self.children:
            match_value, length = child.match(tokens, index)
            if DEBUG:
                print "Match:", child.my_name, match_value
            if match_value == PARTIAL:
                if length > match_len:
                    match_len = length
                if child.cmd_owner:
                    owners.append(child)
                else:
                    arguments.append(child)
            elif match_value == COMPLETE:
                tokens[index+length-1] = child.acceptable_names(tokens, index)[0]
                if DEBUG:
                    print "NEW TOKEN:", tokens[index]
                if child.cmd_owner:
                    return child.find_last_responsible_child(tokens, index+length)
                else:
                    return self
        if len(owners) == 1: # one partial matches is as good as a complete match
            tokens[index+length-1] = owners[0].acceptable_names(tokens, index)[0]
            if DEBUG:
                print "NEW TOKEN:", tokens[index]
            return owners[0].find_last_responsible_child(tokens, index+match_len)
        elif len(owners) == 0:
            if len(arguments) > 0: # this is a valid argument, I will take responsibility.
                return self
            self.print_error_msg(tokens, index)
            return None
        else: # more than one owner-- need to be unambiguous
            self.print_error_msg(tokens, index, "Ambiguous command")
            return None

    def run(self, tokens, no_flag):
        'finds the correct object and runs a command'
        if tokens[-1] == '':
            tokens = tokens[:-1]
        owner = self.find_last_responsible_child(tokens, 0)
        if not owner:
            return FAIL, []
        try:
            return_value = owner.cmd(tokens, no_flag)
        except AmbiguousCommand, amb_err:
            return FAIL, [str(amb_err)]
        except UnknownCommand, unk_err:
            return FAIL, [str(unk_err)]
        if return_value == None or len(return_value) != 2:
            return OK, []
        else:
            status = return_value[0]
            output = return_value[1]
            #if owner.log_command:
                #cmd = log(no_flag, tokens, status, output)
                #system_state.comment_commands.append(cmd)
            system_state.globals["output"] = output
            system_state.globals["status"] = status
            return status, output

    def help(self):
        'pretty-print the help strings of all my children'
        help_text = []
        max_len = 0
        if len(self.children) == 0:
            print "<cr>\n"
            return

        for child in self.children:
            if system_state.auth < child.auth:
                continue
            if child.help_text.rfind('\n') != -1:
                for help_line in child.help_text.split('\n'):
                    cmd, doc = help_line.split('\t')
                    help_text.append([cmd, doc])
                    if len(cmd) > max_len:
                        max_len = len(cmd)
            else:
                cmd, doc = child.help_text.split('\t')
                help_text.append([cmd, doc])
            if len(cmd) > max_len:
                max_len = len(cmd)

        help_text.sort()

        for help_line in help_text:
            cmd = help_line[0]
            text = help_line[1]
            spaces = max_len - len(cmd) + 5
            print "  ", cmd + ' ' * spaces + text
