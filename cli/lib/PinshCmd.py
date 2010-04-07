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

from bombardier_core.static_data import PARTIAL, COMPLETE, INCOMPLETE
from bombardier_core.static_data import NO_MATCH
from Exceptions import AmbiguousCommand, UnknownCommand
from SystemStateSingleton import SystemState, ENABLE
system_state = SystemState()

DEBUGGING = False

# find the names of all the objects given to me
def convert_tokens_to_string(tokens, delimeter=' '):
    '''Used in error reporting.
    delimeter -- typically a space, but some tokens are separated
                 using other characters'''
    retVal = ''
    for token in tokens:
        retVal += token+delimeter
    return retVal[:-1]

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
        self.cmd_owner = 0
        self.auth = auth
        self.token_delimeter = token_delimeter
        self.names = []
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
        '''Because commands can be shortened, it is possible that "sh" could
        be used in place of "show", for example. preferred_names(), on the
        other hand represents the name that best matches this object.
        '''
        return self.preferred_names(tokens, index)

    def match(self, tokens, index):
        '''Used to determine if what the user has typed in could potentially
        be handled by this object. For example, if a user typed in 'sh', and
        this object is called 'show', then this object would return PARTIAL. If
        the user typed in 'show', it would return COMPLETE, and if the user
        typed in "showme" this method would return NO_MATCH.'''
        if self.auth > system_state.auth:
            self.dbg("NOT Authorized")
            return NO_MATCH, 1
        self.dbg("tokens[index] = %s" % tokens[index])
        if tokens[index] == '':
            return PARTIAL, 1
        if self.my_name == tokens[index]:
            return COMPLETE, 1
        if self.my_name.startswith(tokens[index]):
            return PARTIAL, 1
        else:
            return NO_MATCH, 1

    @classmethod
    def print_error_msg(cls, tokens, index, message = "Unrecognized Command"):
        'Prints a customized error message, showing the position of the error'
        preamble = ' '*len(system_state.get_prompt())
        ok_tokens = convert_tokens_to_string(tokens[:index])
        print "%s%s" % (preamble, convert_tokens_to_string(tokens))
        print "%s%s ^" % (preamble, " "*len(ok_tokens))
        print " %% %s" % message

    def find_possible_completions(self, tokens, index):
        '''Look through all the children of this object, and based
        on the tokens/index, find one or more children that could
        possibly match what the user is looking for'''

        completion_objects = []
        incomplete_objects = []
        match_len = 0
        for child in self.children:
            if child.auth > system_state.auth:
                continue
            match_value, length = child.match(tokens, index)
            d_tmp = "find_completions match_value: %s length: %d"
            if match_value == INCOMPLETE:
                self.dbg(d_tmp % (match_value, length))
                incomplete_objects.append(child)
            if match_value == PARTIAL:
                self.dbg(d_tmp % (match_value, length))
                if length > match_len:
                    match_len = length
                completion_objects.append(child)
            elif match_value == COMPLETE: # go see if there are more tokens!
                self.dbg(d_tmp % (match_value, length))
                new_name = child.preferred_names(tokens, index)[0]
                tokens[index+length-1] = new_name.split(' ')[-1]
                self.dbg("NEW TOKEN: %s" % tokens[index])
                completion_objects = [child]
                match_len = length
                break
        return completion_objects, incomplete_objects, match_len

    def find_completions(self, tokens, index):
        '''This is called by complete. When complete is calling this it wants
        a list of objects that could be completions for the final token.'''
        return_error = 1
        d_tmp = "find_completions: self.my_name: %s tokens: %s (%d)"
        self.dbg(d_tmp % (self.my_name, tokens, len(tokens)))
        if len(tokens[index:]) == 0: # no tokens left, I must be who you want!
            self.dbg("find_completions: FOUND at TOP")
            return [self], index
        if tokens[index] == '':
            if len(self.children) > 0:
                output = []
                for child in self.children:
                    if child.auth <= system_state.auth:
                        output.append(child)
                return output, index+1
            return_error = 0

        completion_objects, incomplete_objects, match_len = \
            self.find_possible_completions(tokens, index)

        if len(completion_objects) == 1:
            new_child = completion_objects[0]
            if index+match_len >= len(tokens):
                return [new_child], index+1
            return new_child.find_completions(tokens, index+match_len)
        elif len(completion_objects) == 0: # No matches: go away.
            if len(incomplete_objects) > 0:
                print
                msg = "Command cannot be completed."
                self.print_error_msg(tokens, index, msg)
                system_state.reprompt()
                return [], 1
            if return_error:
                print
                self.print_error_msg(tokens, index)
                system_state.reprompt()
            return [], index
        else: # we have a few possible matches, return them all
            return completion_objects, index

    @classmethod
    def dbg(cls, msg):
        'Little debugger'
        if DEBUGGING:
            print "******", msg

    def find_help(self, tokens, index):
        '''When complete is calling this, it wants help for all the
        possible arguments of the last token, which should be unambiguous.
        No return value is necessary'''
        self.dbg("find_help: %s tokens: %s" % (self.my_name, tokens))
        # no tokens left, I must be who you want!
        if len(tokens[index:]) == 0 or tokens[index] == '':
            self.dbg("my_name: %s" % self.my_name)
            self.help()
            return
        match_len = 0
        completion_objects = []
        for child in self.children:
            match_value, length = child.match(tokens, index)
            self.dbg("child: %s / %s / %s" % (child, match_value, length))
            if match_value == PARTIAL:
                if length > match_len:
                    match_len = length
                completion_objects.append(child)
            elif match_value == COMPLETE:
                completion_objects = [child]
                match_len = length
                break
        if len(completion_objects) == 1:
            return completion_objects[0].find_help(tokens, index + match_len)
        elif len(completion_objects) == 0:
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
        d_tmp = "find_last_responsible_child: %s %s index: %d"
        self.dbg( d_tmp % (self.my_name, tokens, index) )
        if len(tokens[index:]) == 0 or tokens[index] == '':
            # complete and no more tokens. Object takes responsibility
            return self
        owners = []
        arguments = []
        match_len = 0
        for child in self.children:
            match_value, length = child.match(tokens, index)
            self.dbg("Match: %s / %s " % ( child.my_name, match_value ))
            if match_value == PARTIAL:
                if length > match_len:
                    match_len = length
                if child.cmd_owner:
                    owners.append(child)
                else:
                    arguments.append(child)
            elif match_value == COMPLETE:
                new_name = child.acceptable_names(tokens, index)[0]
                tokens[index+length-1] = new_name
                self.dbg("NEW TOKEN: %s" % tokens[index])
                if child.cmd_owner:
                    responsible = child.find_last_responsible_child(tokens,
                                                             index + length)
                    self.dbg("%s IS RESPONSIBLE" % responsible)
                    return responsible
                else:
                    responsible = child.find_last_responsible_child(tokens,
                                                             index + length)
                    if not responsible:
                        return None
                    if responsible.cmd_owner:
                        self.dbg( "%s IS RESPONSIBLE" % responsible)
                        return responsible
                    self.dbg( "%s IS RESPONSIBLE" % self)
                    return self
        if len(owners) == 1:
            owner = owners[0]
            completed_name = owner.acceptable_names(tokens, index)[0]
            tokens[index+length-1] = completed_name
            self.dbg("NEW TOKEN: %s" % tokens[index])
            return owner.find_last_responsible_child(tokens, index+match_len)
        elif len(owners) == 0:
            if len(arguments) > 0:
                return self
            self.print_error_msg(tokens, index)
            return None
        else: # more than one owner-- need to be unambiguous
            self.print_error_msg(tokens, index, "Ambiguous command")
            return None

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
                for line in child.help_text.split('\n'):
                    if '\t' in line:
                        cmd, doc = line.split('\t')
                    else:
                        cmd = child.my_name
                        doc = line
                    help_text.append([cmd, doc])
                    if len(cmd) > max_len:
                        max_len = len(cmd)
            else:
                if '\t' in child.help_text:
                    cmd, doc = child.help_text.split('\t')
                else:
                    cmd = child.my_name
                    doc = child.help_text
                help_text.append([cmd, doc])
            if len(cmd) > max_len:
                max_len = len(cmd)

        help_text.sort()

        for help_line in help_text:
            cmd = help_line[0]
            text = help_line[1]
            spaces = max_len - len(cmd) + 5
            print "  ", cmd + ' ' * spaces + text
