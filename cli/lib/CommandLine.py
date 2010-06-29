#!/usr/bin/env python

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

'''Contains the CommandLine class'''
from SystemStateSingleton import SystemState
system_state = SystemState()

def convert_tokens_to_string(tokens, delimeter=' '):
    '''Used in error reporting.
    delimeter -- typically a space, but some tokens are separated
                 using other characters'''
    ret_val = ''
    for token in tokens:
        ret_val += token + delimeter
    return ret_val[:-1]

def append_not_blank(current_token, tokens):
    '''append something to the list if it isn't blank. Handles users typing
    in more than one space between tokens'''
    current_token = current_token.strip()
    if current_token:
        tokens.append(current_token)
    return tokens

def tokenize(input_str):
    '''Takes an input string and divides it into string tokens. Handles
    the word "no" in front of the string specially, handles a '?' specially
    handles quoted text as one unit, and handles comment markers'''
    tokens = []
    quote_mode = False
    current_token = ''
    append_last = False
    comment_string = ''
    for i in range(0, len(input_str)):
        char = input_str[i]
        if not quote_mode:
            if char not in ['"', '#', ' ']:
                current_token += char
                append_last = True
                continue
            tokens = append_not_blank(current_token, tokens)
            if char == '"': # start quote
                quote_mode = True
            elif char == '#': # discard the rest, it's a real comment
                if i != 0 and input_str[i-1] == ' ':
                    append_last = True
                else:
                    append_last = False
                comment_string = input_str[i+1:]
                break
            elif char == ' ': # tokenize on spaces if not quoted
                current_token = ''
                append_last = True
        else:
            if char == '"': # end quote
                tokens = append_not_blank(current_token, tokens)
                current_token = ''
                append_last = False
                quote_mode = False
            else:
                current_token += char
    if quote_mode: # unbalanced quotes
        raise Exception
    if append_last:
        tokens.append(current_token.lstrip()) # grab the last
    return tokens, comment_string

def process_tokens(tokens):
    """Go through a bunch of command tokens and replace variables
    and evaluate list notation
    """
    processed_data = []
    for token in tokens:
        if token != '':
            processed = False
            if token.startswith('$'):
                var_name = token[1:]
                if system_state.globals.has_key(var_name):
                    value = system_state.globals.get(var_name)
                    if type(value) == type(["list"]):
                        processed_data.append('[')
                        processed_data += value
                        processed_data.append(']')
                    else:
                        token = str(value)
                    processed = True
            if token.startswith('[') and len(token) > 1:
                processed_data.append('[')
                token = token[1:]
            if token.endswith(']') and len(token) > 1:
                processed_data.append(token[:-1])
                processed_data.append(']')
                processed = True
            if not processed:
                processed_data.append(token)
    return processed_data

class CommandLine:
    """Handles data that was entered on the command line"""

    def __init__(self):
        """
        no_flag -- If the user typed "no" at the beginning of the line
                   to turn something off
        help_flag -- if the user typed a '?' character to get help
        bg_flag -- if the user ended the command with a '&' character
                   to start a job in the background.
        tokens -- a list of strings which represent to tokenized command line
        command_string -- If the user added a '#' comment at the end of
                          the line 
        current_token -- used in iterating through the tokens
        """
        self.no_flag = 0
        self.help_flag = 0
        self.bg_flag = 0
        self.tokens = []
        self.comment_string = ''
        self.current_token = 0

    def __iter__(self):
        "Iterator interface"
        self.current_token = 0
        return self

    def next(self):
        "Iterator interface"
        if self.current_token >= self.get_length():
            raise StopIteration
        self.current_token += 1
        return self.tokens[self.current_token - 1]

    def __len__(self):
        "List interface"
        return self.get_length()

    def __setitem__(self, key, item):
        "List interface"
        self.tokens[key] = item

    def __getitem__(self, item):
        "List interface"
        return self.index(item)

    def index(self, item):
        "List interface"
        return self.tokens[item]

    def append(self, token):
        "List interface"
        self.tokens.append(token)

    def get_length(self):
        "List interface"
        return len(self.tokens)

    def get_token(self, index):
        "List interface"
        return self.tokens[index]

    def is_empty(self):
        "Determine if this command-line has nothing in it"
        if len(self.tokens) == 0:
            return True
        return False

    def process_input(self, input_string):
        '''take a line of input from the user and convert it into an argv
        type structure
        return no_flag, help_flag, bg_flag, tokens, comment_string
        '''

        if len(input_string) == 0:
            return
        # determine if the help_flag is there (cheap hack)
        strlen = len(input_string)
        if strlen >= 1 and input_string[-1] == '?':
            self.help_flag = 1
            input_string = input_string[:-1]
        elif strlen >= 1 and input_string[-1] == '&':
            self.bg_flag = 1
            input_string = input_string[:-1]
        if len(input_string) == 0:
            return
        input_string.lstrip() # left space is unimportant
        tokens, self.comment_string = tokenize(input_string)
        # handle a preceding 'no'
        if not tokens:
            return
        if tokens[0] == 'no':
            self.no_flag = 1
            if len(tokens) > 1:
                tokens = tokens[1:]
            else:
                return
        processed_data = process_tokens(tokens)
        if tokens[-1] != '':
            tokens = processed_data
        else:
            tokens = processed_data + ['']
        if tokens[0] == "ls" and len(tokens) == 1:
            tokens[0] = ''
            self.help_flag = True
        self.tokens = tokens
        return

    def clean_last_token(self):
        "If the last token is empty, pull it off"
        if self.tokens[-1] == '':
            self.tokens = self.tokens[:-1]

    def update_system_tokens(self):
        "Prepend our tokens from sub-configuration"
        self.tokens = system_state.get_state_tokens(self.tokens)

    def update_token(self, new_name, index):
        "Replace the name of a token with a new one"
        self.tokens[index] = new_name

    def error_msg(self, index, message = "Unrecognized Command"):
        'Prints a customized error message, showing the position of the error'
        ret_val = []
        preamble = ' ' * len(system_state.get_prompt())
        ok_tokens = convert_tokens_to_string(self.tokens[:index])
        ret_val = ["%s%s" % (preamble, convert_tokens_to_string(self.tokens)),
                   "%s%s ^" % (preamble, " " * len(ok_tokens)),
                   " %% %s" % message,
                  ]
        return ret_val

    def __str__(self):
        "String interface"
        return "[ %s ]" % ",".join( self.tokens )

    def __repr__(self):
        "String interface"
        return self.__str__()
        

def process_input(input_string):
    "Create a command-line object based on an input string"
    command_line = CommandLine()
    command_line.process_input(input_string)
    return command_line
