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

'''A special [PinshCmd] command which is the root of all other commands. Slash
Does not do anything itself, but merely has other PinshCmd objects which are
its children'''

import traceback, StringIO
import exceptions, readline
import sys
import PinshCmd, libUi
from Exceptions import AmbiguousCommand, UnknownCommand, ServerException
from Exceptions import CommandError, ServerTracebackException
from Exceptions import MachineStatusException
from SystemStateSingleton import SystemState
system_state = SystemState()
from bombardier_core.static_data import FAIL, OK

class Slash(PinshCmd.PinshCmd):
    '''Root-level PinshCmd object, has no name.'''
    def __init__(self, children):
        PinshCmd.PinshCmd.__init__(self, "")
        self.children = children
        self.cmd_owner = 1
        self.help_text = ''

    def complete_tokens(self, tokens):
        completed_tokens = []
        current_tokens = []
        for token in tokens:
            current_tokens.append(token)
            names, token_delimeter = self.get_names_and_token_delimeter(current_tokens)
            if not names:
                raise UnknownCommand(' '.join(current_tokens))
            if token in names:
                completed_tokens.append(token)
            else:
                if len(names) > 1:
                    raise AmbiguousCommand(current_tokens[-1], names)
                completed_tokens.append(names[0])
        return completed_tokens

    def run(self, tokens, no_flag):
        'finds the correct object and runs a command'
        if tokens[-1] == '':
            tokens = tokens[:-1]
        owner = self.find_last_responsible_child(tokens, 0)
        if not owner:
            return FAIL, []
        try:
            tokens = self.complete_tokens(tokens)
            return_value = owner.cmd(tokens, no_flag)
        except AmbiguousCommand, amb_err:
            return FAIL, [str(amb_err)]
        except UnknownCommand, unk_err:
            return FAIL, [' ', str(unk_err)]
        except ServerTracebackException, trc:
            for line in trc.traceback:
                print "%%%% (%s)" % line.strip()
            return FAIL, ['Server traceback']
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

    def process_command(self, command):
        'When somebody hits return in the shell, this method handles it.'
        if command == "exit":
            raise EOFError
        try:
            no_flag, help_flag, tokens, comment = libUi.process_input(command)
            if help_flag: # Process the [?] key first
                self.find_help(tokens, 0)
                return None
            if len(tokens) == 0:
                # somebody just pressed return for no reason
                return OK, []
            tokens = system_state.get_state_tokens(tokens)
            status, output = self.run(tokens, no_flag)
            libUi.user_output(output, status)
            return status, output
        except ServerException, err:
            if err.http_code == 403:
                msg = ["You are not authorized for this command."]
                libUi.user_output(msg, FAIL)
                return FAIL, msg
            output = ["ERROR: Cannot communicate with CNM Server"]
            output.append(str(err))
            libUi.user_output(output, FAIL)
        except MachineStatusException, err:
            output = ["ERROR: Machine status is incomplete"]
            output.append(str(err))
            libUi.user_output(output, FAIL)
        except exceptions.SystemExit:
            #if system_state.comment_commands:
                #makeComment()
            sys.exit(0)
        except CommandError, err:
            system_state.fp_out.write("\n %% %s\n\n" % err )
        except ServerTracebackException, err:
            msg = " %% There was a problem on the server:"
            system_state.fp_err.write(msg)
            for line in err.traceback:
                system_state.fp_err.write("  %%%% %s" % line)
                system_state.fp_err.write("\n")
        except Exception, err:
            msg = " %%%% Error detected in %s (%s)." % (command, err)
            system_state.fp_err.write( msg )
            tb_str = StringIO.StringIO()
            traceback.print_exc(file=tb_str)
            tb_str.seek(0)
            data = tb_str.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            system_state.fp_err.write(ermsg)
            system_state.fp_err.write("\n")
        return FAIL, ["process_command Excepted"]

    @classmethod
    def get_names(cls, objects, tokens, index):
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
        tokens -- a list of string literals that represent strings the user
                  typed in
        index -- the current position of evaluation
        """
        names = []
        for obj in objects:
            new_name = obj.preferred_names(tokens, index)
            if type(new_name) == type("string"):
                names.append(new_name)
            else:
                names = names + new_name
        return names

    def get_names_and_token_delimeter(self, tokens):
        index = 0
        if tokens == []:
            completion_objects = self.children
        else:
            completion_objects, index = self.find_completions(tokens, 0)
        if len(completion_objects) == 0:
            return None, ' '
        token_delimeter = completion_objects[0].token_delimeter
        names = self.get_names(completion_objects, tokens, index-1)
        return names, token_delimeter

    def complete(self, _text, status):
        '''Command line completer, called with [tab]
        or [?] (if we could bind it)
        _text -- text that readline sends as what the user enters
        status -- the index into the completion list'''
        try:
            if status > 0:
                if status >= len(self.names):
                    return None
                return self.names[status]
            else:
                _no_flag, _help_flag, tokens, _comment = \
                    libUi.process_input(readline.get_line_buffer())
                tokens = system_state.get_state_tokens(tokens)
                # this is where we would process help if
                # we could bind the '?' key properly
                names, token_delimeter = self.get_names_and_token_delimeter(tokens)
                self.names = names
                if not self.names:
                    return []
                if len(self.names) == 1:
                    return self.names[0] + token_delimeter
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

