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

'Classes that hold command-line interface-specific exceptions'

class ExceptionBase(Exception):
    def __init__(self):
        Exception.__init__(self)
    def __str__(self):
        return self.__repr__()

class CommandError(Exception):
    '''If any command is improper, this can be thrown instead of a fail'''
    def __init__(self, error_string):
        Exception.__init__(self)
        self.error_string = error_string
    def __repr__(self):
        return [self.error_string]
    def __list__(self):
        return self.__repr__()
    def __str__(self):
        return '\n'.join(self.__repr__())

class HostNotEnabledException(Exception):
    '''It is necessary to enable a host before trying to work with it.
    if a user attempts to do something with a host that is not enabled,
    this exception throws.'''
    def __init__(self, server):
        Exception.__init__(self)
        self.server = server
    def __repr__(self):
        return "Host %s is not enabled for this user" % self.server

class ConfigFileException(Exception):
    '''Your config file (~/.bomsh_config) stores critical setup info.
    If there is a problem reading or writing to it, this throws.'''
    def __init__(self, message, fileName):
        Exception.__init__(self)
        self.fileName = fileName
        self.message = message
    def __repr__(self):
        return "%% Error processing config file %s: %s" \
               % (self.fileName, self.message)
    def __str__(self):
        return self.__repr__()

class ServerTracebackException(ExceptionBase):
    def __init__(self, traceback_lines):
        ExceptionBase.__init__(self)
        self.traceback = traceback_lines
    def __repr__(self):
        return "Server Exception"

class MachineUnavailableException(ExceptionBase):
    def __init__(self, msg):
        self.msg = msg
    def __repr__(self):
        return "Machine is not available: %s" % self.msg

class MachineStatusException(ExceptionBase):
    def __init__(self, msg):
        self.msg = msg
    def __repr__(self):
        return "Error in status data: %s" % self.msg

class ServerException(Exception):
    '''It is necessary to communicate with the RESTful Bombardier web server
    to make the CLI work. If there is an issue with a command to that server
    then this throws.'''
    def __init__(self, url, curl_err, http_code):
        Exception.__init__(self)
        self.url = url
        self.curl_err = curl_err
        self.http_code = http_code
    def __repr__(self):
        return "Can't connect to %s (%s) (%d)" % (self.url, self.curl_err, self.http_code)
    def __str__(self):
        return self.__repr__()

class UnexpectedDataException(Exception):
    '''Whenever we are receiving information from a server that we're not
    expecting.'''
    def __init__(self, reason):
        Exception.__init__(self)
        self.reason = reason
    def __repr__(self):
        return "Got unexpected data from the server (%s)" % self.reason
    def __str__(self):
        return self.__repr__()

class UnknownCommand(Exception):
    "Someone entered a command that doesn't make sense"
    def __init__(self, command):
        Exception.__init__(self)
        self.command = command
    def __repr__(self):
        return "Unknown command (%s)" % self.command
    def __str__(self):
        return self.__repr__()

class AmbiguousCommand(Exception):
    "Someone entered a command that could have matched more than one option"
    def __init__(self, command, options):
        Exception.__init__(self)
        self.command = command
        self.options = options
    def __repr__(self):
        return 'Command "%s" ambiguous -- could be %s.' % (self.command, self.options)
    def __str__(self):
        return self.__repr__()

class MachineTraceback(Exception):
    "When trying to execute a command on a machine, it raised."
    def __init__(self, url, traceback):
        Exception.__init__(self)
        self.url = url
        self.traceback = traceback
    def __repr__(self):
        return 'Raised an exception in server url: %s' % (self.url)
    def __str__(self):
        return self.__repr__()

