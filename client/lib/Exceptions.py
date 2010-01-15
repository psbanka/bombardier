#!/usr/bin/python
" Exceptions that might be encountered within the Bombardier Client"
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

class InvalidConfigData(Exception):
    "Thrown if we are given bad configuration info."
    def __init__(self, name):
        Exception.__init__(self)
        self.name = name
    def __repr__(self):
        return "Package %s requires a reboot. Stopping..." % self.name
    def __str__(self):
        return self.__repr__()

class RebootRequiredException(Exception):
    "Thrown after a package is installed that notes a reboot is required."
    def __init__(self, name):
        Exception.__init__(self)
        self.name = name
    def __repr__(self):
        return "Package %s requires a reboot. Stopping..." % self.name
    def __str__(self):
        return self.__repr__()

class FeatureRemovedException(Exception):
    "Thrown when attempting to run an invalid action"
    def __init__(self, feature_name):
        Exception.__init__(self)
        self.feature_name  = feature_name
    def __repr__(self):
        return "Feature %s is obsolete" % (self.feature_name)
    def __str__(self):
        return self.__repr__()

class AbstractClassException(Exception):
    "Thrown when the Spkg superclass hasn't been overridden"
    def __init__(self, method_name):
        Exception.__init__(self)
        self.class_name  = method_name
    def __repr__(self):
        msg = "Method %s was attempted in an abstract class"
        return msg % (self.class_name)
    def __str__(self):
        return self.__repr__()

class DependencyLoopException(Exception):
    "Thrown when a package A depends on package B which depends on package A..."
    def __init__(self, chain):
        Exception.__init__(self)
        self.chain  = chain
    def __repr__(self):
        output  = "Package chain has a recursive loop in dependencies: "
        for pkn in self.chain:
            output += pkn + ','
        return output[:-1]
    def __str__(self):
        return self.__repr__()

class BadPackage(Exception):
    "Thrown when a package is not set up correctly."
    def __init__(self, pkn, errmsg):
        Exception.__init__(self)
        self.errmsg      = errmsg
        self.pkn = pkn
    def __repr__(self):
        return "%s: %s" % (self.pkn, self.errmsg)
    def __str__(self):
        return self.__repr__()
