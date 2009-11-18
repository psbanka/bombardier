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

class RebootRequiredException(Exception):
    def __init__(self, name):
        e = Exception()
        Exception.__init__(e)
        self.name=name
    def __repr__(self):
        return "RebootRequiredException: Package %s requires a reboot, stopping..." %self.name
    def __str__(self):
        return self.__repr__()

class ConsoleException(Exception):
    def __init__(self, msg):
        e = Exception()
        Exception.__init__(e)
        self.msg=msg
    def __repr__(self):
        return "ConsoleException: %s" % self.msg
    def __str__(self):
        return self.__repr__()

class InvalidConfigData(Exception):
    def __init__(self, section, t1, t2):
        e = Exception()
        Exception.__init__(e)
        self.section = section
        self.t1      = t1
        self.t2      = t2
    def __repr__(self):
        return "Unable to read configuration data: %s. [expected %s, got %s]" % (self.section, self.t1, self.t2)
    def __str__(self):
        return "Unable to read configuration data: %s. [expected %s, got %s]" % (self.section, self.t1, self.t2)

class FeatureRemovedException(Exception):
    def __init__(self, featureName):
        e = Exception()
        Exception.__init__(e)
        self.featureName  = featureName
    def __repr__(self):
        return "Feature %s is obsolete" % (self.featureName)
    def __str__(self):
        return "Feature %s is obsolete" % (self.featureName)

class AbstractClassException(Exception):
    def __init__(self, methodName):
        e = Exception()
        Exception.__init__(e)
        self.className  = methodName
    def __repr__(self):
        return "Method %s was attempted in an abstract class" % (self.className)
    def __str__(self):
        return "Method %s was attempted in an abstract class" % (self.className)

class DependencyLoopException(Exception):
    def __init__(self, chain):
        e = Exception()
        Exception.__init__(e)
        self.chain  = chain
    def __repr__(self):
        output  = "Package chain has a recursive loop in dependencies: "
        for pkgName in self.chain:
            output += pkgName + ','
        return output[:-1]
    def __str__(self):
        output  = "Package chain has a recursive loop in dependencies: "
        for pkgName in self.chain:
            output += pkgName + ','
        return output[:-1]

class BadPackage(Exception):
    def __init__(self, pkn, errmsg):
        e = Exception()
        Exception.__init__(e)
        self.errmsg      = errmsg
        self.pkn = pkn
    def __repr__(self):
        return "%s: %s" % (self.pkn, self.errmsg)
    def __str__(self):
        return "%s: %s" % (self.pkn, self.errmsg)

class BadBillOfMaterials(Exception):
    def __init__(self, errmsg):
        e = Exception()
        Exception.__init__(e)
        self.errmsg = errmsg
    def __repr__(self):
        return "%s" % self.errmsg
    def __str__(self):
        return "%s" % self.errmsg

class QuitException(Exception):
    pass

class ServerUnavailable(Exception):
    def __init__(self, path, errmsg):
        e = Exception()
        Exception.__init__(e)
        self.errmsg = errmsg
        self.path   = path
    def __str__(self):
        return "%s: %s" % (self.path, self.errmsg)

class FileNotFound(ServerUnavailable):
    def __init__(self, path, data=""):
        ServerUnavailable.__init__(self, path, data)

class StoppedExecution(Exception):
    pass

class MissingComponent(Exception):
    def __init__(self, name):
        e = Exception()
        Exception.__init__(e)
        self.name = name
    def __str__(self):
        return self.name
    
