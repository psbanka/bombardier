#!/cygdrive/c/Python24/python.exe

# Exceptions.py: These are custom exceptions thrown and (sometimes)
# caught by Bombardier.

# Copyright (C) 2005 Peter Banka

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

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
    def __init__(self, package_name, errmsg):
        e = Exception()
        Exception.__init__(e)
        self.errmsg      = errmsg
        self.package_name = package_name
    def __repr__(self):
        return "%s: %s" % (self.package_name, self.errmsg)
    def __str__(self):
        return "%s: %s" % (self.package_name, self.errmsg)

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
    
