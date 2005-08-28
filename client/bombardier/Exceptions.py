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

class BadPackage(Exception):
    def __init__(self, packageName, errmsg):
        e = Exception()
        Exception.__init__(e)
        self.errmsg      = errmsg
        self.packageName = packageName
    def __repr__(self):
        return "%s: %s" % (self.packageName, self.errmsg)
    def __str__(self):
        return "%s: %s" % (self.packageName, self.errmsg)

class BadBillOfMaterials(Exception):
    def __init__(self, errmsg):
        e = Exception()
        Exception.__init__(e)
        self.errmsg = errmsg
    def __repr__(self):
        return "%s" % self.errmsg
    def __str__(self):
        return "%s" % self.errmsg

class NoYamlData(Exception):
    def __init__(self, filename):
        e = Exception()
        Exception.__init__(e)
        self.filename = filename
    def __str__(self):
        return self.filename

class ServiceNotFound(Exception):
    def __init__(self, serviceName):
        e = Exception()
        Exception.__init__(e)
        self.serviceName = serviceName
    def __str__(self):
        return self.serviceName

class ServiceShutdown(Exception):
    pass

class QuitException(Exception):
    pass

class ServerUnavailable(Exception):
    def __init__(self, url, errmsg):
        e = Exception()
        Exception.__init__(e)
        self.errmsg = errmsg
        self.url    = url
    def __str__(self):
        return "%s: %s" % (self.url, self.errmsg)

class FileNotFound(ServerUnavailable):
    def __init__(self, url):
        ServerUnavailable.__init__(self, url, "")

class PipeNotListenable(Exception):
    def __init__(self, pipeName):
        e = Exception()
        Exception.__init__(e)
        self.pipeName = pipeName
    def __str__(self):
        return self.pipeName
    
class StoppedExecution(Exception):
    pass

class InvalidProgress(Exception):
    def __init__(self, progressData):
        e = Exception()
        Exception.__init__(e)
        self.badData = progressData
    def __str__(self):
        return "Invalid progress: %s" % self.badData
    def __repr__(self):
        return "Invalid progress: %s" % self.badData
    
class MissingComponent(Exception):
    def __init__(self, name):
        e = Exception()
        Exception.__init__(e)
        self.name = name
    def __str__(self):
        return self.name
    
