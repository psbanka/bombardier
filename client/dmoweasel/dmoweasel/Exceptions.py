#!/cygdrive/c/Python24/python.exe

# Exceptions.py: Here are all the exceptions that the weasel can
# raise.

# Copyright (C) 2005 Seth de l'Isle, Peter Banka

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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

class AccessDenied(Exception):
    def __init__(self, connectionString):
        e = Exception()
        Exception.__init__(e)
        self.errmsg = "Access denied to server %s (or it doesn't exist)" % (connectionString)
    def __repr__(self):
        return self.errmsg
    def __str__(self):
        return self.errmsg

class UnknownType(Exception):
    def __init__(self, typeName, objectName):
        Exception.__init__(self)
        self.errmsg = "Object named %s is of an unknown type (%s)" % (objectName, typeName)
    def __repr__(self):
        return self.errmsg
    def __str__(self):
        return self.errmsg
    
class CannotDropObject(Exception):
    def __init__(self, path):
        e = Exception()
        Exception.__init__(e)
        self.errmsg = "Cannot drop object %s" % (path)
    def __repr__(self):
        return self.errmsg
    def __str__(self):
        return self.errmsg

class UnusableObject(Exception):
    def __init__(self, path, action):
        e = Exception()
        Exception.__init__(e)
        self.errmsg = "Cannot perform the requested action (%s) on object %s" % (action, path)
    def __repr__(self):
        return self.errmsg
    def __str__(self):
        return self.errmsg

class NoScriptFileFound(Exception):
    def __init__(self, path):
        e = Exception()
        Exception.__init__(e)
        self.errmsg = "%s does not exist on the filesystem" % path
    def __repr__(self):
        return self.errmsg
    def __str__(self):
        return self.errmsg

class NoDbObjectFound(Exception):
    def __init__(self, path):
        e = Exception()
        Exception.__init__(e)
        self.errmsg = "object %s does not exist on the server" % path
    def __repr__(self):
        return self.errmsg
    def __str__(self):
        return self.errmsg

class EmptyObject(Exception):
    def __init__(self, path):
        e = Exception()
        Exception.__init__(e)
        self.errmsg = "%s is empty" % path
    def __repr__(self):
        return self.errmsg
    def __str__(self):
        return self.errmsg

class ObjectUnavailable(Exception):
    def __init__(self, path):
        e = Exception()
        Exception.__init__(e)
        self.errmsg = "%s unavailable" % path
    def __repr__(self):
        return self.errmsg
    def __str__(self):
        return self.errmsg

class ConfigurationError(Exception):
    def __init__(self, errmsg):
        e = Exception()
        Exception.__init__(e)
        self.errmsg      = errmsg
    def __repr__(self):
        return self.errmsg
    def __str__(self):
        return self.errmsg

class CommandLineError:
    def __init__(self, warning="Error parsing command line"):
        self.warning = warning
        self.usage = "Usage: db-browser.py [-S] " \
                     "[-u <username> -p <password>] " \
                     "server path"
    def __repr__(self):
        return self.warning + '\n' +  self.usage

class CollectionExistenceAssumption(Exception):
    def __init__(self, collectionName):
        e = Exception()
        Exception.__init__(e)
        self.collectionName = collectionName
    def __repr__(self):
        return self.collectionName
    def __str__(self):
        return self.collectionName
