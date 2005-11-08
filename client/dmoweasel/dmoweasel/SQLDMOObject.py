#!/cygdrive/c/Python24/python.exe

# SQLDMOObject.py: This library is for dealing with either objects
# within an MSSQL database or collections of objects within that DB.

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

import SQLDMOQuery2
import SQLObjectType2

class SQLDMOObject:
    def __init__(self, com):
        self.com = com

    def sqlType(self):
        return SQLObjectType2.code2name[self.com.Type]

    def shortName(self):
        return self.com.Name

    def deps(self):
        if hasattr(self.com, 'EnumDependencies'):
            deps = SQLDMOQuery2.SQLDMOQuery(self.com.EnumDependencies())
            rows = deps.get_rows()

            for row in rows:
                typeName = SQLObjectType2.mapping(int(row['oType']))
                name = str(row['oObjName'])

                yield name, typeName

        else:
            raise StopIteration

    def _encapsulate(self, com):
        if hasattr(com, 'Count'):
            return SQLDMOCollection(com)
        else:
            return SQLDMOObject(com)

    def __getitem__(self, key):
        if hasattr(self.com, key):
            obj = self._encapsulate(self.com.__getattr__(key))
            return obj 
        else:
            raise KeyError

    def __repr__(self):
        if hasattr(self.com, 'Name'):
            return self.com.Name + "/"
        else:
            return `self.com`

class SQLDMOCollection(SQLDMOObject):

    def __init__(self, com):
        SQLDMOObject.__init__(self, com)

        if not hasattr(com, "Count"):
            raise TypeError

    def __getitem__(self, key):
        #obj = self._encapsulate(self._map()[key])
        obj = self._map()[key]
        return obj 

    def keys(self):
        return self._map().keys() 

    def items(self):
        return self._map().items() 

    def _map(self):
        #For some reson COM keys don't always match "names"
        #So we don't use the keys of COM collections
        name2Object = {}

        for i in range(0, self.com.Count):
            obj = self.com[i] 
            name2Object[obj.Name] = self._encapsulate(obj)

        return name2Object

    def __repr__(self):
        return self.keys()
