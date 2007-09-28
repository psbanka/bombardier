#!/cygdrive/c/Python24/python.exe

# Copyright (C) 2004 Harald Armin massa, 2004/07/21

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

from __future__ import generators
import win32api, win32con, cPickle, pywintypes

class RegistryDict(object):
    """ RegistryDict.py: Slightly magical Win32api Registry ->
        Dictionary-like-object wrapper """

    def __init__(self, keypath = [], keyhandle = win32con.HKEY_LOCAL_MACHINE, flags = None):
        """If flags=None, then it will create the key.. otherwise pass a win32con.KEY_* sam"""
        self.keyhandle = None
        self.open(keyhandle, keypath, flags)

    def massageIncomingRegistryValue((obj, objtype)):
        if not obj:
            return None
        if objtype == win32con.REG_BINARY and obj[:8]=='PyPickle':
            obj = obj[8:]
            return cPickle.loads(obj)
        elif objtype == win32con.REG_NONE:
            return None
        elif objtype in (win32con.REG_SZ, win32con.REG_EXPAND_SZ, \
                         win32con.REG_RESOURCE_LIST, win32con.REG_LINK, \
                         win32con.REG_BINARY, win32con.REG_DWORD, \
                         win32con.REG_DWORD_LITTLE_ENDIAN, \
                         win32con.REG_DWORD_BIG_ENDIAN, win32con.REG_MULTI_SZ):
            return obj
        raise NotImplementedError, "Registry type 0x%08X not supported" % (objtype,)

    massageIncomingRegistryValue = staticmethod(massageIncomingRegistryValue)

    def __getitem__(self, item):
        item = str(item)

        # is it data?
        try:
        #if 1 == 1:
            (obj, objtype) = win32api.RegQueryValueEx(self.keyhandle, item)
            value = self.massageIncomingRegistryValue((obj, objtype))
            return value
        #else:
        except:
            pass

        # it's probably a key then
        try:
            r = RegistryDict(item, self.keyhandle, win32con.KEY_ALL_ACCESS)
            return r
        except:
            pass

        # must not be there
        raise KeyError, item
    
    def has_key(self, key):
        return self.__contains__(key)
    
    def __contains__(self, key):
        try:
            self.__getitem__(key)
            return 1
        except KeyError:
            return 0

    def copy(self):
        return dict(self.iteritems())

    def __repr__(self):
        return repr(self.copy())

    def __str__(self):
        return self.__repr__()

    def __cmp__(self, other):
        return cmp(self.copy(), other)

    def __hash__(self):
        raise TypeError, "RegistryDict objects are unhashable"
  
    def clear(self):
        for k in self.iterkeys():
            del self[k]
    
    def iteritems_data(self):
        i = 0
        # yield data
        try:
            while 1:
                s, obj, objtype = win32api.RegEnumValue(self.keyhandle, i)
                yield s, self.massageIncomingRegistryValue((obj, objtype))
                i += 1
        except pywintypes.error, e:
            if e[0] != 259:
                print "ERROR: %s" % e

    def iteritems_children(self, access=win32con.KEY_ALL_ACCESS):
        i = 0
        #if 1 == 1:
        try:
            while 1:
                s = win32api.RegEnumKey(self.keyhandle, i)
                yield s, RegistryDict([s], self.keyhandle, access)
                i += 1
        #else:
        except:
            pass
                
    def iteritems(self, access=win32con.KEY_ALL_ACCESS):
       # yield children
        for item in self.iteritems_data():
            yield item
        for item in self.iteritems_children(access):
            yield item
            
    def iterkeys_data(self):
        for key, value in self.iteritems_data():
            yield key

    def iterkeys_children(self, access=win32con.KEY_ALL_ACCESS):
        for key, value in self.iteritems_children(access):
            yield key

    def iterkeys(self):
        for key, value in self.iteritems():
            yield key

    def itervalues_data(self):
        for key, value in self.iteritems_data():
            yield value

    def itervalues_children(self, access=win32con.KEY_ALL_ACCESS):
        for key, value in self.iteritems_children(access):
            yield value

    def itervalues(self, access=win32con.KEY_ALL_ACCESS):
        for key, value in self.iteritems(access):
            yield value

    def items(self, access=win32con.KEY_ALL_ACCESS):
        return list(self.iteritems())
              
    def keys(self):
        return list(self.iterkeys())

    def values(self, access=win32con.KEY_ALL_ACCESS):
        return list(self.itervalues(access))
        
    def __delitem__(self, item):
        win32api.RegDeleteValue(self.keyhandle, str(item))
  
    def __len__(self):
        return len(self.items())

    def __iter__(self):
        return self.iterkeys()
  
    def popitem(self):
        try:
            k, v = self.iteritems().next()
            del self[k]
            return k, v
        except StopIteration:
            raise KeyError, "RegistryDict is empty"
            
    def get(self,key,default=None):
        try:
            return self.__getitem__(key)
        except:
            return default

    def setdefault(self,key,default=None):
        try:
            return self.__getitem__(key)
        except:
            self.__setitem__(key)
            return default

    def update(self,d):
        for k,v in d.items():
            self.__setitem__(k, v)

    def __setitem__(self, item, value):
        item = str(item)
        pyvalue = type(value)
        if pyvalue is dict or isinstance(value, RegistryDict):
            d = RegistryDict(item, self.keyhandle)
            d.clear()
            d.update(value)
            return
        if pyvalue is str:
            valuetype = win32con.REG_SZ
        elif pyvalue is int:
            valuetype = win32con.REG_DWORD
        else:
            valuetype = win32con.REG_BINARY
            value = 'PyPickle' + cPickle.dumps(value)
        win32api.RegSetValueEx(self.keyhandle, item, 0, valuetype, value)
  
    def open(self, keyhandle, keypath, flags = None):
        if self.keyhandle:
            self.close()
        if type(keypath) is str:
            keypath = keypath.split('\\')
        if flags is None:
            for subkey in keypath:
                print "subkey", subkey
                try:
                    keyhandle = win32api.RegOpenKeyEx(keyhandle, subkey, 0)
                except:
                    try:
                        keyhandle = win32api.RegOpenKeyEx(keyhandle, subkey.lower(), 0)
                    except:
                        keyhandle = win32api.RegOpenKeyEx(keyhandle, subkey.upper(), 0)
                

        else:
            for subkey in keypath:
                try:
                    keyhandle = win32api.RegOpenKeyEx(keyhandle, subkey, 0, flags)
                except:
                    try:
                        keyhandle = win32api.RegOpenKeyEx(keyhandle, subkey.lower(), 0, flags)
                    except:
                        keyhandle = win32api.RegOpenKeyEx(keyhandle, subkey.upper(), 0, flags)

        self.keyhandle = keyhandle

    def close(self):
        try:
            win32api.RegCloseKey(self.keyhandle)
        except:
            pass

    def __del__(self):
        self.close()

if __name__ == "__main__":
    pass
