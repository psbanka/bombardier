#!/usr/bin/env python

"common stuff that many Bombardier modules need."

# Copyright (C) 2005-2010 Peter Banka, Shawn Sherwood

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

import os, re, time, random, yaml, shutil
import sys
from static_data import OK, FAIL, BOMBARDIER_CONFIG_DIR
from static_data import PACKAGES, STATUS_FILE, CLIENT_CONFIG_FILE
from Exceptions import StatusException, UnsupportedPlatform
from Exceptions import ConfigurationException


def get_hasher():
    "Return md5sum using appropriate library"
    hasher = None
    try:
        import hashlib
        hasher = hashlib.md5()
    except ImportError:
        import md5
        hasher = md5.md5()
    return hasher

def md5_sum(value):
    "Return md5sum"
    hash_obj = get_hasher()
    hash_obj.update(value)
    return hash_obj.hexdigest()

def update_dict(newdict, olddict):
    """mashes two dictionaries together
    >>> update_dict({'a': 1}, {})
    {'a': 1}
    >>> update_dict({'a': 1}, {'b':2})
    {'a': 1, 'b': 2}
    >>> update_dict({'a': 1}, {'a':2})
    {'a': 1}
    >>> update_dict({'a': [1]}, {'a':[2]})
    {'a': [2, 1]}
    """
    for key, value in newdict.iteritems():
        if type(value) == type({}) and olddict.has_key(key):
            olddict[key] = update_dict(value, olddict[key])
        elif type(value) == type(["list"]) and olddict.has_key(key):
            olddict[key] = olddict[key] + value
        else:
            olddict[key] = value
    return olddict

def integrate(data, dictionary, overwrite):
    "mashes two dictionaries together based on timestamp"
    data["timestamp"] = time.time()
    if overwrite:
        for key, value in dictionary.iteritems():
            data[key] = value
    else:
        data = update_dict(dictionary, data)
    return data

def get_tmp_path():
    "probably could be replaced by tempfile"
    alphabet = map(chr, range(97, 123))
    tmp_fn    = "tmp"
    tmp_fn   += random.choice(alphabet)
    tmp_fn   += random.choice(alphabet)
    tmp_fn   += random.choice(alphabet)
    tmp_fn   += random.choice(alphabet)
    tmp_fn   += random.choice(alphabet)
    tmp_path  = os.path.join(get_spkg_path(), tmp_fn)
    return tmp_path

def cygpath(dos_path):
    "Converts an MS-DOS path to a POSIX path"
    prefix = ''
    if not dos_path:
        return ''
    if ':' in dos_path:
        drive = dos_path.split(':')[0]
        prefix = '/cygdrive/%s' % drive
        dos_path = dos_path.split(':')[-1]
    return prefix + dos_path.replace('\\', '/')

def hash_list(listobj):
    """hashes all the values of a list

    >>> hash_list(['a', 1])
    ['0cc175b9c0f1b6a831c399e269772661', 'c4ca4238a0b923820dcc509a6f75849b']
    """
    output = []
    for value in listobj:
        if type(value) == type({}):
            output.append(hash_dictionary(value))
        elif type(value) == type([]):
            output.append(hash_list(value))
        elif type(value) == type('string'):
            output.append(md5_sum(value))
        elif  type(value) == type(1):
            output.append(md5_sum(str(value)))
    return output

def hash_dictionary(dictionary):
    """goes through a dictionary and turns all the values in it
    to md5 hashes. This is useful for saving off the information
    about a configuration without saving the configuration itself

    >>> hash_dictionary({'a': 2, 'b': {'c': 'terrance'}})
    {'a': 'c81e728d9d4c2f636f067f89cc14862c', 'b': {'c': '370574fb6b12bfabab1b23a7b8c11c0a'}}
    """
    output = {}
    for key in dictionary.keys():
        value = dictionary[key]
        if type(value) == type({}):
            output[key] = hash_dictionary(value)
        elif type(value) == type([]):
            output[key] = hash_list(value)
        elif type(value) == type('string'):
            output[key] = md5_sum(value)
        elif type(value) == type(1):
            output[key] = md5_sum(str(value))
    return output

def diff_lists(sub_list, super_list, check_values=False):
    """Finds the differences between two lists


    """
    differences = []
    if len(sub_list) == 0:
        return []
    if type(sub_list[0]) == type(super_list[0]):
        if type(sub_list[0]) == type(1) or type(sub_list[0]) == type('string'):
            # comparing a list of literals, length doesn't matter.
            return []
    if len(sub_list) != len(super_list):
        differences = list(set(sub_list) - set(super_list))
    for index in range(0, len(sub_list)):
        sub_value = sub_list[index]
        super_value = super_list[index]
        if type(sub_value) != type(super_value):
            differences.append(sub_value)
            continue
        if type(sub_value) == type({}):
            differences.append(diff_dicts(sub_value, super_value, check_values))
            continue
        elif type(sub_value) == type([]):
            differences.append(diff_lists(sub_value, super_value, check_values))
            continue
        elif type(sub_value) == type('string') or type(sub_value) == type(1):
            if check_values and sub_value != super_value:
                differences.append(sub_value)
                continue
        else:
            differences.append(sub_value)
            continue
    return differences

def is_scalar(test_val):
    "Determines if an input object is a scalar value"
    if type(test_val) == type('string') or type(test_val) == type(1) or \
       type(test_val) == type(True):
        return True
    return False


def diff_dicts(sub_dict, super_dict, check_values=False):
    """Finds all the entries in sub_dict that are not in super_dict.
    
    >>> diff_dicts({'a':1}, {'b':2})
    {'a': 1}

    >>> diff_dicts({'a':1, 'b':2}, {'b':2})
    {'a': 1}

    >>> diff_dicts({'b': 2}, {'a':1, 'b':2})
    {}

    When lists are encountered, only the type is relevant, and not the values.
    For example:

    >>> diff_dicts({'b': [2]}, {'a':1, 'b':[1,2,3,4]})
    {}
    
    >>> diff_dicts({'b': 2}, {'a':1, 'b':[1,2,3,4]})
    {'b': 2}

    Similarly, all scalar values are treated the same, whether they are string
    or character values:
    >>> diff_dicts({'b': 2}, {'a':1, 'b':'2'})
    {}

    Unless the check_values flag is given:
    >>> diff_dicts({'b': 2}, {'a':1, 'b':'2'}, True)
    {'b': 2}
    >>> diff_dicts({'a':1}, {'a': 2}, True)
    {'a': 1}
    >>> diff_dicts({'a':True}, {'a': False}, True)
    {'a': True}
    >>> diff_dicts({'a':True}, {'a': True}, True)
    {}
    >>> diff_dicts({'a':1}, {'a': 1}, True)
    {}

    """
    differences = {}
    for sub_key in sub_dict.keys():
        if sub_key not in super_dict.keys():
            differences[sub_key] = sub_dict[sub_key]
            continue
        sub_value = sub_dict[sub_key]
        super_value = super_dict[sub_key]
        if is_scalar(sub_value):
            if not is_scalar(super_value):
                differences[sub_key] = sub_value
                continue
            if not check_values:
                continue
            if sub_value != super_value:
                differences[sub_key] = sub_value
                continue
            continue
        elif type(sub_value) != type(super_value):
            differences[sub_key] = sub_value
            continue
        elif type(sub_value) == type({}):
            diff = diff_dicts(sub_value, super_value, check_values)
            if diff != {}:
                differences[sub_key] = diff
            continue
        elif type(sub_value) == type([]):
            diff = diff_lists(sub_value, super_value, check_values)
            if diff != []:
                differences[sub_key] = diff
            continue
        else:
            differences[sub_key] = sub_value
            continue
    return differences

def compare_dicts(sub_dict, super_dict, check_values=False):
    "Tells you if two dictionaries are the same or not"
    if diff_dicts(sub_dict, super_dict, check_values) == {}:
        return True
    return False

def datesort(thing_one, thing_two):
    "used by sort() to sort a list"
    if type(thing_one) == type(["list"]):
        if type(thing_two) == type(["list"]):
            if thing_one[1]:
                if thing_two[1]:
                    return thing_one[1] - thing_two[1]
    return False

def get_time_struct(time_string):
    """Convert the time_string into a time struct. If a package has
    been both installed and uninstalled, it's necessary to known which
    one came more recently so that we know if the package is on the
    system or not.

    >>> get_time_struct("NA")
    0

    >>> get_time_struct("Fri Dec  4 17:13:46 2009")
    1259975626
    """
    if time_string == "NA":
        return 0
    try:
        timestruct = int(time.mktime(time.strptime(time_string)))
    except ValueError:
        timestruct = int(time.time())
    return timestruct

def rpartition(string, char):
    "Python2.5's rpartition method. Useful in old versions of Python"
    chunks = string.split(char)
    if len(chunks) == 1:
        return ('', '', string)
    first = char.join(chunks[:-1])
    last = chunks[-1]
    return (first, char, last)

# CONFIGURATION FILE METHODS

def ensure_bombardier_config_dir():
    if not os.path.isdir(BOMBARDIER_CONFIG_DIR):
        os.system("mkdir -p %s" % BOMBARDIER_CONFIG_DIR)

def get_linux_client_config():
    "Read CLIENT_CONFIG_FILE and give us the info"
    spkg_dict = {"spkg_path": "/opt/spkg"}
    ensure_bombardier_config_dir()
    if not os.path.isfile(CLIENT_CONFIG_FILE):
        put_linux_client_config(spkg_dict)
        return spkg_dict
    yaml_str = open(CLIENT_CONFIG_FILE, 'r').read()
    config = yaml.load(yaml_str)
    if "spkg_path" not in config:
        config.update( spkg_dict )
        put_linux_client_config(config)
    return config

def put_linux_client_config(config):
    "Write a change to our config file"
    ensure_bombardier_config_dir()
    data = open(CLIENT_CONFIG_FILE, 'w')
    data.write(yaml.dump(config))

def add_dictionaries(dict1, dict2):
    """dict1 gets stuff from dict2, only if it doesn't have it"""
    for key, value in dict2.iteritems():
        if not dict1.has_key(key):
            dict1[key] = value
        else:
            if type(value) == type(dict1[key]):
                if type(value) == type(dict()):
                    dict1[key] = add_dictionaries(dict1[key], value)
    return dict1

def get_spkg_path():
    "Find out where our root directory is on the client"
    spkg_path = ''
    if sys.platform == "linux2" or sys.platform == "cygwin":
        config = get_linux_client_config()
        spkg_path = config.get("spkg_path")
        if not spkg_path:
            spkg_path = config.get("spkgPath")
    elif sys.platform == "win32":
        import _winreg as winreg
        key_name = r"Software\GE-IT\Bombardier"
        try:
            key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                                 key_name, 0, winreg.KEY_QUERY_VALUE)
            spkg_path, dummy = winreg.QueryValueEx(key, "InstallPath")
        except:
            spkg_path = r"C:\spkg"
    else:
        raise UnsupportedPlatform()
    return spkg_path

def get_package_path(instance_name):
    "Find out where our package repository is"
    return os.path.join(get_spkg_path(), instance_name, PACKAGES)

def get_progress_path(instance_name):
    "Find out where our 'status.yml' file is"
    new_path = os.path.join(get_spkg_path(), instance_name, STATUS_FILE)
    return new_path

if __name__ == "__main__":
    import doctest
    doctest.testmod()

