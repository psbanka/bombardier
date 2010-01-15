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

import os, re, time, random, yaml, md5, shutil
import sys
from static_data import OK, FAIL
from static_data import PACKAGES, STATUS_FILE, SERVER_CONFIG_FILE
from Exceptions import StatusException, UnsupportedPlatform
from Exceptions import ConfigurationException

BROKEN_INSTALL   = 0
INSTALLED        = 1
BROKEN_UNINSTALL = 2
UNINSTALLED      = 3

def load_current_progress(instance_name):
    "Loads the status.yml file for this instance"
    status_path = get_progress_path(instance_name)
    try:
        raw_data = open(status_path, 'r').read()
        data = yaml.load(raw_data)
        assert type(data) == type({})
    except Exception:
        raise StatusException(status_path)
    return data

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

def update_progress(dictionary, instance_name, overwrite=False):
    "updates package status information"
    status_path = get_progress_path(instance_name)
    tmp_path    = get_tmp_path()
    data        = load_current_progress(instance_name)
    try:
        int_data = integrate(data, dictionary, overwrite)
        yaml_string = yaml.dump(int_data, default_flow_style=False)
        file_handle = open(tmp_path, 'w')
        file_handle.write(yaml_string)
        file_handle.flush()
        file_handle.close()
        shutil.copy(tmp_path, status_path)
        os.unlink(tmp_path)
    except IOError:
        return FAIL
    except StatusException:
        return FAIL
    return OK

def get_progress_data(instance_name, strip_version_from_name = False):
    "Obtains just the installation progress information from status.yml"
    if not instance_name:
        raise ConfigurationException("Must have a valid instance_name")
    data = load_current_progress(instance_name)
    if data.has_key("install-progress"):
        progress_data = data["install-progress"]
        if strip_version_from_name:
            return strip_version_from_keys(progress_data)
        return progress_data
    return {}

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
            output.append(md5.new(value).hexdigest())
        elif  type(value) == type(1):
            output.append(md5.new(`value`).hexdigest())
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
            output[key] = md5.new(value).hexdigest()
        elif type(value) == type(1):
            output[key] = md5.new(`value`).hexdigest()
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
    """
    differences = {}
    for sub_key in sub_dict.keys():
        if sub_key not in super_dict.keys():
            differences[sub_key] = sub_dict[sub_key]
            continue
        sub_value = sub_dict[sub_key]
        super_value = super_dict[sub_key]
        if type(sub_value) == type('string') or type(sub_value) == type(1):
            if type(super_value) != type('string') and \
              type(super_value) != type(1):
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

def strip_version(package_name):
    """removes the -\d version number from a package name
    >>> strip_version("abc")
    'abc'
    >>> strip_version("abc-2")
    'abc'
    >>> strip_version("abc-2834343-2")
    'abc-2834343'
    """

    if package_name.rfind('-') == -1:
        return package_name
    ending = package_name[package_name.rfind('-')+1:]
    validator = re.compile("([0-9]+)")
    if validator.search(ending):
        if validator.search(ending).groups()[0] == ending:
            package_name = package_name[:package_name.rfind('-')]
    return package_name

def strip_version_from_keys(progress_data):
    """removes the "-\d" version information from the progress info
    >>> b = {'LdapServer-19': {'UNINSTALLED': 'NA', 
    ...                        'VERIFIED': 'Fri Dec  4 17:13:46 2009',
    ...                        'DEPENDENCY_ERRORS': [],
    ...                        'INSTALLED': 'Fri Dec  4 17:13:46 2009'}}
    >>> strip_version_from_keys(b)
    {'LdapServer': {'UNINSTALLED': 'NA', 'VERIFIED': 'Fri Dec  4 17:13:46 2009', 'DEPENDENCY_ERRORS': [], 'INSTALLED': 'Fri Dec  4 17:13:46 2009'}}
    """
    output = {}
    for key in progress_data.keys():
        output[strip_version(key)] = progress_data[key]
    return output

def determine_install_status(item, progress_data):
    '''Goes through the progress data that is stored in status.yml,
    and determines which packages are broken-not installed, which are installed,
    which are broken-installed, and which are uninstalled.

    >>> b = {'LdapServer-19': {'UNINSTALLED': 'NA',
    ...                        'VERIFIED': 'Fri Dec  4 17:13:46 2009',
    ...                        'DEPENDENCY_ERRORS': [],
    ...                        'INSTALLED': 'Fri Dec  4 17:13:46 2009'}}
    >>> determine_install_status("LdapServer-19", b)
    (1, 1259975626)

    >>> b["LdapServer-19"]["UNINSTALLED"] = 'Sat Dec  5 17:13:46 2009'
    >>> determine_install_status("LdapServer-19", b)
    (3, 1260062026)
    '''
    if progress_data[item].get("INSTALLED") == None:
        progress_data[item]["INSTALLED"] = ''
    if progress_data[item].get("UNINSTALLED") == None:
        progress_data[item]["UNINSTALLED"] = ''
    inst_txt = progress_data[item]["INSTALLED"]
    uninst_txt = progress_data[item]["UNINSTALLED"]
    if inst_txt == "BROKEN":
        return BROKEN_INSTALL, None
    if uninst_txt == "BROKEN":
        return BROKEN_UNINSTALL, None
    if inst_txt == "NA":
        return UNINSTALLED, None
    inst_int = get_time_struct(inst_txt)
    if uninst_txt != "NA":
        uninst_int = get_time_struct(uninst_txt)
        if uninst_int > inst_int:
            return UNINSTALLED, uninst_int
    return INSTALLED, inst_int

def get_installed_uninstalled_times(progress_data):
    """Provides information about what time installation or uninstallation
    activities have taken place

    >>> b = {'LdapServer-19': {'UNINSTALLED': 'NA',
    ...                        'VERIFIED': 'Fri Dec  4 17:13:46 2009',
    ...                        'DEPENDENCY_ERRORS': [],
    ...                        'INSTALLED': 'Fri Dec  4 17:13:46 2009'}}
    >>> get_installed_uninstalled_times(b)
    {'broken_uninstalled': [], 'uninstalled': [], 'broken_installed': [], 'installed': [['LdapServer-19', 1259975626]]}
    """
    output = {"installed":[], "uninstalled":[], 
              "broken_installed":[], "broken_uninstalled":[]}
    for item in progress_data.keys():
        status, last_action = determine_install_status(item, progress_data)
        if status == INSTALLED:
            output["installed"].append([item, last_action])
        elif status == UNINSTALLED:
            output["uninstalled"].append([item, last_action])
        elif status == BROKEN_INSTALL:
            output["broken_installed"].append([item, last_action])
        elif status == BROKEN_UNINSTALL:
            output["broken_uninstalled"].append([item, last_action])
    output["installed"].sort(datesort)
    output["uninstalled"].sort(datesort)
    return output

def rpartition(string, char):
    "Python2.5's rpartition method. Useful in old versions of Python"
    chunks = string.split(char)
    if len(chunks) == 1:
        return ('', '', string)
    first = char.join(chunks[:-1])
    last = chunks[-1]
    return (first, char, last)

def check_if_more_recent(base_package_name, action_time, comparison_list):
    """If a package has both an INSTALL action and an UNINSTALL action
    shown next to it, we need to determine which is the more recent and
    then ignore the other one.
    """
    remove = False
    for full_package_name_2, action_time_2 in comparison_list:
        base_package_name_2 = rpartition(full_package_name_2, '-')[0]
        if base_package_name == base_package_name_2:
            if action_time_2 > action_time:
                remove = True
    return remove

def strip_version_info(pkg_info):
    '''
    This strips '-\d' version information from each package_name in
    the pkg_info dictionary.
    pkg_info -- a dictionary of ["installed"], ["uninstalled"],
                ["broken_installed"], ["broken_uninstalled"]. Each item
                is a tuple of package_name / date.
    '''
    output = {"installed":[], "uninstalled":[], 
              "broken_installed":[], "broken_uninstalled":[]}
    package_list_names = output.keys()

    # look at package_list_name for duplicates in other listTypes
    for package_list_name in package_list_names:
        # do other package lists have this basename?
        for full_package_name, action_time in pkg_info[package_list_name]:
            base_package_name = rpartition(full_package_name, '-')[0]
            other_types = output.keys()
            other_types.remove(package_list_name)
            for compare_list in other_types:
                more_recent = check_if_more_recent(base_package_name, action_time,
                                              pkg_info[compare_list])
                if more_recent == True:
                    break
            if not more_recent:
                output[package_list_name].append([base_package_name,
                                                  action_time])
    return output

def get_installed(progress_data):
    """Tells you what packages are installed (and which are broken)

    >>> b = {'LdapServer-19': {'UNINSTALLED': 'NA',
    ...                        'VERIFIED': 'Fri Dec  4 17:13:46 2009',
    ...                        'DEPENDENCY_ERRORS': [],
    ...                        'INSTALLED': 'Fri Dec  4 17:13:46 2009'}}
    >>> get_installed(b)
    (['LdapServer'], [])

    >>> b["LdapServer-19"]["UNINSTALLED"] = 'Fri Dec  4 20:13:46 2009'
    >>> get_installed(b)
    ([], [])

    >>> b["LdapServer-19"]["UNINSTALLED"] = 'BROKEN'
    >>> get_installed(b)
    ([], ['LdapServer'])
    """
    pkg_info = get_installed_uninstalled_times(progress_data)
    pkg_info = strip_version_info(pkg_info)
    installed_package_names = [package_name[0] for package_name in pkg_info["installed"]]
    broken_package_names    = [package_name[0] for package_name in pkg_info["broken_installed"]]
    broken_package_names   += [package_name[0] for package_name in pkg_info["broken_uninstalled"]]
    return installed_package_names, broken_package_names

# CONFIGURATION FILE METHODS

def get_linux_config():
    "our config file is in /etc/bombardier.yml. Go read it and give us the info"
    try:
        data = open(SERVER_CONFIG_FILE, 'r').read()
    except IOError, ioe:
        raise ConfigurationException(str(ioe))
    config = yaml.load(data)
    return config

def put_linux_config(config):
    "Write a change to our config file"
    data = open("/etc/bombardier.yml", 'w')
    data.write(yaml.dump(config))

def add_dictionaries(dict1, dict2):
    """dict1 gets stuff from dict2, only if it doesn't have it"""
    for key,value in dict2.iteritems():
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
        config = get_linux_config()
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

