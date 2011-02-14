#!/usr/bin/env python

"Holds the Progress class for examining and modifying the status of a machine"

import os, re, shutil
from static_data import OK, FAIL
from Exceptions import StatusException

from mini_utility import rpartition, integrate, get_time_struct
from mini_utility import datesort, get_progress_path, yaml_load, yaml_dump
import tempfile

BROKEN_INSTALL   = 0
INSTALLED        = 1
BROKEN_UNINSTALL = 2
UNINSTALLED      = 3

class Progress:

    "Tracks the installation status of a machine"

    def __init__(self, instance_name):
        "progress_path -- the location for the status.yml file"
        self.progress_path = get_progress_path(instance_name)

    @classmethod
    def check_if_more_recent(cls, base_package_name, action_time,
                             comparison_list):
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

    @classmethod
    def strip_version_info(cls, pkg_info):
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
                    more_recent = cls.check_if_more_recent(base_package_name,
                                                       action_time,
                                                       pkg_info[compare_list])
                    if more_recent == True:
                        break
                if not more_recent:
                    output[package_list_name].append([base_package_name,
                                                      action_time])
        return output

    @classmethod
    def get_installed(cls, progress_data):
        """Tells you what packages are installed (and which are broken)

        >>> b = {'LdapServer-19': {'UNINSTALLED': 'NA',
        ...                        'VERIFIED': 'Fri Dec  4 17:13:46 2009',
        ...                        'DEPENDENCY_ERRORS': [],
        ...                        'INSTALLED': 'Fri Dec  4 17:13:46 2009'}}
        >>> Progress.get_installed(b)
        (['LdapServer'], [])

        >>> b["LdapServer-19"]["UNINSTALLED"] = 'Fri Dec  4 20:13:46 2009'
        >>> Progress.get_installed(b)
        ([], [])

        >>> b["LdapServer-19"]["UNINSTALLED"] = 'BROKEN'
        >>> Progress.get_installed(b)
        ([], ['LdapServer'])
        """
        pkg_info = cls.get_installed_uninstalled_times(progress_data)
        pkg_info = cls.strip_version_info(pkg_info)
        installed_package_names = [pkg_name[0] \
                                   for pkg_name in pkg_info["installed"]]
        broken_package_names  = [pkg_name[0] \
                                 for pkg_name in pkg_info["broken_installed"]]
        broken_package_names += [pkg_name[0] \
                                 for pkg_name in pkg_info["broken_uninstalled"]]
        return installed_package_names, broken_package_names

    @classmethod
    def strip_version(cls, package_name):
        """removes the -\d version number from a package name
        >>> Progress.strip_version("abc")
        'abc'
        >>> Progress.strip_version("abc-2")
        'abc'
        >>> Progress.strip_version("abc-2834343-2")
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

    @classmethod
    def strip_version_from_keys(cls, progress_data):
        """removes the "-\d" version information from the progress info
        >>> b = {'LdapServer-19': {'UNINSTALLED': 'NA', 
        ...                        'VERIFIED': 'Fri Dec  4 17:13:46 2009',
        ...                        'DEPENDENCY_ERRORS': [],
        ...                        'INSTALLED': 'Fri Dec  4 17:13:46 2009'}}
        >>> Progress.strip_version_from_keys(b)
        {'LdapServer': {'UNINSTALLED': 'NA', 'VERIFIED': 'Fri Dec  4 17:13:46 2009', 'DEPENDENCY_ERRORS': [], 'INSTALLED': 'Fri Dec  4 17:13:46 2009'}}
        """
        output = {}
        for key in progress_data.keys():
            output[cls.strip_version(key)] = progress_data[key]
        return output

    @classmethod
    def determine_install_status(cls, item, progress_data):
        '''Goes through the progress data that is stored in status.yml,
        and determines which packages are broken-not installed, which are installed,
        which are broken-installed, and which are uninstalled.

        >>> b = {'LdapServer-19': {'UNINSTALLED': 'NA',
        ...                        'VERIFIED': 'Fri Dec  4 17:13:46 2009',
        ...                        'DEPENDENCY_ERRORS': [],
        ...                        'INSTALLED': 'Fri Dec  4 17:13:46 2009'}}
        >>> Progress.determine_install_status("LdapServer-19", b)
        (1, 1259975626)

        >>> b["LdapServer-19"]["UNINSTALLED"] = 'Sat Dec  5 17:13:46 2009'
        >>> Progress.determine_install_status("LdapServer-19", b)
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

    @classmethod
    def get_installed_uninstalled_times(cls, progress_data):
        """Provides information about what time installation or uninstallation
        activities have taken place

        >>> b = {'LdapServer-19': {'UNINSTALLED': 'NA',
        ...                        'VERIFIED': 'Fri Dec  4 17:13:46 2009',
        ...                        'DEPENDENCY_ERRORS': [],
        ...                        'INSTALLED': 'Fri Dec  4 17:13:46 2009'}}
        >>> Progress.get_installed_uninstalled_times(b)
        {'broken_uninstalled': [], 'uninstalled': [], 'broken_installed': [], 'installed': [['LdapServer-19', 1259975626]]}
        """
        output = {"installed":[], "uninstalled":[], 
                  "broken_installed":[], "broken_uninstalled":[]}
        for item in progress_data.keys():
            status, last_action = cls.determine_install_status(item, progress_data)
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

    def load_current_progress(self):
        "Loads the status.yml file for this instance"
        try:
            raw_data = open(self.progress_path, 'r').read()
            data = yaml_load(raw_data)
            assert type(data) == type({})
        except Exception:
            raise StatusException(self.progress_path)
        return data

    def update_progress(self, dictionary, overwrite=False):
        "updates package status information"
        return_status = OK
        tmp_path = tempfile.mkstemp()[1]
        data = self.load_current_progress()
        try:
            int_data = integrate(data, dictionary, overwrite)
            dump_string = yaml_dump(int_data)
            file_handle = open(tmp_path, 'w')
            file_handle.write(dump_string)
            file_handle.flush()
            file_handle.close()
            del file_handle
            shutil.copy(tmp_path, self.progress_path)
            #os.unlink(tmp_path)
        except IOError:
            return_status = FAIL
        except StatusException:
            return_status = FAIL
        os.system('bash -c "rm -rf %s"' % tmp_path)
        return return_status

    def get_progress_data(self, strip_version_from_name = False):
        "Obtains just the installation progress information from status.yml"
        data = self.load_current_progress()
        if data.has_key("install-progress"):
            progress_data = data["install-progress"]
            if strip_version_from_name:
                return self.strip_version_from_keys(progress_data)
            return progress_data
        return {}

