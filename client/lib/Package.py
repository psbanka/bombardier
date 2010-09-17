#!/usr/bin/python

'Abstract class that implements a package'

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

import yaml
import os, time, stat
import tempfile
import StringIO, traceback

import MetaData
from bombardier_core.mini_utility import get_package_path, get_spkg_path
from bombardier_core.mini_utility import rpartition
from bombardier_core.Progress import Progress

from Exceptions import BadPackage, FeatureRemovedException
from bombardier_core.Logger import Logger
from bombardier_core.static_data import OK, FAIL
from bombardier_core.static_data import INSTALL, UNINSTALL, CONFIGURE, VERIFY
from bombardier_core.static_data import BACKUP

from bombardier_core.PluggableFileProcessor import COMPRESS, SPLIT, ENCRYPT
from bombardier_core.PluggableFileProcessor import ForwardPluggableFileProcessor


AVERAGE = 100

class Package:

    """This class provides an abstraction for a downloadable,
    installable, verifiable, and uninstallable thing. Each
    package is held in the repository"""

    def __init__(self, name, repository, config, instance_name):
        '''
        name -- name of the package
        repository -- object that manages files on the disk for the package
        config -- configuration of the machine
        instance_name -- name of this machine
        '''
        self.name         = name
        self.repository   = repository
        self.instance_name = instance_name
        self.action       = INSTALL
        self.installed    = False
        self.dependencies = []
        self.depends_on_me = []
        self.console      = False
        self.reboot       = False
        self.package_version = 0
        self.checksum     = ''
        self.config       = config
        self.status       = OK
        self.priority     = AVERAGE
        self.meta_data    = MetaData.MetaData({})
        self.working_dir  = ''
        self.scripts_dir  = ''
        self.maint_dir    = ''
        self.downloaded   = False
        self.dependency_errors = []
        self.full_name    = None
        self.progress     = Progress(instance_name)

    ############################################ PUBLIC METHODS

    def initialize(self):
        '''Evaluate the package meta-data for consistency
        and initialize parameters. Verify that data on the disk for
        this package is consistent.
        '''
        try:
            self.meta_data = self.repository.get_meta_data(self.name)
        except BadPackage, bpe:
            Logger.error("initialize INVALIDATING")
            self._invalidate(bpe)
            return
        self._gather_dependencies()
        self._eval_priority()

    def get_configuration(self):
        'getter method'
        return self.meta_data.data.get("configuration", {})

    def get_priority(self):
        'getter method'
        return self.priority

    def add_dependency_error(self, dependency_name):
        '''
        Record the fact that the system has a dependency error.
        This is used when uninstalling packages later.
        dependency_name -- name of the dependency that is missing
        '''
        errmsg = "BOM file is incomplete: should contain %s" % dependency_name
        Logger.warning(errmsg)
        self.dependency_errors.append(dependency_name)

    def install_and_verify(self, pkns, dry_run=False):
        '''
        Perform an installation and verification operation
        pkns -- list of package names
        dry_run -- whether to actually perform the action
        '''
        self._download()
        if self.action == INSTALL:
            status = self._install(pkns, dry_run=dry_run)
            if not dry_run:
                if status == OK:
                    status = self.verify()
                self._write_progress()
            return status
        raise FeatureRemovedException(self.action)

    def configure(self, dry_run=False):
        '''
        Perform a configuration action on this package
        dry_run -- whether to actually perform the action
        '''
        self._download()
        return self._find_cmd(CONFIGURE, [], dry_run)

    def verify(self, dry_run=False): 
        '''
        Perform a verification action on this package
        dry_run -- whether to actually perform the action
        '''
        self._download()
        message = "Verifying package %s" % self.full_name
        Logger.info(message)
        self.status = self._find_cmd(VERIFY, [], dry_run)
        if self.action != INSTALL:
            self._write_progress()
        Logger.info("Verify result for %s : %s" % (self.full_name, self.status))
        return self.status

    def uninstall(self, dry_run=False):
        '''
        Perform an uninstall action on this package
        dry_run -- whether to actually perform the action
        '''
        self.action = UNINSTALL
        self._download()
        if self.status != OK:
            return self.status
        dry_run_string = ""
        if dry_run:
            dry_run_string = " --DRY_RUN-- "
        Logger.info("Uninstalling package %s%s" % (self.name, dry_run_string))
        self.status = self._find_cmd(UNINSTALL, [], dry_run)
        if not dry_run:
            self._write_progress()
        msg = "Uninstall result for %s%s : %s"
        Logger.info(msg % (self.full_name, dry_run_string, self.status))
        return self.status

    def execute_maint_script(self, script_name):
        '''
        execute a user-defined function
        script_name -- name of the function to run
        '''
        self._download()
        # remove old history
        output_path = os.path.join(get_spkg_path(), self.instance_name,
                                  "output", "%s-output.yml" % script_name)
        if os.path.isfile(output_path):
            os.unlink(output_path)
        message = "Executing (%s) inside package (%s)"
        Logger.info(message % (script_name, self.full_name))

    def get_path(self):
        'find place on the disk where this package can be accessed'
        path = os.path.join(get_package_path(self.instance_name), 
                            self.full_name)
        return path

    ############################################ PRIVATE METHODS

    def _download(self):
        'Virtual method'
        pass

    def _find_cmd(self, action, pkns, dry_run):
        'Virtual method'
        return FAIL

    def _invalidate(self, exception_object):
        'set the package as not operable'
        erstr = "INVALID PACKAGE: %s" % self.name
        Logger.error(erstr)
        Logger.error(str(exception_object))
        self.status = FAIL

    def _eval_priority(self):
        'determine priority of this package'
        if not self.priority:
            self.priority = AVERAGE
        else:
            try:
                self.priority = int(self.priority)
            except ValueError:
                Logger.warning(ermsg)
                self.priority = AVERAGE
        
    def _gather_dependencies(self):
        'determine what packages must be installed before this package'
        self.dependencies = self.meta_data.data.get("dependencies")
        if type(self.dependencies) == type(None):
            self.dependencies = []
        elif type(self.dependencies) == type('string'):
            self.dependencies = [self.dependencies]
        elif type(self.dependencies) == type([]):
            pass
        else:
            errmsg = "Unsupported dependencies data for %s. "\
                     "Must be a list. [%s] instead."
            Logger.warning(errmsg % (self.name, self.dependencies))
            self.dependencies = []
    
    @classmethod
    def _dump_error(cls, err, file_name):
        '''
        perform a traceback for logging purposes
        err -- an exception object
        file_name -- the module that was running
        '''
        Logger.error("Error detected in %s (%s)." % (file_name, err))
        sio = StringIO.StringIO()
        traceback.print_exc(file=sio)
        sio.seek(0)
        data = sio.read()
        ermsg = ''
        for line in data.split('\n'):
            ermsg += "\n||>>>%s" % line
        Logger.error(ermsg)
        Logger.error("Error ocurred in %s" % file_name)

    def _install(self, pkns, dry_run=False):
        '''
        perform an installation operation
        pkns -- package names to be installed after this one
        dry_run -- whether or not to actuall perform the installation
        '''
        dry_run_string = ""
        if dry_run:
            dry_run_string = " --DRY_RUN-- "
        self._download()
        message = "Beginning installation of (%s)%s"
        Logger.info(message % (self.full_name, dry_run_string))
        self.status = self._find_cmd(INSTALL, pkns, dry_run)
        msg = "Install result for %s%s : %s"
        Logger.info(msg % (self.full_name, dry_run_string, self.status))
        return self.status

    def _write_install(self, pdat, time_string):
        if pdat.get(self.full_name) == None:
            pdat[self.full_name] = {}
        if self.full_name:
            if self.status == OK:
                pdat[self.full_name]['INSTALLED']   = time_string
                pdat[self.full_name]['VERIFIED']    = time_string 
                pdat[self.full_name]['UNINSTALLED'] = 'NA'

            else:
                pdat[self.full_name]['INSTALLED']   = "BROKEN"
                pdat[self.full_name]['VERIFIED']    = 'NA'
                pdat[self.full_name]['UNINSTALLED'] = 'NA'
        else:
            msg = "Package installed that doesn't have a full_name"
            raise BadPackage(self.name, msg)
        return pdat

    def _write_uninstall(self, pdat, time_string):
        like_packages = []
        for pkn in pdat:
            if pkn.startswith(self.name) and pkn != self.full_name:
                like_packages.append(pkn)
        for pkn in like_packages:
            del pdat[pkn]
        if self.status == OK:
            pdat[self.full_name]['UNINSTALLED'] = time_string
            pdat[self.full_name]['INSTALLED']   = 'NA'
        else:
            pdat[self.full_name]['UNINSTALLED'] = "BROKEN"
        return pdat

    def _backup(self, obj, target_dict, future_pkns, dry_run):
        "Perform basic backup functions for a package"

        pre_backup_cmd = target_dict.get("__pre_backup__")
        post_backup_cmd = target_dict.get("__post_backup__")

        if pre_backup_cmd:
            status = self._find_cmd(pre_backup_cmd, future_pkns, dry_run)
            if status != OK:
                erstr = "%s: backup FAILED because pre-backup command failed."
                Logger.error(erstr % self.full_name)
                return FAIL

        backup_dir = tempfile.mkdtemp()
        for target in target_dict:
            if target.startswith("__"):
                continue
            if type(target_dict[target]) != type({}):
                errmsg = "Package %s did not define its backup data correctly."\
                         " '%s' should be a dictionary."
                Logger.error(errmsg % (self.full_name, target_dict[target]))
                return FAIL
            target_dir = os.path.join(backup_dir, target)
            os.system("mkdir -p %s" % target_dir)
            start_time = time.time()
            target_dict[target]["start_time"] = start_time
            file_name = target_dict[target]["file_name"]
            options = target_dict[target].get("options", [COMPRESS])
            fpf = ForwardPluggableFileProcessor(file_name, options, Logger)
            elapsed_time = time.time() - start_time
            target_dict[target]["elapsed_time"] = elapsed_time
            md5_dict = fpf.process_all()
            target_dict[target]["md5"] = md5_dict
            size = os.stat(file_name)[stat.ST_SIZE]
            target_dict[target]["size"] = size
            directory, _dummy, base_file = rpartition(file_name, '/')
            files = md5_dict.keys()
            files.remove(base_file)
            if len(files) != 1:
                target_dict[target]["status"] = FAIL
            else:
                backup_file = os.path.join(directory, files[0])
                cmd = "mv %s %s" % (backup_file, target_dir)
                Logger.info("CMD: %s" % cmd)
                status = os.system(cmd)
                if status == OK:
                    target_dict[target]["status"] = OK
                else:
                    target_dict[target]["status"] = FAIL

        if post_backup_cmd:
            status = self._find_cmd(post_backup_cmd, future_pkns, dry_run)
            if status != OK:
                erstr = "%s: backup FAILED because post-backup command failed."
                Logger.error(erstr % self.full_name)
                return FAIL

        target_dict["__BACKUP_DIR__"] = backup_dir
        yaml_string = yaml.dump(target_dict)
        for line in yaml_string.split('\n'):
            Logger.info("==REPORT==:%s" % line)
        return OK

    def _write_progress(self):
        '''Write out information about the state of the packages on this
        system. Keeps track of when packages were installed, uninstalled,
        verified, and keeps track of dependency errors.'''
        time_string = time.ctime()
        pdat = self.progress.get_progress_data()
        if not pdat.has_key(self.full_name):
            pdat[self.full_name] = {"INSTALLED": "NA",
                                             "UNINSTALLED": "NA",
                                             "VERIFIED": "NA",
                                             "DEPENDENCY_ERRORS": []}
        if self.action == INSTALL:
            pdat = self._write_install(pdat, time_string)
    
        elif self.action == UNINSTALL:
            pdat = self._write_uninstall(pdat, time_string)
            like_packages = []
            for pkn in pdat:
                if pkn.startswith(self.name) and pkn != self.full_name:
                    like_packages.append(pkn)
            for pkn in like_packages:
                del pdat[pkn]
            if self.status == OK:
                pdat[self.full_name]['UNINSTALLED'] = time_string
                pdat[self.full_name]['INSTALLED']   = 'NA'
            else:
                pdat[self.full_name]['UNINSTALLED'] = "BROKEN"

        elif self.action == VERIFY:
            pdat[self.full_name]['VERIFIED'] = time_string
        pdat[self.full_name]['DEPENDENCY_ERRORS'] = self.dependency_errors
        self.progress.update_progress({"install-progress":pdat}, overwrite=True)
        return OK
