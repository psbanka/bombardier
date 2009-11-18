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

import os, time
import sys, StringIO, traceback

import MetaData
from bombardier_core.mini_utility import getPackagePath, getSpkgPath
from Exceptions import BadPackage, FeatureRemovedException
from threading import Thread
from bombardier_core.Logger import Logger
from bombardier_core.static_data import OK, FAIL, AVERAGE
from bombardier_core.static_data import INSTALL, UNINSTALL, CONFIGURE, VERIFY

class JobThread(Thread):
    'Runs an action on a machine in a separate thread'

    def __init__(self, import_string, cmd, config):
        '''
        import_string -- python code which will be exec'd to import the
                         package class
        cmd -- python code which will be exec'd which will run the actual
               action on this machine.
        config -- the configuration of this machine
        '''
        Thread.__init__(self)
        self.import_string = import_string
        self.cmd = cmd
        self.config = config
        self.cmd_status = None

    def run(self):
        'Thread interface'
        Logger.debug("Running %s..." % self.cmd)
        try:
            exec(self.import_string)
            exec("self.cmd_status = %s" % self.cmd)
        except StandardError, err:
            Logger.error("Failed to run %s (%s)" % (self.cmd, err))
            sio = StringIO.StringIO()
            traceback.print_exc(file=sio)
            sio.seek(0)
            data = sio.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            Logger.error(ermsg)
            self.cmd_status = FAIL

class Job:
    'Controls the execution of a thread'

    def __init__(self, import_string, cmd, config):
        '''
        import_string -- python code which will be exec'd to import the
                         package class
        cmd -- python code which will be exec'd which will run the actual
               action on this machine.
        config -- the configuration of this machine
        '''
        self.import_string = import_string
        self.cmd = cmd
        self.config = config
        self.start_time = None
        self.job_thread = None
        self.job_status = None

    def is_running(self):
        'ability to check if the job is finished'
        if self.job_thread:
            if self.job_thread.isAlive():
                return True
            if type(self.job_thread.cmd_status) == type(0) \
               or type(self.job_thread.cmd_status) == type('str'):
                Logger.debug("-- status: %s" % (self.job_thread.cmd_status))
                self.job_status = self.job_thread.cmd_status
            else:
                msg = "Invalid return status (type: %s)"
                Logger.error(msg % (type(self.job_thread.cmd_status)))
                self.job_status = FAIL
        return False

    def execute(self):
        'starts a job and waits for it to finish'
        self.start_time = time.time()
        status = FAIL
        if self.is_running():
            msg = "Refusing to run %s; it is already running" % self.cmd
            Logger.error(msg)
            return FAIL
        self.job_thread = JobThread(self.import_string, self.cmd, self.config)
        self.job_thread.start()
        Logger.info("Started job...")
        counter = 1
        while self.is_running():
            time.sleep(1)
            counter += 1
            if not counter % 100:
                msg = "Waiting for completion (%s)..."
                Logger.info(msg % time.strftime(time.ctime()))
                counter = 1
        status = self.job_thread.cmd_status
        return status

    def kill_thread(self, timeout=5):
        'Aborts a job'
        if not self.is_running():
            return OK
        self.job_thread.join(timeout)
        return OK

class Package:

    """This class provides an abstraction for a downloadable,
    installable, verifiable, and uninstallable thing. Each
    package is held in the repository"""

    def __init__(self, name, repository, config, filesystem,
                 instance_name):
        '''
        name -- name of the package
        repository -- object that manages files on the disk for the package
        config -- configuration of the machine
        filesystem -- object that interfaces with the filesystem
        instance_name -- name of this machine
        '''
        self.name         = name
        self.repository   = repository
        self.filesystem   = filesystem
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
        self.status = self._find_cmd(UNINSTALL, [], dry_run=dry_run)
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
        output_path = os.path.join(getSpkgPath(), self.instance_name,
                                  "output", "%s-output.yml" % script_name)
        self.filesystem.rmScheduledFile(output_path)
        message = "Executing (%s) inside package (%s)"
        Logger.info(message % (script_name, self.full_name))
        script_path = "%s/%s.py" % (self.maint_dir, script_name)
        start_dir = os.getcwd()
        os.chdir(self.maint_dir)
        if not os.path.isfile(script_path):
            msg = "%s does not exist" % script_path
            raise BadPackage, (self.name, msg)
        sys.path.append(self.maint_dir )
        import_string = 'import %s' % script_name
        cmd = 'status = %s.execute(self.config, Logger)' % script_name
        job = Job(import_string, cmd, self.config)
        status = job.execute()
        sys.path.remove(self.maint_dir)
        os.chdir(start_dir)
        if status == None:
            status = OK
        if type(status) != type(1):
            msg = "Invalid status type (%s: '%s')"
            Logger.warning(msg % (type(status), status))
            status = FAIL
        return status

    def get_path(self):
        'find place on the disk where this package can be accessed'
        path = os.path.join(getPackagePath(self.instance_name), 
                            self.full_name)
        return path

    ############################################ PRIVATE METHODS

    def _download(self):
        'Virtual method'
        pass

    def _find_cmd(self):
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
            ermsg = "Package %s does not have a priority "\
                    "(assuming %s)" % (self.name, AVERAGE)
            Logger.warning(ermsg)
            self.priority = AVERAGE
        else:
            try:
                self.priority = int(self.priority)
            except ValueError:
                ermsg = "Package %s has an invalid priority value"\
                        "(assuming %s)" % (self.name, AVERAGE)
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
    
    def _dump_error(self, err, file_name):
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
        self.status = self._find_cmd(INSTALL, pkns, dry_run=dry_run)
        msg = "Install result for %s%s : %s"
        Logger.info(msg % (self.full_name, dry_run_string, self.status))
        return self.status

    def _write_progress(self):
        '''Write out information about the state of the packages on this
        system. Keeps track of when packages were installed, uninstalled,
        verified, and keeps track of dependency errors.'''
        time_string = time.ctime()
        pdat = self.filesystem.getProgressData(self.instance_name)
        if not pdat.has_key(self.full_name):
            pdat[self.full_name] = {"INSTALLED": "NA",
                                             "UNINSTALLED": "NA",
                                             "VERIFIED": "NA",
                                             "DEPENDENCY_ERRORS": []}
        if self.action == INSTALL:
            if pdat.get(self.full_name) == None:
                pdat[self.full_name] = {}
            if self.full_name:
                if self.status == OK:
                    pdat[self.full_name]['INSTALLED']   = time_string
                    # FIXME: shouldn't this next line be 'NA'? -- pbanka
                    pdat[self.full_name]['VERIFIED']    = time_string 
                    pdat[self.full_name]['UNINSTALLED'] = 'NA'

                else:
                    pdat[self.full_name]['INSTALLED']   = "BROKEN"
                    pdat[self.full_name]['VERIFIED']    = 'NA'
                    pdat[self.full_name]['UNINSTALLED'] = 'NA'
            else:
                Logger.error("Unnamed package installed: (%s)" % (self.name))
                return FAIL
    
        elif self.action == UNINSTALL:
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
        self.filesystem.updateProgress({"install-progress":pdat},
                                       self.instance_name, overwrite=True)
        return OK
