#!/usr/bin/python

"Implements a single-file package, subclassing the Package class"

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

import os, glob, random
import sys

from bombardier_core import Spkg
from bombardier_core.mini_utility import get_package_path
from Exceptions import BadPackage, FeatureRemovedException
from Job import Job
from Exceptions import RebootRequiredException
from bombardier_core.Logger import Logger
from bombardier_core.static_data import OK, FAIL, REBOOT
from bombardier_core.static_data import INSTALL, UNINSTALL
from bombardier_core.static_data import CONFIGURE, VERIFY

from Package import Package

def eval_boolean(data):
    """Someone has typed in some data in a configuration file that should
    be treated as boolean data. Convert it."""
    if not data:
        return False
    if type(data) != type('string'):
        if type(data) == type(1):
            if data == 1:
                return True
            else:
                return False
        else:
            return False
    data = data.strip().upper()
    if data in ["TRUE", "YES", "1", "OK"]:
        return True
    return False


class PackageV4(Package):

    """This class provides an abstraction for a downloadable,
    installable, verifiable, and uninstallable thing. Each
    package is held in the repository"""

    def __init__(self, name, repository, config, 
                 instance_name):
        '''
        name -- the name of this package
        repository -- object that keeps track of package data
        config -- configuration for this machine
        instance_name -- name of this machine
        '''
        Package.__init__(self, name, repository, config,
                         instance_name)
        self.package_version = 4

    ############################################ PUBLIC METHODS

    def initialize(self):
        '''Evaluate the package meta-data for consistency
        and initialize parameters. Verify that data on the disk for
        this package is consistent.
        '''
        Package.initialize(self)
        self.status = FAIL
        if self.meta_data.data:
            if type(self.meta_data.data) == type(dict()):
                install_data = self.meta_data.data.get("install")
                chk = install_data.get('console')
                self.console = eval_boolean(chk)
                chk = install_data.get('reboot')
                self.reboot = eval_boolean(chk)
                self.checksum = install_data.get('md5sum')
                if install_data:
                    if type(install_data) == type({}):
                        self.full_name = install_data.get('fullName')
                        if not self.full_name:
                            self.full_name = install_data.get('full_name')
                        if self.full_name:
                            self.status = OK
                            return
                        else:
                            msg = "Package does not exist on the server"
                            raise BadPackage, (self.name, msg)
                    else:
                        msg = "The server's record is completely corrupt"
                        raise BadPackage, (self.name, msg)
                else:
                    msg = "server record is corrupt: no 'install' section"
                    raise BadPackage, (self.name, msg)
            else:
                msg = "server record is corrupt: not a dictionary"
                raise BadPackage, (self.name, msg)
        else:
            msg = "No metadata found for this package"
            raise BadPackage, (self.name, msg)

    def execute_maint_script(self, script_name):
        '''
        execute a user-defined function
        script_name -- name of the function to run
        '''
        Package.execute_maint_script(self)
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

    ############################################ PRIVATE METHODS

    def _eval_priority(self):
        'determine priority of this package'
        self.priority = self.meta_data.data["install"].get('priority')
        Package._eval_priority(self)

    def _download(self):
        '''
        Run right before a package action takes place.
        We see if we have a package directory and if not, tell the
        repository to unpack it from the filesystem. Then verify all
        the pieces and parts are there for a good type-4 package
        '''
        if not self.downloaded:
            pkg_dir = os.path.join(get_package_path(self.instance_name),
                                                  self.full_name)
            if not os.path.isdir(pkg_dir):
                self.repository.get_type_4(self.full_name)
            self.scripts_dir = os.path.join(pkg_dir, "scripts")
            self.maint_dir = os.path.join(pkg_dir, "maint")
            injector_dir = os.path.join(pkg_dir, "injector")
            for required_dir in [self.scripts_dir, injector_dir]:
                if not os.path.isdir(required_dir):
                    errmsg = "Required directory %s does not exist"
                    errmsg = errmsg % required_dir
                    self.status = FAIL
                    raise BadPackage(self.name, errmsg)
            self.working_dir = injector_dir
            self.downloaded = True

    def _get_possible_module_files(self):
        '''In type-4 packages, we play a guessing game which python script
        to use as the one to run the installation'''
        files = glob.glob("*.py")
        files = [x.split('.py')[0] for x in files]
        if self.name in files:
            files = [self.name]
        else:
            vpkg_name = self.meta_data.data.get('virtualpackage')
            if vpkg_name in files:
                files = [vpkg_name]
        return files

    def _find_cmd(self, action, future_pkns, dry_run):
        '''
        Perform the action on the system, importing modules from the package
        and running the appropriate method on the class within.
        action -- INSTALL, UNINSTALL, CONFIGURE, VERIFY
        future_pkns -- future package names. Some packages want to know
                       about the packages that will come after them
        dry_run -- boolean flag to see if we're really going to do this
        '''
        cwd = os.getcwd()
        sys.path.insert(0, self.scripts_dir)
        os.chdir(self.scripts_dir)
        files = self._get_possible_module_files()
        status = FAIL
        file_found = False
        for file_name in files:
            try:
                obj = Spkg.SpkgV4(self.config, logger=Logger)
                os.chdir(self.working_dir)
                letters = [ chr( x ) for x in range(65, 91) ]
                random.shuffle(letters)
                rand_string = ''.join(letters)
                exec("import %s as %s" % (file_name, rand_string))
                self.config["__FUTURE_PACKAGES__"] = future_pkns
                self.config["__INSTANCE__"] = self.instance_name
                cmd_str = "obj = %s.%s(self.config, Logger)"
                exec(cmd_str % (rand_string, file_name))
                file_found = True
                if not dry_run:
                    if action == INSTALL:
                        status = obj.installer()
                    elif action == VERIFY:
                        status = obj.verify()
                    elif action == UNINSTALL:
                        status = obj.uninstaller()
                    elif action == CONFIGURE:
                        status = obj.configure()
                    else:
                        raise FeatureRemovedException(action)
                else:
                    status = OK
                del rand_string
                if file_name in sys.modules:
                    sys.modules.pop(file_name)
                while self.scripts_dir in sys.path:
                    sys.path.remove(self.scripts_dir)
                break
            except ImportError:
                msg = "File %s is not runnable. Looking for others" % file_name
                Logger.debug(msg)
                continue
            except SystemExit, err:
                if err.code:
                    status = err.code
                else:
                    status = 0
                file_found = True
                break
            except KeyboardInterrupt:
                Logger.error("Keyboard interrupt detected. Exiting...")
                sys.exit(10)
            except SyntaxError, err:
                self._dump_error(err, file_name)
                file_found = False
                break
            except StandardError, err:
                self._dump_error(err, file_name)
                file_found = True
                status = FAIL
                break
        os.chdir(cwd)
        if not file_found:
            msg = "Unable to find a suitable script to install."
            raise BadPackage(self.name, msg)
        if status == None:
            status = OK
        if status == REBOOT:
            raise RebootRequiredException(self.name)
        if status != OK:
            erstr = "%s: failed with status %s" % (self.full_name, status)
            Logger.error(erstr)
            return FAIL
        return OK

