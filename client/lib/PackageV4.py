#!/cygdrive/c/Python24/python.exe

# Package.py: This guy is responsible for most of the actual work that
# gets done in Bombardier. It's responsible for getting, extracting,
# installing, verifying, etc. packages from the repository onto the
# local system.

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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

import os, glob, random
import sys

import Spkg
from bombardier_core.mini_utility import evalBoolean, getPackagePath
from Exceptions import BadPackage, FeatureRemovedException, RebootRequiredException
from bombardier_core.Logger import Logger
from bombardier_core.static_data import OK, FAIL, REBOOT
from bombardier_core.static_data import INSTALL, UNINSTALL, CONFIGURE, VERIFY

from Package import Package

class PackageV4(Package):

    """This class provides an abstraction for a downloadable,
    installable, verifiable, and uninstallable thing. Each
    package is held in the repository"""

    def __init__(self, name, repository, config, filesystem,
                 operating_system, instance_name):
        Package.__init__(self, name, repository, config,
                         filesystem, operating_system,
                         instance_name)
        self.full_name    = ''
        self.package_version = 4

    ############################################ PUBLIC METHODS

    def initialize(self):
        Package.initialize(self)
        self._check_meta_data()
        self.checksum = self.meta_data.data['install'].get('md5sum')
        if not self.checksum:
            ermsg = "Package %s does not have a checksum field"\
                    " (not checking)" % (self.name)
            Logger.warning(ermsg)
        if self.meta_data.data['install'].get('md5list'):
            self.checksum = self.meta_data.data['install']['md5list']
        chk = self.meta_data.data["install"].get('console')
        self.console = evalBoolean(chk)
        chk = self.meta_data.data["install"].get('reboot')
        self.reboot = evalBoolean(chk)
        chk = self.meta_data.data.get("package-version")
        if type(chk) == type(1):
            self.package_version = chk


    def get_path(self):
        path = os.path.join(getPackagePath(self.instance_name), 
                            self.full_name)
        return path


    ############################################ PRIVATE METHODS

    def _eval_priority(self):
        self.priority = self.meta_data.data["install"].get('priority')
        Package._eval_priority(self)

    def _check_meta_data(self):
        self.status = FAIL
        if self.meta_data.data:
            if type(self.meta_data.data) == type(dict()):
                if self.meta_data.data.get("install"):
                    if type(self.meta_data.data["install"]) == type({}):
                        self.full_name = self.meta_data.data['install'].get('fullName')
                        if not self.full_name:
                            self.full_name = self.meta_data.data['install'].get('full_name')
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


    def _initialize_from_filesystem(self):
        """ Expects a standard package to be extracted in the packages
        directory """
        package_path = getPackagePath(self.instance_name)
        if not self.full_name:
            raise BadPackage(self.name, "Could not find full name.")
        new_dir = os.path.join(package_path, self.full_name)
        self.scripts_dir = os.path.join(new_dir, "scripts")
        if self.package_version > 3:
            self.maint_dir = os.path.join(new_dir, "maint")
        else:
            self.maint_dir = os.path.join(self.scripts_dir, "maint")
        if not self.filesystem.isdir(self.scripts_dir):
            errmsg = "Scripts directory does not exist"
            raise BadPackage(self.name, errmsg)
        injector_dir = os.path.join(new_dir, "injector")
        if self.filesystem.isdir(injector_dir):
            self.working_dir = injector_dir
        else:
            errmsg = "The injector directory does not exist for [%s]" % self.full_name
            raise BadPackage(self.name, errmsg)

    def _find_cmd(self, action, package_list=[], dry_run=False):
        cwd = self.filesystem.getcwd()
        sys.path.insert(0, self.scripts_dir)
        self.filesystem.chdir(self.scripts_dir)
        files = glob.glob("*.py")
        files = [x.split('.py')[0] for x in files]
        if self.name in files:
            files = [self.name]
        else:
            vpkg_name = self.meta_data.data.get('virtualpackage')
            if vpkg_name in files:
                files = [vpkg_name]
        status = FAIL
        file_found = False
        for file_name in files:  # FIXME this is stupid
            if file_name.split('.')[0] in ["installer", "verify", "uninstaller", "configure"]:
                continue
            try:
                obj = Spkg.SpkgV4(self.config, Logger)
                self.filesystem.chdir(self.working_dir)
                letters = [ chr( x ) for x in range(65, 91) ]
                random.shuffle(letters)
                rand_string = ''.join(letters)
                exec("import %s as %s" % (file_name, rand_string)) # FIXME
                Logger.debug("This is package version %s" % self.package_version)
                if self.package_version == 2:
                    exec("obj = %s.%s(self.config)" % (rand_string, file_name))
                elif self.package_version == 3:
                    self.config["__INSTANCE__"] = self.instance_name
                    cmd_str = "obj = %s.%s(self.config, package_list, Logger)"
                    exec(cmd_str % (rand_string, file_name))
                elif self.package_version == 4:
                    self.config["__FUTURE_PACKAGES__"] = package_list
                    self.config["__INSTANCE__"] = self.instance_name
                    cmd_str = "obj = %s.%s(self.config, Logger)"
                    exec(cmd_str % (rand_string, file_name))
                else:
                    raise BadPackage( self.name, "Unknown package version %s" % self.package_version )
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
                Logger.debug("File %s is not runnable. Looking for others" % file_name)
                continue
            except SystemExit, err:
                if err.code:
                    status = err.code
                else:
                    status = 0
                file_found = True
                del rand_string
                break
            except KeyboardInterrupt:
                Logger.error("Keyboard interrupt detected. Exiting...")
                sys.exit(10)
            except SyntaxError, err:
                self._dump_error(err, file_name)
                file_found = False
                del rand_string
                break
            except StandardError, err:
                self._dump_error(err, file_name)
                file_found = True
                status = FAIL
                del rand_string
                break
        self.filesystem.chdir(cwd)
        if not file_found:
            raise BadPackage(self.name, "Unable to find a suitable script to install.")
        if status == None:
            status = OK
        if status == REBOOT:
            raise RebootRequiredException(self.name)
        if status != OK:
            erstr = "%s: failed with status %s" % (self.full_name, status)
            Logger.error(erstr)
            return FAIL
        return OK

    def _get_package(self, tries=3):
        package_path = getPackagePath(self.instance_name)
        if self.filesystem.isdir(os.path.join(package_path, self.full_name)):
            return OK
        while tries:
            status = self.repository.unpack(self.full_name, self.checksum)
            if status == OK:
                return OK
            tries -= 1
        raise BadPackage(self.name, "Could not get and unpack.")

    def _download(self):
        if not self.downloaded:
            try:
                self._get_package()
                self._initialize_from_filesystem()
                self.downloaded = True
            except BadPackage, bpe:
                Logger.error("download INVLIDATION")
                self._invalidate(bpe)
                return FAIL
        return OK

