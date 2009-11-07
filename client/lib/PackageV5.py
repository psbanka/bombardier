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
from bombardier_core.mini_utility import getSpkgPath, rpartition
from bombardier_core.mini_utility import evalBoolean, getPackagePath
from Exceptions import BadPackage, FeatureRemovedException, RebootRequiredException
from bombardier_core.Logger import Logger
from bombardier_core.static_data import OK, FAIL, REBOOT
from bombardier_core.static_data import INSTALL, UNINSTALL, CONFIGURE, VERIFY
from Package import Package

class PackageV5(Package):

    """This class provides an abstraction for a downloadable,
    installable, verifiable, and uninstallable thing. Each
    package is held in the repository"""

    def __init__(self, name, repository, config, filesystem,
                 operating_system, instance_name):
        Package.__init__(self, name, repository, config,
                         filesystem, operating_system,
                         instance_name)
        self.package_version = 5
        self.release = None
        self.injectors_info = {}
        self.libs_info = {}
        self.script_info = {}
        self.full_name = None

    ############################################ PUBLIC METHODS

    def initialize(self):
        Package.initialize(self)
        self.full_name = "%s-%d" % (self.name, self.release)

    def get_path(self):
        return "/tmp"

    ############################################ PRIVATE METHODS

    def _eval_priority(self):
        self.priority = self.meta_data.data.get('priority')
        Package._eval_priority(self)

    def _check_meta_data(self):
        self.status = FAIL
        if self.meta_data.data:
            release = self.meta_data.data.get('release')
            if type(release) == type(1):
                self.release = release
                script_info = self.meta_data.data.get('script')
                if type(release) == type({}):
                    self.script_info = script_info
                    self.status = OK
                    injectors_info = self.meta_data.data.get('injectors')
                    if type(release) == type({}):
                        self.injectors_info = injectors_info
                    libs_info = self.meta_data.data.get('libs')
                    if type(release) == type({}):
                        self.libs_info = libs_info
                else:
                    msg = "No script infomation for this package"
                    raise BadPackage, (self.name, msg)
                    return
            else:
                msg = "No release infomation for this package"
                raise BadPackage, (self.name, msg)
                return
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

    # ^^^ STOPPED HERE
    def get_package(self):
        base_path = os.path.join(getSpkgPath(), "repos")
        # See if we have to unpack anything
        self.repository.hunt_and_explode()
        # Create directory under /opt/spkg/<instance>/packages/<pkg>-<ver> with symlinks
        package_path = os.path.join(getSpkgPath(), self.instance_name, "packages", self.full_name)
        if not os.path.isdir(package_path):
            Logger.info("Making directory %s" % package_path)
            cmd = "mkdir -p %s" % package_path
            status = os.system(cmd)
            
        raise BadPackage(self.name, "Could not get and unpack.")


    def _download(self):
        if not self.downloaded:
            try:
                self.get_package()
                self._initialize_from_filesystem()
                self.downloaded = True
            except BadPackage, bp:
                Logger.error("download INVLIDATION")
                self._invalidate(bp)
                return FAIL
        return OK

