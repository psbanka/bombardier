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
        self.class_name = None
        self.full_name = None

    ############################################ PUBLIC METHODS

    def initialize(self):
        Package.initialize(self)
        self._check_meta_data()
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
                class_name = self.meta_data.data.get('class_name')
                if type(class_name) == type('string'):
                    self.status = OK
                    self.class_name = class_name
                    injectors_info = self.meta_data.data.get('injectors')
                    if type(injectors_info) == type({}):
                        self.injectors_info = injectors_info
                    libs_info = self.meta_data.data.get('libs')
                    if type(libs_info) == type({}):
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
        injector_dir = os.path.join(new_dir, "injectors")
        if self.filesystem.isdir(injector_dir):
            self.working_dir = injector_dir
        else:
            errmsg = "The injector directory does not exist for [%s]" % self.full_name
            raise BadPackage(self.name, errmsg)

    def _find_cmd(self, action, package_list=[], dry_run=False):
        package_path = os.path.join(getSpkgPath(), self.instance_name, "packages", self.full_name)
        injector_path = os.path.join(package_path, "injectors")
        lib_path = os.path.join(package_path, "libs")
        sys.path.insert(0, lib_path)

        try:
            module_name = self.class_name.split('.')[0]
            class_name = '.'.join(self.class_name.split('.')[1:])
            obj = Spkg.SpkgV4(self.config, Logger)
            cwd = os.getcwd()
            self.filesystem.chdir(self.working_dir)
            letters = [ chr( x ) for x in range(65, 91) ]
            random.shuffle(letters)
            rand_string = ''.join(letters)
            exec("import %s as %s" % (self.class_name, rand_string))
            self.config["__FUTURE_PACKAGES__"] = package_list
            self.config["__INSTANCE__"] = self.instance_name
            cmd_str = "obj = %s.%s(self.config, Logger)"
            exec(cmd_str % (rand_string, class_name))
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
            if self.class_name in sys.modules:
                sys.modules.pop(self.class_name)
            sys.path.remove(lib_path)
        except ImportError:
            msg = "Class %s is not runnable." % self.class_name
            raise BadPackage(self.name, msg)
        except SystemExit, err:
            if err.code:
                status = err.code
            else:
                status = OK
            del rand_string
        except KeyboardInterrupt:
            Logger.error("Keyboard interrupt detected. Exiting...")
            status = FAIL
            sys.exit(10) # FIXME: Literal
        except SyntaxError, err:
            self._dump_error(err, self.class_name)
            status = FAIL
            del rand_string
        except StandardError, err:
            self._dump_error(err, self.class_name)
            status = FAIL
            del rand_string
        self.filesystem.chdir(cwd)
        if status == None:
            status = OK
        if status == REBOOT:
            raise RebootRequiredException(self.name)
        if status != OK:
            erstr = "%s: failed with status %s" % (self.full_name, status)
            Logger.error(erstr)
            return FAIL
        return OK

    def prepare_dict(self, package_path, package_dict):
        base_path = os.path.join(getSpkgPath(), "repos")
        for component_type in package_dict:
            info_dict = package_dict[component_type]
            for component_name in info_dict:
                full_path = info_dict[component_name]["path"]
                full_name = full_path.split(os.path.sep)[0]
                full_name = full_name.split('.tar.gz')[0]
                src = os.path.join(base_path, component_type, full_name)
                dst = os.path.join(package_path, component_type, component_name)
                cmd = "ln -s %s %s" % (src, dst)
                if os.system(cmd) != OK:
                    msg = "Could not create symlink (%s)" % cmd
                    raise BadPackage(self.name, msg)

    def get_package(self):
        base_path = os.path.join(getSpkgPath(), "repos")
        # See if we have to unpack anything
        self.repository.hunt_and_explode()
        # Create directory under /opt/spkg/<instance>/packages/<pkg>-<ver> with symlinks
        package_path = os.path.join(getSpkgPath(), self.instance_name, "packages", self.full_name)
        injector_path = os.path.join(package_path, "injectors")
        lib_path = os.path.join(package_path, "libs")
        if not os.path.isdir(package_path):
            Logger.info("Making directory %s" % package_path)
            for path in [package_path, injector_path, lib_path]:
                cmd = "mkdir -p %s" % path
                if os.system(cmd) != OK:
                    msg = "Could not create directory structure (%s)" % path
                    raise BadPackage(self.name, msg)
        info_dict = {"injectors": self.injectors_info,
                     "libs": self.libs_info}
        self.prepare_dict(package_path, info_dict)
        return OK

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

