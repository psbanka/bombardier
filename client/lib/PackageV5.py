#!/usr/bin/python

"Implements a multi-file package, subclassing the Package class"

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

import os, sys, random

import Spkg
from bombardier_core.mini_utility import getSpkgPath
from bombardier_core.mini_utility import getPackagePath
from Exceptions import BadPackage
from Exceptions import FeatureRemovedException, RebootRequiredException
from bombardier_core.Logger import Logger
from bombardier_core.static_data import OK, FAIL, REBOOT
from bombardier_core.static_data import INSTALL, UNINSTALL, CONFIGURE, VERIFY
from Package import Package

class PackageV5(Package):

    """This class provides an abstraction for a downloadable,
    installable, verifiable, and uninstallable thing. Each
    package is held in the repository"""

    def __init__(self, name, repository, config, filesystem,
                 instance_name):
        Package.__init__(self, name, repository, config,
                         filesystem, instance_name)
        self.package_version = 5
        self.release = None
        self.injectors_info = {}
        self.libs_info = {}
        self.class_name = None
        self.full_name = None

    ############################################ PUBLIC METHODS

    def initialize(self):
        '''Evaluate the package meta-data for consistency
        and initialize parameters. Verify that data on the disk for
        this package is consistent.
        '''
        Package.initialize(self)
        self.status = FAIL
        dat = self.meta_data.data
        if dat:
            release = dat.get('release')
            if type(release) == type(1):
                self.release = release
                class_name = dat.get('class_name')
                if type(class_name) == type('string'):
                    self.status = OK
                    self.class_name = class_name
                    injectors_info = dat.get('injectors')
                    if type(injectors_info) == type({}):
                        self.injectors_info = injectors_info
                    libs_info = dat.get('libs')
                    if type(libs_info) == type({}):
                        self.libs_info = libs_info
                else:
                    msg = "No script infomation for this package"
                    raise BadPackage, (self.name, msg)
            else:
                msg = "No release infomation for this package"
                raise BadPackage, (self.name, msg)
        else:
            msg = "No metadata found for this package"
            raise BadPackage, (self.name, msg)
        self.full_name = "%s-%d" % (self.name, self.release)

    ############################################ PRIVATE METHODS

    def _eval_priority(self):
        'determine priority of this package'
        self.priority = self.meta_data.data.get('priority')
        Package._eval_priority(self)

    def _download(self):
        '''
        Run right before a package action takes place.
        We see if we have a package directory and if not, tell the
        repository to unpack it from the filesystem. Then verify all
        the pieces and parts are there for a good type-4 package
        '''
        if not self.downloaded:
            self.repository.get_type_5(self.full_name, self.injectors_info,
                                       self.libs_info)
            pkg_dir = os.path.join(getPackagePath(self.instance_name),
                                                  self.full_name)
            injector_dir = os.path.join(pkg_dir, "injectors")
            if self.filesystem.isdir(injector_dir):
                self.working_dir = injector_dir
            else:
                self.status = FAIL
                errmsg = "The injector directory does not exist for [%s]"
                raise BadPackage(self.name, errmsg % self.full_name)
        self.downloaded = True

    def _find_cmd(self, action, future_pkns=[], dry_run=False):
        '''
        Perform the action on the system, importing modules from the package
        and running the appropriate method on the class within.
        action -- INSTALL, UNINSTALL, CONFIGURE, VERIFY
        future_pkns -- future package names. Some packages want to know
                       about the packages that will come after them
        dry_run -- boolean flag to see if we're really going to do this
        '''
        package_path = os.path.join(getSpkgPath(), self.instance_name,
                                    "packages", self.full_name)
        lib_path = os.path.join(package_path, "libs")
        sys.path.insert(0, lib_path)

        try:
            class_name = '.'.join(self.class_name.split('.')[1:])
            obj = Spkg.SpkgV4(self.config, Logger)
            cwd = os.getcwd()
            self.filesystem.chdir(self.working_dir)
            letters = [ chr( x ) for x in range(65, 91) ]
            random.shuffle(letters)
            rand_string = ''.join(letters)
            exec("import %s as %s" % (self.class_name, rand_string))
            self.config["__FUTURE_PACKAGES__"] = future_pkns
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

