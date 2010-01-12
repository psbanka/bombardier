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

from bombardier_core import Spkg
import StringIO, traceback
from bombardier_core.mini_utility import get_spkg_path
from bombardier_core.mini_utility import get_package_path
from Exceptions import BadPackage
from Exceptions import RebootRequiredException
from bombardier_core.Logger import Logger
from bombardier_core.static_data import OK, FAIL, REBOOT
from bombardier_core.static_data import ACTION_REVERSE_LOOKUP
from Package import Package

class PackageV5(Package):

    """This class provides an abstraction for a downloadable,
    installable, verifiable, and uninstallable thing. Each
    package is held in the repository"""

    def __init__(self, name, repository, config,
                 instance_name):
        Package.__init__(self, name, repository, config,
                         instance_name)
        self.package_version = 5
        self.release = None
        self.injectors_info = {}
        self.libs_info = {}
        self.class_name = None

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

    def execute_maint_script(self, script_name):
        '''
        execute a user-defined function
        script_name -- name of the function to run
        '''
        Package.execute_maint_script(self, script_name)
        self.status = self._find_cmd(script_name, [], False)
        msg = "%s result for %s : %s"
        Logger.info(msg % (script_name, self.full_name, self.status))
        return self.status

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
            pkg_dir = os.path.join(get_package_path(self.instance_name),
                                                  self.full_name)
            injector_dir = os.path.join(pkg_dir, "injectors")
            if os.path.isdir(injector_dir):
                self.working_dir = injector_dir
            else:
                self.status = FAIL
                errmsg = "The injector directory does not exist for [%s]"
                raise BadPackage(self.name, errmsg % self.full_name)
        self.downloaded = True

    def _get_lib_path(self):
        '''
        Need to modify our path and then clean it out again. This gets
        the data that both operations will need.
        '''
        package_path = os.path.join(get_spkg_path(), self.instance_name,
                                    "packages", self.full_name)
        lib_path = os.path.join(package_path, "libs")
        return lib_path

    def _get_object(self, future_pkns):
        '''
        Import modules from the package in order to run an appropriate method
        on the class within.
        future_pkns -- future package names. Some packages want to know
                       about the packages that will come after them
        '''
        lib_path = self._get_lib_path()
        sys.path.insert(0, lib_path)
        obj = None

        try:
            class_name = '.'.join(self.class_name.split('.')[1:])
            obj = Spkg.SpkgV5(self.config)
            os.chdir(self.working_dir)
            letters = [ chr( x ) for x in range(65, 91) ]
            random.shuffle(letters)
            rand_string = ''.join(letters)
            exec("import %s as %s" % (self.class_name, rand_string))
            self.config["__FUTURE_PACKAGES__"] = future_pkns
            self.config["__INSTANCE__"] = self.instance_name
            cmd_str = "obj = %s.%s(self.config)"
            exec(cmd_str % (rand_string, class_name))
        except ImportError, ie:
            tb_str = StringIO.StringIO()
            traceback.print_exc(file=tb_str)
            tb_str.seek(0)
            data = tb_str.read()
            ermsg = ''
            for line in data.split('\n'):
                Logger.error(line)
            msg = "Class %s is not importable." % self.class_name
            raise BadPackage(self.name, msg)
        return obj, rand_string

    def _cleanup(self, obj):
        '''
        Running a package action is messy business. Here's where we
        attempt to clean up the mess
        '''
        lib_path = self._get_lib_path()
        if self.class_name in sys.modules:
            sys.modules.pop(self.class_name)
        sys.path.remove(lib_path)

    def _find_cmd(self, action, future_pkns, dry_run):
        '''
        Perform the action on the system, importing modules from the package
        and running the appropriate method on the class within.
        action -- INSTALL, UNINSTALL, CONFIGURE, VERIFY
        future_pkns -- future package names. Some packages want to know
                       about the packages that will come after them
        dry_run -- boolean flag to see if we're really going to do this
        '''
        if type(action) == type(1):
            action=ACTION_REVERSE_LOOKUP[action]
        cwd = os.getcwd()
        obj, rand_string = self._get_object(future_pkns)
        try:
            if not hasattr(obj, action):
                msg = "Class %s does not have a %s method."
                raise BadPackage(self.name, msg % (self.class_name, action))
            if not dry_run:
                exec("status = obj.%s()" % action)
            else:
                status = OK
            self._cleanup(obj)
            del rand_string
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
        os.chdir(cwd)
        if status == None:
            status = OK
        if status == REBOOT:
            raise RebootRequiredException(self.name)
        if status != OK:
            erstr = "%s: failed with status %s" % (self.full_name, status)
            Logger.error(erstr)
            return FAIL
        return OK

