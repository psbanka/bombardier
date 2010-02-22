#!/usr/bin/python

"Responsible for preparing filesystem for a package's use"

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


from bombardier_core.static_data import OK, FAIL
from bombardier_core.static_data import VALID_PACKAGE_VERSIONS
import os, tarfile, sys, gzip, tarfile
import MetaData
from bombardier_core.mini_utility import get_package_path
from bombardier_core.mini_utility import get_spkg_path
import Exceptions
from bombardier_core.Logger import Logger
from commands import getstatusoutput as gso

BLOCK_SIZE = 10000


class Repository:
    '''Provides support functions for package, dealing with proper
    package file layout, unpacking, and verification'''

    def __init__(self, instance_name, pkg_data = {}):
        '''
        instance_name -- name of this machine
        pkg_data -- data regarding all packages on this machine
        '''
        self.instance_name = instance_name
        self.pkg_data      = pkg_data
        #self.packages      = pkg_data.keys()

    def get_meta_data(self, name):
        '''
        Query DSL database for this machine and return metadata
        for a given package name
        name -- name of the package
        '''
        if not name in self.pkg_data:
            msg = "Not found in %s" % self.pkg_data.keys()
            raise Exceptions.BadPackage(name, msg)
        data = self.pkg_data.get(name)
        return MetaData.MetaData(data)

    def unzip_type_4(self, pkg_path, full_name):
        '''
        Perform untar/ungzip operations. (NOTE: NOT RELIABLE IN Python)
        pkg_path -- full path to package file, minus extension
        full_name -- name of the package, including version
        '''
        Logger.info("Unzipping %s" % full_name)
        gzip_file = gzip.open(pkg_path + ".spkg")
        output_file = open(pkg_path + ".tar", 'wb')
        data = '1'
        while data:
            try:
                data = gzip_file.read(BLOCK_SIZE)
            except IOError, err:
                msg = "Error Reading %s: %s" % (full_name, err.__str__())
                Logger.error(msg)
                return FAIL
            except Exception, err:
                msg = "Corrupt package: %s (%s)" % (full_name, err.__str__())
                Logger.error(msg)
                return FAIL
            output_file.write(data)
        output_file.close()
        gzip_file.close()
        return OK

    def get_type_4(self, full_name):
        '''
        Get a type-4 package from the filesystem, and process it
        full_name -- name of package (with version)
        '''
        pkg_dir = get_package_path(self.instance_name)
        pkg_path = os.path.join(pkg_dir, full_name)
        if not os.path.isfile(pkg_path + ".spkg"):
            erstr = "No package file in %s." % (pkg_path + ".spkg")
            Logger.error(erstr)
            raise Exceptions.BadPackage(full_name, erstr)
        if sys.platform != 'win32':
            cmd = "cd %s && tar -mxzf %s.spkg" % (pkg_dir, full_name)
            Logger.info("Untarring with command: %s" %cmd)
            if not os.system(cmd) == OK:
                raise Exceptions.BadPackage(full_name, "Could not unpack")
            return OK
        if self.unzip_type_4(pkg_path, full_name) == FAIL:
            raise Exceptions.BadPackage(full_name, "could not unzip")
        tar = tarfile.open(pkg_path + ".tar", "r")
        tar.errorlevel = 2
        cwd = os.getcwd()
        os.chdir(pkg_dir)
        for tarinfo in tar:
            try:
                tar.extract(tarinfo)
            except tarfile.ExtractError, err:
                Logger.warning("Error with package %s,%s: "\
                               "%s" % (full_name, tarinfo.name, err))
        tar.close()
        if not os.path.isdir(os.path.join(pkg_path, full_name)):
            erstr = "Package %s is malformed." % (full_name)
            os.chdir(cwd)
            raise Exceptions.BadPackage(full_name, erstr)
        os.chdir(cwd)
        os.unlink(pkg_path + ".tar")
        return OK

    def hunt_and_explode(self):
        '''
        Used to find all type-5 package data in the repository and
        untar the files properly.
        '''
        _status, output = gso('find /opt/spkg/ -name "*.tar.gz"')
        start_dir = os.getcwd()
        for full_tar_file_name in output.split('\n'):
            tmp_list = full_tar_file_name.split(os.path.sep)
            tar_file_name = tmp_list[-1]
            base_name = tar_file_name.split('.tar.gz')[0]
            tar_file_dir = os.path.sep.join(tmp_list[:-1] + [base_name])
            if not os.path.isdir(tar_file_dir):
                Logger.info("Exploding %s..." % base_name)
                cmd = "mkdir -p %s" % tar_file_dir
                status = os.system(cmd)
                if status == OK:
                    cmd = "cd %s && tar -mxzf ../%s"
                    cmd = cmd % (tar_file_dir, tar_file_name)
                    if os.system(cmd) != OK:
                        msg = "could not untar %s" % (tar_file_name)
                        raise Exceptions.BadPackage(full_name, msg)
        os.chdir(start_dir)

    def make_symlinks(self, pkg_path, info_dict, full_name):
        '''
        Create symlinks from the repository into where the package
        expects to run
        pkg_path -- directory where the versioned package will reside
        info_dict -- information regarding injectors and libraries
        full_name -- name of the package, including version info
        '''
        base_path = os.path.join(get_spkg_path(), "repos")
        for component_type in info_dict:
            #Logger.info("Component: %s" % component_type)
            #Logger.info("info_dict: %s" % info_dict)
            component_dict = info_dict[component_type]
            for component_name in component_dict:
                full_path = component_dict[component_name]["path"]
                full_name = full_path.split(os.path.sep)[-1]
                full_name = full_name.split('.tar.gz')[0]
                src = os.path.join(base_path, component_type, full_name)
                dst = os.path.join(pkg_path, component_type, component_name)
                if os.path.islink(dst):
                    continue
                cmd = "ln -s %s %s" % (src, dst)
                #Logger.info("CMD: (%s)" % cmd)
                if os.system(cmd) != OK:
                    msg = "Could not create symlink (%s)" % cmd
                    raise Exceptions.BadPackage(full_name, msg)

    def get_type_5(self, full_name, injectors_info, libs_info):
        '''
        Get type-5 package components from the filesystem, and process them
        full_name -- name of the package, including version info
        injectors_info -- dictionary describing injector libraries
        libs_info -- dictionary describing python code libraries
        '''
        self.hunt_and_explode()
        pkg_path = os.path.join(get_spkg_path(), self.instance_name,
                                "packages", full_name)
        injector_path = os.path.join(pkg_path, "injectors")
        lib_path = os.path.join(pkg_path, "libs")
        if not os.path.isdir(pkg_path):
            Logger.info("Making directory %s" % pkg_path)
            for path in [pkg_path, injector_path, lib_path]:
                cmd = "mkdir -p %s" % path
                if os.system(cmd) != OK:
                    msg = "Could not create directory structure (%s)" % path
                    raise Exceptions.BadPackage(full_name, msg)
        info_dict = {"injectors": injectors_info,
                     "libs": libs_info}
        self.make_symlinks(pkg_path, info_dict, full_name)
        return OK

    def determine_pkg_version(self, pkn):
        'We need to know what version a package is'
        meta_data = self.get_meta_data(pkn)
        version = meta_data.data.get("package-version")
        if type(version) == type(1):
            if version in VALID_PACKAGE_VERSIONS:
                return version
        msg = "Unknown package version"
        raise Exceptions.BadPackage, (pkn, msg)

