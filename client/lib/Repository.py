#!/cygdrive/c/Python24/python.exe

# Repository.py: This module is responsible for keeping track of
# packages on the repository, downloading them, extracting them, etc.

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

#from old_static_data import *
from bombardier_core.static_data import OK, FAIL, BLOCK_SIZE
import os, tarfile, shutil, sys
import MetaData
from bombardier_core.mini_utility import getPackagePath, rpartition
from bombardier_core.mini_utility import getSpkgPath, rpartition
from bombardier_core.mini_utility import cygpath, getProgressPath
import md5, yaml, base64
import Exceptions
from bombardier_core.Logger import Logger

# PACKAGE FIELDS
FULL_NAME    = "fullName"



class Repository:

    def __init__(self, filesystem, instance_name, package_data = {}):
        self.filesystem   = filesystem
        self.instance_name = instance_name
        self.package_data  = package_data

    @classmethod
    def computeMd5(cls, filename, checksum):
        if not checksum:
            Logger.warning("No checksum provided for %s. Skipping check." % filename)
            return OK, ''
        mchk = md5.new()
        fileHandle = open(filename, 'rb')
        data = fileHandle.read(BLOCK_SIZE)
        while data:
            mchk.update(data)
            data = fileHandle.read(BLOCK_SIZE)
        fileHandle.flush()
        fileHandle.close()
        del fileHandle # necessary because the file may need to be moved before gc. -pbanka
        computedMd5 = mchk.hexdigest()
        if computedMd5 != checksum:
            return FAIL, computedMd5
        return OK, computedMd5

    @classmethod
    def packageRequest(cls, filename, instance_name, checksum):
        dosPath = getPackagePath(instance_name)
        cygPath = cygpath(dosPath)
        Logger.info("==REQUEST-PACKAGE==:%s:%s" % (filename, cygPath))
        response = sys.stdin.read(3)
        if response.strip() != "OK":
            Logger.error("Received an invalid response from the server")
            raise Exceptions.FileNotFound(filename, "server told us that it didn't have our file.")
        filePath = dosPath +'/'+filename
        if os.path.isfile(filePath):
            status, actualChecksum = cls.computeMd5(filePath, checksum)
            if status != OK:
                errmsg = "MD5Sum did not verify. Expected: (%s), got: (%s)" % (checksum, actualChecksum)
                raise Exceptions.BadPackage(filename, errmsg)
            return filePath
        raise Exceptions.FileNotFound(filePath, "did not receive from the server")

    def checkLocalPackages(self):
        localPackages = []
        packagesPath = getPackagePath(self.instance_name)
        inodes = self.filesystem.glob("%s/*" % packagesPath)
        for inode in inodes: 
            shortName = rpartition(inode, packagesPath+os.path.sep)[-1]
            if self.filesystem.isfile(inode):
                if inode.endswith('.spkg'):
                    localPackages.append(shortName.split(".spkg")[0])
                else:
                    Logger.warning("Unknown file in local repository: %s" % inode)
            elif self.filesystem.isdir(inode):
                localPackages.append(shortName)
            else:
                Logger.warning("Unknown inode in local repository: %s" % shortName)

        fileName = getProgressPath(self.instance_name)
        statusData = self.filesystem.loadYaml(fileName)
        statusData["local-packages"] = localPackages
        self.filesystem.dumpYaml(fileName, statusData)

    # TESTED
    def get_meta_data(self, name):
        if not name in self.package_data:
            raise Exceptions.BadPackage(name,"Package not found in Definitive Software Library." )
        pkgData = self.package_data.get(name)
        return MetaData.MetaData(pkgData)

    def unpack_type5(self, file_path, checksum):
        archive_path = "%s.tar.gz" % file_path
        archive_dir = file_path.rpartition(os.path.sep)[0]
        if not self.filesystem.isfile(archive_path):
            erstr = "No package file: %s." % (archive_path)
            Logger.error(erstr)
            return FAIL
        if sys.platform != 'win32':
            cmd = "cd %s && tar -xzf %s" % (archive_dir, archive_path)
            Logger.debug("Untarring with filesystem command: %s" %cmd)
            if not self.filesystem.system(cmd) == OK:
                return FAIL
            return OK
        if self.unzip_t5(pkgPath, full_package_name, removeSpkg) == FAIL:
            return FAIL
        tar = self.filesystem.tarOpen(pkgPath+".tar", "r")
        tar.errorlevel = 2
        cwd = self.filesystem.getcwd()
        self.filesystem.chdir(package_path)
        for tarinfo in tar:
            try:
                tar.extract(tarinfo)
            except tarfile.ExtractError, e:
                Logger.warning("Error with package %s,%s: "\
                                    "%s" % (full_package_name, tarinfo.name, e))
        tar.close()
        if not self.filesystem.isdir(os.path.join(package_path, full_package_name)):
            Logger.error("Package %s is malformed." % (full_package_name))
            self.filesystem.chdir(cwd)
            return FAIL
        self.filesystem.chdir(cwd)
        self.filesystem.unlink(pkgPath+".tar")
        return OK

    def unzip_t5(self, archive_path):
        Logger.info("Unzipping %s" % archive_path)
        gzipFile = self.filesystem.gzipOpen(archive_path)
        output_filename = archive.path.rpartition('tar.gz')[0]
        outputFile = self.filesystem.open(pkgPath+".tar", 'wb')
        data = '1'
        while data:
            try:
                data = gzipFile.read(BLOCK_SIZE)
            except IOError, e:
                Logger.error("Error Reading %s: %s" % (full_package_name, e.__str__()))
                return FAIL
            except Exception, e:
                Logger.error("Corrupt package: %s (%s)" % (full_package_name, e.__str__()))
                return FAIL
            outputFile.write(data)
        outputFile.close()
        gzipFile.close()
        if removeSpkg:
            self.filesystem.unlink(pkgPath+".spkg")
        return OK


    # TESTED
    def unpack(self, full_package_name, checksum, removeSpkg = True):
        package_path = getPackagePath(self.instance_name)
        pkgPath = os.path.join(package_path, full_package_name)
        if not self.filesystem.isfile(pkgPath+".spkg"):
            erstr = "No package file in %s." % (pkgPath+".spkg")
            Logger.error(erstr)
            return FAIL
        if sys.platform != 'win32':
            cmd = "cd %s && tar -xzf %s.spkg" % (package_path, full_package_name)
            Logger.info("Untarring with filesystem command: %s" %cmd)
            if not self.filesystem.system(cmd) == OK:
                return FAIL
            return OK
        if self.unzip(pkgPath, full_package_name, removeSpkg) == FAIL:
            return FAIL
        tar = self.filesystem.tarOpen(pkgPath+".tar", "r")
        tar.errorlevel = 2
        cwd = self.filesystem.getcwd()
        self.filesystem.chdir(package_path)
        for tarinfo in tar:
            try:
                tar.extract(tarinfo)
            except tarfile.ExtractError, e:
                Logger.warning("Error with package %s,%s: "\
                                    "%s" % (full_package_name, tarinfo.name, e))
        tar.close()
        if not self.filesystem.isdir(os.path.join(package_path, full_package_name)):
            Logger.error("Package %s is malformed." % (full_package_name))
            self.filesystem.chdir(cwd)
            return FAIL
        self.filesystem.chdir(cwd)
        self.filesystem.unlink(pkgPath+".tar")
        return OK

    # TESTED
    def unzip(self, pkgPath, full_package_name, removeSpkg = True):
        Logger.info("Unzipping %s" % full_package_name)
        gzipFile = self.filesystem.gzipOpen(pkgPath+".spkg")
        outputFile = self.filesystem.open(pkgPath+".tar", 'wb')
        data = '1'
        while data:
            try:
                data = gzipFile.read(BLOCK_SIZE)
            except IOError, e:
                Logger.error("Error Reading %s: %s" % (full_package_name, e.__str__()))
                return FAIL
            except Exception, e:
                Logger.error("Corrupt package: %s (%s)" % (full_package_name, e.__str__()))
                return FAIL
            outputFile.write(data)
        outputFile.close()
        gzipFile.close()
        if removeSpkg:
            self.filesystem.unlink(pkgPath+".spkg")
        return OK

    def get_type5_package(self, package_name, injector, scripts, checksum=''):
        base_path = os.path.join(getSpkgPath(), self.instance_name)
        injector_path = os.path.join(base_path, "injector", injector)
        scripts_path = os.path.join(base_path, "scripts", scripts)
        for test_path in [injector_path, scripts_path]:
            if self.filesystem.isdir(test_path):
                continue
            status = self.unpack(test_path)
            if status == OK:
                return OK
            tries -= 1
        raise Exceptions.BadPackage(package_name, "Could not get and unpack.")

    def get_type4_package(self, package_name, tries=3, checksum=''):
        # FIXME: for uninstall, this should find the directory in packages
        package_path = getPackagePath(self.instance_name)
        try:
            full_package_name = self.package_data[package_name]['install'][FULL_NAME]
        except KeyError:
            errmsg = "Package not found in Definitive Software Library."
            Logger.info("packages: (%s)" % " ".join(self.package_data.keys()))
            raise Exceptions.BadPackage(package_name, errmsg)
        if self.filesystem.isdir(os.path.join(package_path, full_package_name)):
            return OK
        while tries:
            status = self.unpack(full_package_name, checksum)
            if status == OK:
                return OK
            tries -= 1
        raise Exceptions.BadPackage(package_name, "Could not get and unpack.")

