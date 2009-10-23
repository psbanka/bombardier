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
from bombardier_core.mini_utility import cygpath, getProgressPath
import md5, yaml, base64
import Exceptions
from bombardier_core.Logger import Logger

# PACKAGE FIELDS
FULL_NAME    = "fullName"



class Repository:

    def __init__(self, filesystem, instanceName, packageData = {}):
        self.filesystem   = filesystem
        self.instanceName = instanceName
        self.packageData  = packageData

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
    def packageRequest(cls, filename, instanceName, checksum):
        dosPath = getPackagePath(instanceName)
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
        packagesPath = getPackagePath(self.instanceName)
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

        fileName = getProgressPath(self.instanceName)
        statusData = self.filesystem.loadYaml(fileName)
        statusData["local-packages"] = localPackages
        self.filesystem.dumpYaml(fileName, statusData)

    # TESTED
    def get_meta_data(self, name):
        if not name in self.packageData:
            raise Exceptions.BadPackage(name,"Package not found in Definitive Software Library." )
        pkgData = self.packageData.get(name)
        return MetaData.MetaData(pkgData)

    # TESTED
    def unpack(self, fullPackageName, checksum, removeSpkg = True):
        packagePath = getPackagePath(self.instanceName)
        pkgPath = os.path.join(packagePath, fullPackageName)
        if not self.filesystem.isfile(pkgPath+".spkg"):
            erstr = "No package file in %s." % (pkgPath+".spkg")
            Logger.error(erstr)
            return FAIL
        if sys.platform != 'win32':
            cmd = "cd %s && tar -xzf %s.spkg" % (packagePath, fullPackageName)
            Logger.info("Untarring with filesystem command: %s" %cmd)
            if not self.filesystem.system(cmd) == OK:
                return FAIL
            return OK
        if self.unzip(pkgPath, fullPackageName, removeSpkg) == FAIL:
            return FAIL
        tar = self.filesystem.tarOpen(pkgPath+".tar", "r")
        tar.errorlevel = 2
        cwd = self.filesystem.getcwd()
        self.filesystem.chdir(packagePath)
        for tarinfo in tar:
            try:
                tar.extract(tarinfo)
            except tarfile.ExtractError, e:
                Logger.warning("Error with package %s,%s: "\
                                    "%s" % (fullPackageName, tarinfo.name, e))
        tar.close()
        if not self.filesystem.isdir(os.path.join(packagePath, fullPackageName)):
            Logger.error("Package %s is malformed." % (fullPackageName))
            self.filesystem.chdir(cwd)
            return FAIL
        self.filesystem.chdir(cwd)
        self.filesystem.unlink(pkgPath+".tar")
        return OK

    # TESTED
    def unzip(self, pkgPath, fullPackageName, removeSpkg = True):
        Logger.info("Unzipping %s" % fullPackageName)
        gzipFile = self.filesystem.gzipOpen(pkgPath+".spkg")
        outputFile = self.filesystem.open(pkgPath+".tar", 'wb')
        data = '1'
        while data:
            try:
                data = gzipFile.read(BLOCK_SIZE)
            except IOError, e:
                Logger.error("Error Reading %s: %s" % (fullPackageName, e.__str__()))
                return FAIL
            except Exception, e:
                Logger.error("Corrupt package: %s (%s)" % (fullPackageName, e.__str__()))
                return FAIL
            outputFile.write(data)
        outputFile.close()
        gzipFile.close()
        if removeSpkg:
            self.filesystem.unlink(pkgPath+".spkg")
        return OK

    # TESTED
    def get_package(self, packageName, tries=3, checksum=''):
        # FIXME: for uninstall, this should find the directory in packages
        packagePath = getPackagePath(self.instanceName)
        try:
            fullPackageName = self.packageData[packageName]['install'][FULL_NAME]
        except KeyError:
            errmsg = "Package not found in Definitive Software Library."
            Logger.info("packages: (%s)" % " ".join(self.packageData.keys()))
            raise Exceptions.BadPackage(packageName, errmsg)
        if self.filesystem.isdir(os.path.join(packagePath, fullPackageName)):
            return OK
        while tries:
            status = self.unpack(fullPackageName, checksum)
            if status == OK:
                return OK
            tries -= 1
        raise Exceptions.BadPackage(packageName, "Could not get and unpack.")

