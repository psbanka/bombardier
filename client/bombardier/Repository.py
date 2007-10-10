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

import os, tarfile, shutil
import yaml
from staticData import *
import miniUtility, MetaData, Logger

PACKAGE_DB = "packages.yml"

# PACKAGE FIELDS
FULL_NAME    = "fullName"

class Repository:

    def __init__(self, config, filesystem, server):
        self.config     = config
        self.server     = server
        self.filesystem = filesystem
        self.packages   = {}

    # TESTED
    def getPackageData(self):
        #Logger.debug("Downloading package data...")
        filename = self.server.packageRequest(PACKAGE_DB)
        if filename != '':
            self.packages = yaml.load(open(filename).read())

    # TESTED
    def getFullPackageNames(self):
        output = []
        for packageName in self.packages.keys():
            output.append(self.packages[packageName]['install'][FULL_NAME])
        return output

    # TESTED
    def getMetaData(self, name):
        pkgData = self.packages.get(name)
        return MetaData.MetaData(pkgData)

    # TESTED
    def getAndUnpack(self, fullPackageName, checksum):
        packagePath = miniUtility.getPackagePath()
        pkgPath = os.path.join(packagePath, fullPackageName)
        try:
            shutil.rmtree(pkgPath)
        except OSError:
            pass
        if type(checksum) != type(["list"]):
            filename = self.server.packageRequest(fullPackageName+".spkg")
        return self.unpack(pkgPath, fullPackageName, True)

    def unpack(self, pkgPath, fullPackageName, removeSpkg = True):
        packagePath = miniUtility.getPackagePath()
        if not self.filesystem.isfile(pkgPath+".spkg"):
            erstr = "No package file in %s." % (pkgPath+".spkg")
            Logger.error(erstr)
            return FAIL
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
    def getPackage(self, packageName, tries=3, checksum=''):
        # FIXME: for uninstall, this should find the directory in packages
        packagePath = miniUtility.getPackagePath()
        try:
            fullPackageName = self.packages[packageName]['install'][FULL_NAME]
        except KeyError:
            Logger.error("package %s is not in the package database" % packageName)
            Logger.info("packages: (%s)" % " ".join(self.packages.keys()))
            return FAIL
        if self.filesystem.isdir(os.path.join(packagePath, fullPackageName)):
            return OK
        while tries:
            status = self.getAndUnpack(fullPackageName, checksum)
            if status == OK:
                return OK
            tries -= 1
        return FAIL
