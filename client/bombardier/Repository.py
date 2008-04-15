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
from staticData import *
import miniUtility, MetaData, Logger
import md5, yaml, base64
import Exceptions

# PACKAGE FIELDS
FULL_NAME    = "fullName"



class Repository:

    def __init__(self, filesystem, instanceName):
        self.filesystem   = filesystem
        self.instanceName = instanceName
        self.packages     = {}

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
        dosPath = miniUtility.getPackagePath(instanceName)
        cygPath = miniUtility.cygpath(dosPath)
        Logger.info("==REQUEST-PACKAGE==:%s:%s" % (filename, cygPath))
        response = sys.stdin.read(3)
        if response.strip() != "OK":
            Logger.error("Received an invalid response from the server")
            raise Exceptions.FileNotFound(filename, "server told us that it didn't have our file.")
        filepath = dosPath +'/'+filename
        if os.path.isfile(filepath):
            status, actualChecksum = cls.computeMd5(filepath, checksum)
            if status != OK:
                errmsg = "MD5Sum did not verify. Expected: (%s), got: (%s)" % (checksum, actualChecksum)
                raise Exceptions.BadPackage(filename, errmsg)
        raise Exceptions.FileNotFound(filepath, "did not receive from the server")

    @classmethod
    def packageInfoRequest(cls, packageName):
        config = cls.dataRequest("==REQUEST-PKGINFO==:%s" % (packageName))
        return config

    @classmethod
    def configRequest(cls):
        config = cls.dataRequest("==REQUEST-CONFIG==")
        return config

    @classmethod
    def dataRequest(cls, requestString):
        import zlib
        Logger.info(requestString)
        STREAM_BLOCK_SIZE= 77
        b64Data = []
        while True:
            chunk = sys.stdin.read(STREAM_BLOCK_SIZE)
            if not chunk or chunk[0] == ' ':
                break
            b64Data.append(chunk)
        yamlData = ''
        yamlData = zlib.decompress(base64.decodestring(''.join(b64Data)))
        Logger.debug("Received %s lines of yaml" % len(yamlData.split('\n')))

        try:
            config = yaml.load(yamlData)
        except:
            ermsg = "Received bad YAML: %s" % (repr(yamlData))
            raise Exceptions.ServerUnavailable, ("config", ermsg)
        if type(config) == type("string"):
            Logger.error("Invalid Yaml on server: %s" % config)
            raise Exceptions.ServerUnavailable, ("config", "invalid yaml")
        if type(config) != type({}) and type(config) != type([]): # backwards comptible yaml
            config = config.next()
        return config

    # TESTED
    def getMetaData(self, name):
        if not name in self.packages:
            self.packages[name] = self.packageInfoRequest(name)
        pkgData = self.packages.get(name)
        return MetaData.MetaData(pkgData)

    # TESTED
    def getAndUnpack(self, fullPackageName, checksum):
        packagePath = miniUtility.getPackagePath(self.instanceName)
        pkgPath = os.path.join(packagePath, fullPackageName)
        try:
            shutil.rmtree(pkgPath)
        except OSError:
            pass
        self.packageRequest(fullPackageName+".spkg", self.instanceName, checksum)
        return self.unpack(pkgPath, fullPackageName, True)

    def unpack(self, pkgPath, fullPackageName, removeSpkg = True):
        packagePath = miniUtility.getPackagePath(self.instanceName)
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
        packagePath = miniUtility.getPackagePath(self.instanceName)
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

