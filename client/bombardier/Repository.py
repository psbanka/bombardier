#!/cygdrive/c/Python23/python.exe

import os, tarfile, shutil

from staticData import *
import miniUtility, MetaData, Logger

PACKAGE_LIST = "packages.dat"

# PACKAGE FIELDS
FULL_NAME    = "fullName"

class Repository:

    def __init__(self, config, filesystem, server):
        self.config     = config
        self.server     = server
        self.filesystem = filesystem
        self.packages   = {}
        self.status     = self.getPackageData()

    # TESTED
    def getPackageData(self):
        Logger.debug("Downloading package data...")
        self.packages = self.server.serviceYamlRequest("package", args= {"type":"yaml"})

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
    def getAndUnpack(self, fullPackageName, checksum, abortIfTold):
        packagePath = miniUtility.getPackagePath()
        pkgPath = os.path.join(packagePath, fullPackageName)
        try:
            shutil.rmtree(pkgPath)
        except OSError:
            pass
        erstr = "Downloading package %s from "\
                "%s..." % (fullPackageName, self.server.serverData["address"])
        Logger.info(erstr)
        if type(checksum) != type(["list"]):
            status = self.server.wget("deploy", fullPackageName+".spkg",
                                      destDir=miniUtility.getPackagePath(),
                                      checksum=checksum, abortIfTold=abortIfTold)
        else:
            status = self.server.wgetMultiple("deploy", fullPackageName+".spkg", 
                                              destDir=miniUtility.getPackagePath(),
                                              checksumList=checksum, abortIfTold=abortIfTold)
        if status == FAIL:
            return FAIL
        if not self.filesystem.isfile(pkgPath+".spkg"):
            erstr = "No package file in %s." % (pkgPath+".spkg")
            Logger.error(erstr)
            return FAIL
        if self.unzip(fullPackageName, abortIfTold) == FAIL:
            return FAIL
        tar = self.filesystem.tarOpen(pkgPath+".tar", "r")
        tar.errorlevel = 2
        cwd = self.filesystem.getcwd()
        self.filesystem.chdir(packagePath)
        for tarinfo in tar:
            abortIfTold()
            try:
                tar.extract(tarinfo)
            except tarfile.ExtractError, e:
                Logger.warning("Error with package %s,%s: "\
                                    "%s" % (fullPackageName, tarinfo.name, e))
        tar.close()
        packagePath = miniUtility.getPackagePath()
        if not self.filesystem.isdir(os.path.join(packagePath, fullPackageName)):
            Logger.error("Package %s is malformed." % (fullPackageName))
            self.filesystem.chdir(cwd)
            return FAIL
        self.filesystem.chdir(cwd)
        self.filesystem.unlink(pkgPath+".tar")
        return OK

    # TESTED
    def unzip(self, fullPackageName, abortIfTold):
        pkgPath = os.path.join(miniUtility.getPackagePath(), fullPackageName)
        Logger.info("Unzipping %s" % fullPackageName)
        gzipFile = self.filesystem.gzipOpen(pkgPath+".spkg")
        outputFile = self.filesystem.open(pkgPath+".tar", 'wb')
        data = '1'
        while data:
            abortIfTold()
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
        self.filesystem.unlink(pkgPath+".spkg")
        return OK

    # TESTED
    def getPackage(self, packageName, abortIfTold, tries=3, checksum=''):
        # FIXME: for uninstall, this should find the directory in packages
        packagePath = miniUtility.getPackagePath()
        try:
            fullPackageName = self.packages[packageName]['install'][FULL_NAME]
        except KeyError:
            Logger.error("package %s is not in the package database" % packageName)
            return FAIL
        if self.filesystem.isdir(os.path.join(packagePath, fullPackageName)):
            return OK
        while tries:
            status = self.getAndUnpack(fullPackageName, checksum, abortIfTold)
            if status == OK:
                return OK
            tries -= 1
        return FAIL
