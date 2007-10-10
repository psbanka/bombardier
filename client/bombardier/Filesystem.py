#!/cygdrive/c/Python24/python.exe

# Filesystem.py: This module is largely used to wrap functionality
# that interacts with the filesystem in order to better be able to
# test this functionality using mock objects in the MockObjects.py
# class in the TEST directory.

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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

import shutil, os, sys, tarfile, gzip, yaml, re, time, random
import Exceptions, miniUtility, Logger
from staticData import *

def copyDirectory( _src, _dest ):
    try:
        if os.path.isdir(_dest):
            deleteDirectory( _dest )
        if os.path.islink(_dest):
            os.unlink(_dest)
        shutil.copytree(_src, _dest)
    except Exception, e:
        errString = "Error creating %s directory:\n%s" %(_dest, e)
        miniUtility.consoleFail(errString)
            
def copyFile( _src, _dest ):
    try:
        if os.path.isfile( _dest ):
            os.unlink( _dest )
        shutil.copy( _src, _dest )
    except:
        errString = "error copying from %s to %s" %( _src,  _dest ) 
        if Logger != None:
            miniUtility.consoleFail(errString)
        else:
            print errString

def makeDirWritable( dir ):
    WRITABLE_DIRECTORY_MODE = 16895
    os.chmod( dir, WRITABLE_DIRECTORY_MODE )

def makeFileWritable( file ):
    WRITABLE_FILE_MODE = 33206
    os.chmod( file, WRITABLE_FILE_MODE )

def makeWritableRecursive( rootDir ):
    for root, dirs, files in os.walk(rootDir, topdown=False):
        for name in files:
            makeFileWritable(os.path.join(root, name))
        for name in dirs:
            print "%s" %(os.path.join(root, name))
            makeDirWritable(os.path.join(root, name))
        makeDirWritable( root )    

def deleteDirectory( path ):
    if os.path.isdir( path ):
        makeWritableRecursive( path )
        shutil.rmtree( path )

def removeFile( path ):
    if os.path.isfile( path ):
        makeFileWritable(path)
        os.remove( path )

def rmScheduledFile(filename):
    import pywintypes
    try:
        win32api.MoveFileEx(filename, None,
                            win32file.MOVEFILE_DELAY_UNTIL_REBOOT)
    except pywintypes.error, e:
        Logger.error("Cannot remove file: %s (%s)" % (filename, e))

def rmScheduledDir(path):
    for root, dirs, files in os.walk(path):
        for name in files:
            rmScheduledFile(os.path.join(root, name))
        for name in dirs:
            rmScheduledFile(os.path.join(root, name))

def deleteDirectories( deletePaths ):
    reboot = False
    for path in deletePaths:
        if path == "\\" or path.lower() == "c:\\":
            Logger.error("Refusing to delete %s" % path)
            return FAIL
        if os.path.isdir(path):
            Logger.info( "Attempting to remove %s" %(path) )
            try:
                deleteDirectory(path)
            except:
                Logger.info("%s not deletable...scheduling for deletion." % path)
                rmScheduledDir(path)
                reboot = True
    if reboot:
        return REBOOT
    else:
        return OK

class Filesystem:

    """The purpose of this class is to provide an abstraction layer
    around all those activities which would require interaction with
    the operating system or a web service. It is mostly a front for
    the os module. The goal is to be able to replace this thing with a
    mock version of the operating system for testing. -pbanka"""
    def __init__(self):
        self.environ = os.environ
    def open(self, path, mode=None):
        if mode:
            return open(path, mode)
        else:
            return open(path)
    def isfile(self, path):
        if os.path.isfile(path):
            return True
        return False
    def isdir(self, path):
        if os.path.isdir(path):
            return True
        return False
    def getcwd(self):
        return os.getcwd()
    def stat(self, path):
        return os.stat(path)
    def unlink(self, path):
        os.unlink(path)
    def loadYaml(self, filename):
        if not os.path.isfile(filename):
            filename = os.path.join(miniUtility.getSpkgPath(), filename)
            if not os.path.isfile(filename):
                raise Exceptions.NoYamlData, filename
        try:
            data = open(filename, 'r').read()
            return yaml.load(data)
        except:
            raise Exceptions.NoYamlData, filename
    def gzipOpen(self, path):
        return gzip.open(path)
    def tarOpen(self, path, mode):
        return tarfile.open(path, mode)
    def createTar(self, tarFileName, backupPath):
        tar = tarfile.open( tarFileName, "w:gz" )
        for inode in os.listdir( backupPath ):
            fullPath = os.path.join( backupPath, inode )
            tar.add( fullPath )
        tar.close()
    def listdir(self, path):
        return os.listdir(path)
    def mkdir(self, path):
        os.makedirs(path)

    def getAllFromFile(self, regex, filename):
        if not os.path.isfile(filename):
            return []
        lines = open(filename, 'r').readlines()
        c = re.compile(regex)
        output = []
        for line in lines:
            m = c.match(line.strip())
            if m:
                output.append(line.strip())
        return output

    def getBinaryDataFromFilePath( self, filePath ):
        return( self.getDataFromFilePath( filePath, "rb" ) )

    def getStringDataFromFilePath( self, filePath ):
        return( self.getDataFromFilePath( filePath, "r" ) )

    def getDataFromFilePath( self, filePath, readMethodString ):
        try:
            filePointer =  open( filePath, readMethodString )
        except TypeError, e:
            Logger.error("GETDATAFROMFILE: Unable to open file %s / %s" % (filePath, readMethodString))
            Logger.error(`e`)
            return ''
        data = self.getDataFromFilePointer( filePointer )
        return data

    def getDataFromFilePointer( self, filePointer ):
        if( filePointer ):
            data = filePointer.read()
            Logger.info("Read %s bytes" % len(data))
            filePointer.close()
        else:
            Logger.info( "filePointer was None" )
            data = None
        return( data )

    def setLock(self):
        #Logger.info("Setting installation lock.")
        lockPath = os.path.join(miniUtility.getSpkgPath(), INSTALL_LOCK)
        if os.path.isfile(lockPath):
            erstr = "There is another installation currently "\
                    "running. (%s exists)" % lockPath
            Logger.error(erstr)
            return FAIL
        else:
            try:
                open(lockPath, 'w').write("locked")
            except OSError:
                Logger.error("Unable to create the lock file")
                return FAIL
        return OK

    def clearLock(self):
        lockPath = os.path.join(miniUtility.getSpkgPath(), INSTALL_LOCK)
        if os.path.isfile(lockPath):
            try:
                os.unlink(lockPath)
            except OSError:
                Logger.error("Unable to delete lock file (%s)" % lockPath)
                return FAIL
        return OK

    def loadCurrent(self):
        statusPath = os.path.join(miniUtility.getSpkgPath(), STATUS_FILE)
        try:
            rawData = open(statusPath, 'r').read()
            data = yaml.load(rawData)
        except Exception:
            raise Exceptions.StatusException(statusPath)
        return data

    def warningLog(self, message, server):
        self.updateProgress({"warnings": {"%s-%s" % (time.time(), random.randint(1,500)):
                                          message}}, server)
        
    def updateCurrentStatus(self, overall, message, server):
        self.updateProgress({"status": {"overall": overall, "main":message}}, server)

    def updateCurrentAction(self, message, percent, server):
        self.updateProgress({"status": {"action": message, "percentage": percent}}, server)

    def updateTimestampOnly(self, server):
        self.updateProgress({}, server)

    def updateProgress(self, dictionary, server, overwrite=False):
        statusPath = os.path.join(miniUtility.getSpkgPath(), STATUS_FILE)
        tmpPath    = miniUtility.getTmpPath()
        data = self.loadCurrent()
        try:
            intData = miniUtility.integrate(data, dictionary, overwrite)
            yamlString = yaml.dump(intData)
            fh = open(tmpPath, 'w')
            fh.write(yamlString)
            fh.flush()
            fh.close()
            shutil.copy(tmpPath, statusPath)
            os.unlink(tmpPath)
        except IOError, e:
            Logger.warning("Cannot update progress data: %s" % e)
            
    def getCurrentAction(self):
        data = self.loadCurrent()
        if data.get("status"):
            return data["status"].get("action")
        return None

    def append(self, source, dest):
        fin = open(source, "rb")
        fout = open(dest, "ab")
        data = fin.read(BLOCK_SIZE)
        while data:
            fout.write(data)
            data = fin.read(BLOCK_SIZE)
        fin.close()
        fout.flush()
        fout.close()

    def getProgressData(self, stripVersionFromName = False):
        filename = os.path.join(miniUtility.getSpkgPath(), STATUS_FILE)
        if not os.path.isfile(filename):
            return {}
        data = open(filename, 'r').read()
        try:
            retVal = yaml.load(data)
            assert type(retVal) == type({})
        except:
            raise Exceptions.InvalidProgress(data)
        if retVal.has_key("install-progress"):
            progressData = retVal["install-progress"]
            if stripVersionFromName:
                return miniUtility.stripVersionFromKeys(progressData)
            return progressData
        return {}

    def getHostname(self):
        if sys.platform == "linux2":
            hostname = open("/proc/sys/kernel/hostname").read().strip().lower()
        else:
            hostname = self.environ["COMPUTERNAME"].lower()
        return hostname

    def tryPatiently(self, action, verify, errorMessage=None, retries = 100):
        succeeded = True
        while retries:
            if not eval(verify):
                try:
                    eval(action)
                    if not succeeded:
                        Logger.info("Successful trying to %s" % action)
                    succeeded = True
                    break
                except Exception, e:
                    Logger.warning(e)
                    retries -= 1
                    time.sleep(.5)
                    if not errorMessage:
                        Logger.warning("still trying to %s" % action)
                    succeeded = False
            else:
                if not succeeded:
                    Logger.info("Successful trying to %s" % action)
                succeeded = True
                break
        if succeeded: return OK
        return FAIL

    def copyfile(self, source, dest):
        shutil.copyfile(source, dest)
    def rmtree(self, path):
        shutil.rmtree(path)
    def chdir(self, path):
        os.chdir(path)

    def moveToDestination(self, destDir, filename):
        #gc.collect() # File this under the category
        #             # 'windows sucks': can't have open files! - pbanka
        if destDir == '':
            return OK
        if not os.path.isdir(destDir):
            Logger.error("Destination directory %s does not exist" % destDir)
            return FAIL
        elif os.getcwd().upper() != destDir.upper():
            try:
                destPath = os.path.join(destDir, filename)
                status1 = self.tryPatiently ("os.unlink(r'%s')" % destPath,
                                             "os.path.isfile(r'%s') == False" % destPath )
                status2 = self.tryPatiently ("shutil.copy(r'%s', r'%s')" % (filename, destPath),
                                             "os.path.isfile(r'%s') == True" % destPath )
                self.tryPatiently ("os.unlink(r'%s')" % filename,
                                   "os.path.isfile(r'%s') == False" % filename)
                if (status1 or status2) == FAIL:
                    return FAIL
            except IOError, e:
                Logger.error("Problem moving %s to %s" % (filename, destPath))
                Logger.error(e)
                return FAIL
            except OSError, e:
                Logger.error("Problem moving %s to %s" % (filename, destPath))
                Logger.error(e)
                return FAIL          
        return OK
