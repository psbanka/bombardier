#!/usr/bin/python
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

import shutil, os, sys, tarfile, gzip, yaml, re, time, glob
import Exceptions, mini_utility
from Logger import Logger
from static_data import OK, FAIL, REBOOT, INSTALL_LOCK, BLOCK_SIZE

class CopyException(Exception):
    def __init__(self, message=""):
        Exception.__init__(self)
        self.message=message
    def __repr__(self):
        return self.message
    def __str__(self):
        return self.__repr__()

def copyDirectory( _src, _dest ):
    try:
        if os.path.isdir(_dest):
            deleteDirectory( _dest )
        if os.path.islink(_dest):
            os.unlink(_dest)
        shutil.copytree(_src, _dest)
    except Exception, e:
        raise CopyException("Error creating %s directory:\n%s" %(_dest, e))

def copyFile( _src, _dest ):
    try:
        if os.path.isfile( _dest ):
            os.unlink( _dest )
        shutil.copy( _src, _dest )
    except:
        raise CopyException("error copying from %s to %s" %( _src,  _dest ))

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

def removeDirectory(path):
    deleteDirectory(path)

def removeFile( path ):
    if os.path.isfile( path ):
        makeFileWritable(path)
        os.remove( path )

def rmScheduledFile(fileName):
    try:
        # first, let's shred it
        status = os.system("shred -f %s" % fileName)
        if status != OK:
            status = os.system("/usr/bin/shred -f %s" % fileName)
            if status != OK:
                Logger.warning("Unable to shred before deleting %s" % fileName)
        if '*' in fileName:
            for fileName in glob.glob(fileName):
                os.unlink(fileName)
        else:
            os.unlink(fileName)
        return OK
    except:
        if sys.platform == "win32":
            import pywintypes
            import win32api, win32file
            try:
                win32api.MoveFileEx(fileName, None,
                                    win32file.MOVEFILE_DELAY_UNTIL_REBOOT)
                return REBOOT
            except pywintypes.error, e:
                Logger.error("Cannot remove file: %s (%s)" % (fileName, e))
        else:
            Logger.error("Cannot remove file: %s (%s)" % (fileName, e))

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

def removeDirectories( deletePaths ):
    return deleteDirectories( deletePaths)

class Filesystem:

    """The purpose of this class is to provide an abstraction layer
    around all those activities which would require interaction with
    the operating system or a web service. It is mostly a front for
    the os module. The goal is to be able to replace this thing with a
    mock version of the operating system for testing. -pbanka"""
    def __init__(self):
        self.environ = os.environ
    def rmScheduledFile(self,fileName):
        try:
            # first, let's shred it
            status = os.system("shred -f %s" % fileName)
            if status != OK:
                status = os.system("/usr/bin/shred -f %s" % fileName)
                if status != OK:
                    Logger.warning("Unable to shred before deleting %s" % fileName)
            os.unlink(fileName)
            return OK
        except:
            if sys.platform == "win32":
                import pywintypes
                import win32api, win32file
                try:
                    win32api.MoveFileEx(fileName, None,
                                        win32file.MOVEFILE_DELAY_UNTIL_REBOOT)
                    return REBOOT
                except pywintypes.error, e:
                    Logger.error("Cannot remove file: %s (%s)" % (fileName, e))
            else:
                return FAIL
    def system(self, command):
        return os.system(command)
    def execute(self, command):
        return os.system(command)
    def glob(self, path):
        return glob.glob(path)
    def attrib(self, fileName, flagString):
        attrib   = os.path.join(os.environ["WINDIR"], "SYSTEM32", "attrib.exe") 
        cmd = "%s %s %s" % (attrib, flagString, fileName)
        status = os.system(cmd)
        return status
    def runCacls(self, filename, account, permission, edit=True, recurse=True): 
        cacls    = os.path.join(os.environ["WINDIR"], "SYSTEM32", "cacls.exe") 
        editFlag = '' 
     
        if edit:  
            editFlag = "/E" 
        recurseFlag = '' 
        if recurse: 
            recurseFlag = '/T'  
        cmd = "echo y| %s %s %s %s /P %s:%s >c:\.cacls.out" % (cacls, filename, recurseFlag, editFlag, account, permission) 
        status = os.system(cmd) 
        return status 
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
    def dumpYaml(self, filename, dictionary):
        open(filename, 'w').write(yaml.dump(dictionary))
    def loadYaml(self, filename):
        if not os.path.isfile(filename):
            filename = os.path.join(mini_utility.getSpkgPath(), filename)
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
    def makedirs(self, path):
        os.makedirs(path)
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
        lockPath = os.path.join(mini_utility.getSpkgPath(), INSTALL_LOCK)
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
        lockPath = os.path.join(mini_utility.getSpkgPath(), INSTALL_LOCK)
        if os.path.isfile(lockPath):
            try:
                os.unlink(lockPath)
            except OSError:
                Logger.error("Unable to delete lock file (%s)" % lockPath)
                return FAIL
        return OK

    def loadCurrent(self, instanceName):
        statusPath = mini_utility.getProgressPath(instanceName)
        try:
            rawData = open(statusPath, 'r').read()
            data = yaml.load(rawData)
        except Exception:
            raise Exceptions.StatusException(statusPath)
        return data

    def updateProgress(self, dictionary, instanceName, overwrite=False):
        statusPath = mini_utility.getProgressPath(instanceName)
        tmpPath    = mini_utility.getTmpPath()
        data = self.loadCurrent(instanceName)
        try:
            intData = mini_utility.integrate(data, dictionary, overwrite)
            yamlString = yaml.dump(intData, default_flow_style=False)
            fh = open(tmpPath, 'w')
            fh.write(yamlString)
            fh.flush()
            fh.close()
            shutil.copy(tmpPath, statusPath)
            os.unlink(tmpPath)
        except IOError, e:
            Logger.warning("Cannot update progress data: %s" % e)

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

    def getProgressData(self, instanceName, stripVersionFromName = False):
        filename = mini_utility.getProgressPath(instanceName)
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
                return mini_utility.strip_version_from_keys(progressData)
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
