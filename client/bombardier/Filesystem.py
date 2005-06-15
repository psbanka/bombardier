#!/cygdrive/c/Python/python.exe

import shutil, os, sys, tarfile, gzip, yaml, re, time
import gc
import Exceptions
import miniUtility
from staticData import *

### TESTED
def stripVersion(packageFile):
    if packageFile.rfind('-') == -1:
        return packageFile
    ending = packageFile[packageFile.rfind('-')+1:]
    validator = re.compile("([0-9]+)")
    if validator.search(ending):
        if validator.search(ending).groups()[0] == ending:
            packageFile = packageFile[:packageFile.rfind('-')]
    return packageFile

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
            return yaml.loadFile(filename).next()
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
        os.mkdir(path)
    def execute(self, cmd, errorString="", debug=0, dieOnExit=False, logger=None,
                workingDirectory = '.', captureOutput=False):
        capture = ''
        if captureOutput:
            capture = " 2> result1.txt > result2.txt"
        curDir = os.getcwd()
        if workingDirectory != ".":
            os.chdir(workingDirectory)
        if debug: 
            if logger:
                logger.debug("EXECUTING: %s" % cmd)
            else:
                print "EXECUTING: %s" % cmd
        status = os.system(cmd+capture)
        if captureOutput and logger:
            self.catToLog("result1.txt", logger)
            self.catToLog("result2.txt", logger)
        os.chdir(curDir)
        if status != OK:
            ermsg = "Nonzero exit code %s (executing command %s)." % (errorString, cmd)
            if logger:
                logger.error(ermsg)
            else:
                print ermsg
            if dieOnExit == 1:
                sys.exit(1)
        return status
    def catToLog(self, file, logger):
        if not os.path.isfile(file):
            logger.warning("--------------------------------")
            logger.warning("Output file was not created.")
            logger.warning("--------------------------------")
            return
        lines = open(file, 'r').readlines()
        if len(lines) == 0:
            return
        logger.info("---------------------------------------")
        for line in lines:
            logger.info("output:"+line.strip())
        logger.info("---------------------------------------")
    def copyfile(self, source, dest):
        shutil.copyfile(source, dest)
    def rmtree(self, path):
        shutil.rmtree(path)
    def chdir(self, path):
        os.chdir(path)

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

    def getBinaryDataFromFilePath( self, filePath, logger ):
        return( self.getDataFromFilePath( filePath, "rb", logger ) )

    def getStringDataFromFilePath( self, filePath, logger ):
        return( self.getDataFromFilePath( filePath, "r", logger ) )

    def getDataFromFilePath( self, filePath, readMethodString, logger ):
        try:
            filePointer =  open( filePath, readMethodString )
        except TypeError, e:
            logger.error("GETDATAFROMFILE: Unable to open file %s / %s" % (filePath, readMethodString))
            logger.error(`e`)
            return ''
        data = self.getDataFromFilePointer( filePointer, logger )
        return data

    def getDataFromFilePointer( self, filePointer, logger ):
        if( filePointer ):
            data = filePointer.read()
            logger.info("Read %s bytes" % len(data))
            filePointer.close()
        else:
            logger.info( "filePointer was None" )
            data = None
        return( data )

    def setLock(self, logger):
        logger.info("Setting installation lock.")
        lockPath = os.path.join(miniUtility.getSpkgPath(), INSTALL_LOCK)
        if os.path.isfile(lockPath):
            erstr = "There is another installation currently "\
                    "running. (%s exists)" % lockPath
            logger.error(erstr)
            return FAIL
        else:
            try:
                open(lockPath, 'w').write("locked")
            except OSError:
                logger.error("Unable to create the lock file")
                return FAIL
        return OK

    def clearLock(self, logger = None):
        if logger:
            logger.info("Clearing installation lock.")
        lockPath = os.path.join(miniUtility.getSpkgPath(), INSTALL_LOCK)
        if os.path.isfile(lockPath):
            try:
                os.unlink(lockPath)
            except OSError:
                if logger:
                    logger.error("Unable to delete lock file (%s)" % lockPath)
                return FAIL
        return OK

    def loadCurrent(self):
        statusPath = os.path.join(miniUtility.getSpkgPath(), CURRENT_FILE)
        try:
            data = yaml.loadFile(statusPath).next()
        except:
            data = {}
        return data

    def updateCurrentStatus(self, overall, message):
        self.updateProgressFile({"status": {"overall": overall, "main":message}})

    def updateCurrentAction(self, message, percent):
        self.updateProgressFile({"status": {"action": message, "percentage": percent}})

    def getCurrentAction(self):
        data = self.loadCurrent()
        if data.get("status"):
            return data["status"].get("action")
        return None

    def updateDict(self, newdict, olddict):
        for key, value in newdict.iteritems():
            if type(value) == type({}) and olddict.has_key(key):
                olddict[key] = self.updateDict(value, olddict[key])
            elif type(value) == type(["list"]) and olddict.has_key(key):
                olddict[key] = olddict[key] + value
            else:
                olddict[key] = value
        return olddict

    def updateProgressFile(self, dictionary, overwrite=False):
        statusPath = os.path.join(miniUtility.getSpkgPath(), CURRENT_FILE)
        data = self.loadCurrent()
        data["timestamp"] = time.time()
        fh = open(statusPath, 'w')
        if overwrite:
            for key, value in dictionary.iteritems():
                data[key] = value
        else:
            data = self.updateDict(dictionary, data)
        yaml.dumpToFile(fh, data)
        fh.flush()
        fh.close()

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

    ### TESTED
    def getPackagesFromFile(self, filename, stripVersionFromName = False):
        if not os.path.isfile(filename):
            filename = os.path.join(miniUtility.getSpkgPath(), filename)
            if not os.path.isfile(filename):
                return []

        data = self.convertProgressData(open(filename).read())
        try:
          progressData = yaml.load(data).next()
        except:
          print "BAD YAML:",progressData
          return []

        instPkgs = filter(lambda x: progressData[x]['INSTALLED'] != 'NA',
                          progressData.keys())               
        if stripVersionFromName:
          return map(stripVersion, instPkgs)
        else:
          return instPkgs

    def getProgressData(self):
        filename = os.path.join(miniUtility.getSpkgPath(), PROGRESS_FILE)
        if not os.path.isfile(filename):
            return {}

        try:
            retVal = yaml.load(self.convertProgressData(open(filename).read())).next()
        except:
            return {}

        return retVal

    def watchForTermination(self, logger=None, sleepTime = 10.0, timeout = 600, abortIfTold=None):
        start = time.time()
        consoleFile = os.path.join(miniUtility.getSpkgPath(),CONSOLE_MONITOR)
        while True:
            if abortIfTold != None:
                abortIfTold()
            elapsedTime = time.time() - start
            if elapsedTime > timeout:
                if logger:
                    logger.debug("%d seconds have elapsed (max "\
                                 "of %d). Giving up." % (elapsedTime, timeout))
                return FAIL
            if not os.path.isfile(consoleFile):
                return FAIL
            data = open(consoleFile, 'r').read().strip()
            if data:
                if int(data) == OK:
                    return OK
                elif int(data) == FAIL:
                    return FAIL
                elif int(data) == REBOOT:
                    return REBOOT
                logger.error("Invalid status received from log file: %d" % int(data))
                return FAIL
            left = timeout - elapsedTime
            logger.debug("Watching status of console installation (%3.1f)" % left)
            time.sleep(sleepTime)
        return FAIL

    def beginConsole(self, logger): 
        logger.info("Beginning monitored console-based installation")
        consoleFile = os.path.join(miniUtility.getSpkgPath(),CONSOLE_MONITOR)
        f = open(consoleFile, 'w')
        f.close()

    def convertProgressData(self, data): # Scheduled for demolition
        retVal = {}
        try:
            retVal = yaml.load(data).next()
        except:
            for line in data.strip().split("\n"):
                if re.match(r'[\w=+-]+', line):
                    retVal[line] = {}

        for package in retVal.keys():
            if type(retVal[package]) == type(''):
                if retVal[package] == 'UNINSTALLED':
                    retVal[package] = { 'INSTALLED': 'NA',
                                        'UNINSTALLED': time.ctime() }
                else:
                    retVal[package] = {}
            elif type(retVal[package]) == type([]):
                if retVal[package][0] == 'UNINSTALLED':
                    if retVal[package][1]:
                        retVal[package] = { 'INSTALLED': 'NA',
                                            'UNINSTALLED': retVal[package][1] }
                    else:
                        retVal[package] = { 'INSTALLED': 'NA',
                                            'UNINSTALLED': time.ctime() }
                elif retVal[package][1]:
                    retVal[package] = {'INSTALLED': retVal[package][1] }

            if 'INSTALLED'   not in retVal[package]:
                retVal[package]['INSTALLED']   = time.ctime()
            if 'VERIFIED'    not in retVal[package]:
                retVal[package]['VERIFIED']    = time.ctime()
            if 'UNINSTALLED' not in retVal[package]:
                retVal[package]['UNINSTALLED'] = 'NA'

        return yaml.dump(retVal)

    def tryPatiently(self, action, verify, logger, errorMessage=None, retries = 100):
        succeeded = True
        while retries:
            if not eval(verify):
                try:
                    eval(action)
                    if not succeeded:
                        logger.info("Successful trying to %s" % action)
                    succeeded = True
                    break
                except Exception, e:
                    logger.warning(e)
                    retries -= 1
                    time.sleep(.5)
                    if not errorMessage:
                        logger.warning("still trying to %s" % action)
                    succeeded = False
            else:
                if not succeeded:
                    logger.info("Successful trying to %s" % action)
                succeeded = True
                break
        if succeeded: return OK
        return FAIL

    def moveToDestination(self, destDir, logger, filename):
        gc.collect() # File this under the category
                     # 'windows sucks': can't have open files! - pbanka
        if destDir == '':
            return OK
        if not os.path.isdir(destDir):
            if logger: logger.error("Destination directory %s does not exist" % destDir)
            return FAIL
        elif os.getcwd().upper() != destDir.upper():
            try:
                destPath = os.path.join(destDir, filename)
                status1 = self.tryPatiently ("os.unlink(r'%s')" % destPath,
                                             "os.path.isfile(r'%s') == False" % destPath, logger)
                status2 = self.tryPatiently ("shutil.copy(r'%s', r'%s')" % (filename, destPath),
                                             "os.path.isfile(r'%s') == True" % destPath, logger)
                self.tryPatiently ("os.unlink(r'%s')" % filename,
                                   "os.path.isfile(r'%s') == False" % filename, logger)
                if (status1 or status2) == FAIL:
                    return FAIL
            except IOError, e:
                if logger:
                    logger.error("Problem moving %s to %s" % (filename, destPath))
                    logger.error(e)
                return FAIL
            except OSError, e:
                if logger:
                    logger.error("Problem moving %s to %s" % (filename, destPath))
                    logger.error(e)
                return FAIL          
        return OK
