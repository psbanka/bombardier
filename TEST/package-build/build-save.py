#!/usr/bin/python2.4

#^ CASE-sensitivity

import os, shutil, sys, md5
import getopt
import pycurl
import yaml
import tarfile
import time
import ConfigParser
import bombardier.Server as Server
import bombardier.Filesystem as Filesystem


from static import *

def getChecksum( fullPath ):
    fp = open( fullPath, 'rb' )
    checkSum = md5.new( fp.read() ).hexdigest()
    fp.close()
    return( checkSum )

class PackageCreationThread:

    def __init__(self, tarFileName, packageName, scriptName, version):
        self.tarFileName = tarFileName
        self.packageName = packageName
        self.scriptName  = scriptName
        self.version     = version

        self.server      = Server.Server(Filesystem.Filesystem(), SERVER_DATA)
        
        self.version == None:
            self.findVersion()
        self.packageName = packageName
        self.fullname = self.packageName+'-'+self.version
        self.spkg = self.packageName+"-"+self.version+".spkg"
        self.destDir = TMP_DIR +'/'+ self.fullname
        self.status   = OK
        self.warnings = []
        self.errors   = []

    def cleanup(self):
        print "cleaning up..."
        try:
            shutil.rmtree(self.destDir)
        except OSError:
            self.warnings.append("Unable to clean up directory %s" % self.destDir)
        
        files = os.listdir( DEPLOY_DIR )
        for inode in files:
            if inode == self.spkg:
                continue
            if inode.endswith(".spkg"):
                if inode.startswith(self.packageName):
                    warning = "You should remove old package "\
                              "%s/%s..." % (DEPLOY_DIR,inode)
                    self.warnings.append(warning)
        return OK

    def metaDataTweak(self, dict):
        return dict

    def createMetaData(self):
        iniPath = os.path.join(self.destDir,'scripts','package.ini')
        if not os.path.isfile(iniPath):
            self.errors.append("%s file does not exist" % iniPath)
            return FAIL
        packagesYml = os.path.join(DEPLOY_DIR, "packages", "packages.yml")
        if not os.path.isfile(packagesYml):
            self.errors.append("%s does not exist. "\
                               "Please re-create it." % packagesYml)
            return FAIL
        output = yaml.loadFile(packagesYml).next()
        d = {}
        configParser = ConfigParser.ConfigParser()
        configParser.read(iniPath)
        for section in configParser.sections():
            d[section] = {}
            for option in configParser.options(section):
                d[section][option] =  configParser.get(section, option)
        d['install']['fullName'] = self.fullname
        filename = "%s/packages/%s.spkg" % (DEPLOY_DIR, self.fullname)
        checksum = getChecksum(filename)
        d['install']['md5sum'] = checksum
        d = self.metaDataTweak(d)
        print "amending packages.yml for %s" % self.packageName
        try:
            output[self.packageName] = d
        except Exception, e:
            self.errors.append("unable to modify metadata database: %s" % e)
            return FAIL
        status = self.writeYamlSecurely(output, packagesYml)
        return status

    def createTarball(self):
        currDir = os.getcwd()
        print "creating spkg (%s)..." % self.fullname
        os.chdir(TMP_DIR)
        tar = tarfile.open(self.spkg, "w:gz")
        tar.add(self.fullname)
        tar.close()
        print "moving into place..."
        shutil.copyfile(self.spkg, os.path.join(DEPLOY_DIR, "packages", self.spkg))
        os.unlink(self.spkg)
        os.chdir(currDir)
        return OK

    def createScripts(self):
        print "exporting install scripts..."
        if not os.path.isdir(self.destDir):
            print "making directory %s" % self.destDir
            os.makedirs(self.destDir)
        cmd = "svn export --username %s --password %s %s/%s "\
              "%s/scripts > output.txt 2>&1" \
              % (SVN_USERNAME, SVN_PASSWORD, SVN_ROOT, self.scriptName, self.destDir)
        print "executing %s" % cmd
        if sys.platform == "win32":
            status = os.system(cmd)
        else:
            status, output = commands.getstatusoutput(cmd)
        if status != OK:
            self.errors.append("error exporting scripts for %s" % self.packageName)
            if os.path.isfile("output.txt"):
                for line in open("output.txt").readlines():
                    self.errors.append("debug: %s" % line.strip())
            return FAIL
        return OK

    def populateInjector(self):
        print  "copying injectors..."
        injectorDir = os.path.join(self.destDir, "injector")
        startDir = os.getcwd()
        os.mkdir(injectorDir)
        os.chdir(injectorDir)
        try:
            tar = tarfile.open(os.path.join(startDir, self.tarFileName), mode="r:gz")
            entries = []
            for tarinfo in tar:
                entries.append(tarinfo)
                try:
                    tar.extract(tarinfo)
                except tarfile.ExtractError, e:
                    self.errors.append("Error with package %s,%s "\
                                       "%s" % (self.fullname, tarinfo.name, e))
            tar.close()
        except tarfile.ReadError, e:
            self.errors.append( "Cannot unzip uploaded file -- %s" % e )
            os.chdir(startDir)
            return FAIL
        except Exception, e: # tarfile's exceptions are lame, like "error" -pbanka
            self.errors.append( "Cannot untar uploaded file -- %s" % e )
            os.chdir(startDir)
            return FAIL
        if len(entries) == 0:
            errmsg = "There were no valid entries in the uploaded tarfile."
            self.warnings.append(errmsg)
        else:
            print "number of files in the tarball: %s" % len(entries)
        os.chdir(startDir)
        return OK

    def prepare(self):
        print "Building %s" % self.spkg
        if os.path.isfile(os.path.join(os.path.join(DEPLOY_DIR, "packages", self.spkg))):
            self.errors.append("file %s already exists in the repository" % self.spkg)
            return FAIL
        print "creating temporary directory %s..." % self.destDir
        if os.path.isdir(self.destDir):
            try:
                shutil.rmtree(self.destDir)
            except OSError:
                self.errors.append("Unable to clean out the temp directory.")
                return FAIL
        os.makedirs(self.destDir)
        return OK

    def sendWrapup(self, status):
        outputYaml = yaml.dump({"fullname":self.fullname,
                                "warnings":self.warnings, "errors":self.errors,
                                "status":status}).split('\n')
        return "\n".join(outputYaml)
                           
    def run(self):
        if self.prepare() == FAIL:
            self.errors.append("Failed in package creation preparation: check server")
            return self.sendWrapup(FAIL)
        if self.createScripts() == FAIL:
            self.errors.append("Failed in preparing the management scripts: check SVN")
            return self.sendWrapup(FAIL)
        if self.populateInjector() == FAIL:
            self.errors.append("Failed in populating the injectors: check tarball")
            return self.sendWrapup(FAIL)
        if self.createTarball() == FAIL:
            self.errors.append("Failed in taring up the .spkg: check the server")
            return self.sendWrapup(FAIL)
        if self.createMetaData() == FAIL:
            self.errors.append("Failed in creating the metadata: check the server")
            return self.sendWrapup(FAIL)
        self.cleanup()
        return self.sendWrapup(OK)

    def verifyYamlData(self, data, filename):
        fh = open(filename, 'r')
        yamldata = yaml.load(fh.read()).next()
        if yamldata == data:
            return OK
        return FAIL

    def lock(self, filename):
        counter = 0
        lockfile = "%s-lock" % filename
        while os.path.isfile(lockfile):
            time.sleep(1)
            counter += 1
            if counter > 10:
                errmsg = "Unable to obtain a lock on %s -- "\
                         "aborting operation to write" % filename
                print errmsg
                return FAIL
        fh = open(lockfile, 'w')
        fh.write('lock')
        fh.close()
        return OK

    def unlock(self, filename):
        lockfile = "%s-lock" % filename
        if not os.path.isfile(lockfile):
            print "lock on %s was not set, asking to be removed" % filename
            return OK
        os.unlink(lockfile)
        return OK

    def writeYamlSecurely(self, data, filename):
        if self.lock(filename) == FAIL:
            return FAIL
        tmpfile = "%s-tmp" % filename
        fh = open(tmpfile, 'w')
        fh.write(yaml.dump(data))
        fh.close()
        self.verifyYamlData(data, tmpfile)
        shutil.move(tmpfile, filename)
        self.unlock(filename)
        return OK

    def findVersion(self):
        files = os.listdir(os.path.join(DEPLOY_DIR, "packages"))
        maxVersion = 1

        for inode in files:
            if inode.endswith(".spkg"):
                if inode.startswith(self.packageName):
                    version = int(inode.split('-')[-1][:-5])
                    if version > maxVersion:
                        maxVersion = version
        return `maxVersion+1`


def displayHelp:
    usage = """
%s: Create a bombardier package from a tarball

USAGE:
%s <-f tar filename> <-n package name> [-s script name] [-r package-rev]

    <-f tar filename> the name of the tarfile which contains the injector data
    <-n package name> the name that the package should be called (minus package version)
    [-s script name]  the name of the scripts to check out of svn
    [-v package ver]  the version number of the package itself 
    
    """ % (sys.argv[0], sys.argv[0])
    print usage
    sys.exit(1)

if __name__ == "__main__":


    try:
        options,args = getopt.getopt(sys.argv[1:], "f:n:s:v:h?",
                                     ["filename" "name", "script", "version", "help"])
    except getopt.GetoptError:
        print "ERROR: Unable to parse options."
        displayHelp()

    scriptName  = None
    tarFileName = None
    packageName = None
    version    = None
    for opt,arg in options:
        if opt in ['-h','-?','--help']: 
            displayHelp()
        elif opt in ['-f', 'filename']:
            tarFileName = arg
        elif opt in ['-s', '--script']:
            scriptName = arg
        elif opt in ['-n', '--name']:
            packageName = arg
        elif opt in ['-v', '--version']:
            version = arg
        else:
            print "Unknown Option",opt
            displayHelp()

    if scriptName == None:
        scriptName = packageName
    if tarFileName == None:
        displayHelp()
    if not os.path.isfile(tarFileName):
        print '\nERROR: File "%s" does not exist' % tarFileName
        displayHelp()

    packageThread = PackageCreationThread(tarFileName, packageName, scriptName, version)
    wrapup = packageThread.run()
    print wrapup
