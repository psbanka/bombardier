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

PACKAGES_FILE = "packages.yml"
PACKAGES_PATH = "deploy/packages/"

OK          = 0
FAIL        = 1

def getChecksum( fullPath ):
    fp = open( fullPath, 'rb' )
    checkSum = md5.new( fp.read() ).hexdigest()
    fp.close()
    return( checkSum )

def findClassName():
    ignoredList = ["installer.py", "uninstaller.py", "configure.py", "verify.py"]
    for filename in os.listdir('.'):
        if filename.endswith(".py"):
            if filename not in ignoredList:
                print "Class file: %s" % filename[:-3]
                return filename[:-3]
    return ''

class MockConfig:
    def __init__(self):
        self.requests = {}
    def get(self, section, option, default=""):
        if not self.requests.has_key(section):
            self.requests[section] = {}
        self.requests[section][option] = default
        return default

class PackageCreationThread:

    def __init__(self, tarFileName, packageName, scriptName, version, config):
        self.tarFileName = tarFileName
        self.packageName = packageName
        self.scriptName  = scriptName
        self.version     = version
        self.config      = config
        self.server      = Server.Server(Filesystem.Filesystem(), config["server"])
        
        if self.version == None:
            self.findVersion()
            
        self.packageName = packageName
        self.fullname    = self.packageName+'-'+self.version
        self.spkg        = self.packageName+"-"+self.version+".spkg"
        self.destDir     = config["site"]["tmpPath"] +'/'+ self.fullname
        self.status      = OK
        self.warnings    = []
        self.errors      = []
        self.startDir    = os.getcwd()

    def cleanup(self):
        print "cleaning up..."
        os.unlink(self.spkg)
        os.chdir(self.startDir)
        try:
            shutil.rmtree(self.destDir)
        except OSError:
            self.warnings.append("Unable to clean up directory %s" % self.destDir)
        return OK

    def verifyYamlData(self, packageData):
        checkData = self.server.serviceYamlRequest(PACKAGES_PATH + PACKAGES_FILE)
        if checkData == packageData:
            return OK
        return FAIL

    def updateMetaData(self):
        iniPath = os.path.join(self.destDir,'scripts','package.ini')
        checksum = getChecksum(self.spkg)
        metadata = {}
        if os.path.isfile(iniPath):
            print "processing metadata for a type 1 package..."
            configParser = ConfigParser.ConfigParser()
            configParser.read(iniPath)
            for section in configParser.sections():
                metadata[section] = {}
                for option in configParser.options(section):
                    metadata[section][option] =  configParser.get(section, option)
            metadata['package-version'] = 1
        else:
            print "processing metadata for a type 2 or greater package"
            os.chdir(self.fullname+'/'+'scripts')
            className = findClassName()
            sys.path.append('.')
            exec("import %s as Pkg" % className )
            os.chdir(self.config["site"]["tmpPath"])
            if not hasattr(Pkg, "metadata"):
                self.warnings.append("Package does not contain any metadata")
            else:
                metadata = Pkg.metadata
            config = MockConfig()
            exec ('object = Pkg.%s(config)' % className)
            metadata['configuration'] = config.requests

        metadata['install']['fullName'] = self.fullname
        metadata['install']['md5sum'] = checksum
        packageData = self.server.serviceYamlRequest(PACKAGES_PATH + PACKAGES_FILE)
        print "amending %s for %s" % (PACKAGES_FILE, self.packageName)
        try:
            packageData[self.packageName] = metadata
        except Exception, e:
            self.errors.append("unable to modify metadata database: %s" % e)
            return FAIL
        print ">>>>>>>",yaml.dump(packageData)
        status = self.server.serviceYamlRequest(PACKAGES_PATH + PACKAGES_FILE, putData=packageData)
        if status == OK:
            status = self.verifyYamlData(packageData)
        return status

    def createTarball(self):
        print "creating spkg (%s)..." % self.fullname
        os.chdir(self.config["site"]["tmpPath"])
        tar = tarfile.open(self.spkg, "w:gz")
        tar.add(self.fullname)
        tar.close()
        data = open(self.spkg, 'rb').read()
        print "uploading spkg...(bytes %s)" % len(data)
        self.server.serviceRequest(PACKAGES_PATH + self.spkg, putData=data)
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

    def createScripts(self):
        print "exporting install scripts..."
        if not os.path.isdir(self.destDir):
            print "making directory %s" % self.destDir
            os.makedirs(self.destDir)
        cmd = "svn export "
        if self.config["svn"].has_key("username"):
            cmd += "--username %s " % self.config["svn"]["username"]
        if self.config["svn"].has_key("password"):
            cmd += "--password %s " % self.config["svn"]["password"]
        cmd += "%s/%s " % (self.config["svn"]["root"], self.scriptName)
        cmd += "%s/scripts > output.txt 2>&1" % self.destDir
        #print "executing %s" % cmd
        print "pulling data from SVN"
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

    def prepare(self):
        print "Building %s" % self.spkg
        #if os.path.isfile(os.path.join(os.path.join(DEPLOY_DIR, "packages", self.spkg))):
        #    self.errors.append("file %s already exists in the repository" % self.spkg)
        #    return FAIL
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
        output  = yaml.dump({"fullname":self.fullname})
        output += yaml.dump({"warnings":self.warnings})
        output += yaml.dump({"errors":self.errors})
        output += yaml.dump({"status":self.status})
        return output
                           
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
        if self.updateMetaData() == FAIL:
            self.errors.append("Failed in creating the metadata: check the server")
            return self.sendWrapup(FAIL)
        self.cleanup()
        return self.sendWrapup(OK)

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


def displayHelp():
    usage = """
%s: Create a bombardier package from a tarball

USAGE:
%s <-f tar filename> <-n package name> [-s script name] [-r package-rev] [-c config file]

    <-f tar filename> the name of the tarfile which contains the injector data
    <-n package name> the name that the package should be called (minus package version)
    [-s script name]  the name of the scripts to check out of svn
    [-v package ver]  the version number of the package itself
    [-c config file]  the path for the configuration file which specifies svn server
                      url and repository url (default is ./build-config.yml
    
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

    config = yaml.loadFile("build-config.yml").next()
    packageThread = PackageCreationThread(tarFileName, packageName, scriptName, version, config)
    wrapup = packageThread.run()
    print "\n\n\n",wrapup
