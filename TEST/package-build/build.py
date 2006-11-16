#!/usr/bin/python2.4

#^ CASE-sensitivity

import os, shutil, sys, md5
import getopt
import pycurl
import yaml
import tarfile
import time
import ConfigParser
import commands
sys.path.append("../../client")
sys.path.append("../../client/site-root")

import bombardier.Server as Server
import bombardier.Filesystem as Filesystem
import bombardier.Spkg as Spkg
import installUtils

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
    ignoredList = ["installer.py", "uninstaller.py", "configure.py", "verify.py", "backup.py"]
    for filename in os.listdir('../scripts'):
        if filename.endswith(".py"):
            if filename not in ignoredList:
                print "Class file: %s" % filename[:-3]
                return filename[:-3]
    return ''

class MockDict:
    def __init__(self, name):
        self.name = name
        self.dicts = []
    def get(self, key):
        md = MockDict(key)
        self.dicts.append(md)
        return md
    def getRequests(self):
        output = {}
        for subdict in self.dicts:
            key = subdict.name
            if subdict.dicts:
                value = subdict.getRequests()
            else:
                value = ""
            output[key] = value
        return output

class MockConfig:
    def __init__(self):
        self.requests = {}
        self.data = MockDict("/")

    def get(self, section, option, default=""):
        if not self.requests.has_key(section):
            self.requests[section] = {}
        self.requests[section][option] = default
        return default

    def getRequests(self):
        requests = self.requests
        for dict in self.data.dicts:
            dataRequests = self.data.getRequests()
        for key in dataRequests.keys():
            requests[key] = dataRequests[key]
        return requests

class PackageCreator:

    def __init__(self, config):
        self.server      = Server.Server(Filesystem.Filesystem(), config["server"])
        self.tempDir     = config["site"]["tmpPath"]
        self.destDir     = self.tempDir +'/'+ self.fullname
        self.status      = OK
        self.warnings    = []
        self.errors      = []
        self.startDir    = os.getcwd()
        
    def cleanup(self, deleteSpkg = True):
        print "cleaning up..."
        if deleteSpkg:
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

    def updateMetaData(self, createTarball = True):
        iniPath = os.path.join(self.destDir,'scripts','package.ini')
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
            injectorDir = os.path.join( self.destDir, 'injector' )
            scriptsDir  = os.path.join( self.destDir, 'scripts' )
            os.chdir(injectorDir)
            className = findClassName()
            if className == '':
                self.errors.append("cannot find suitable class file.")
                return FAIL
            sys.path.append(scriptsDir)
            exec("import %s as Pkg" % className )
            if not hasattr(Pkg, "metadata"):
                self.warnings.append("Package does not contain any metadata")
            else:
                metadata = Pkg.metadata
            config = MockConfig()
            exec ('object = Pkg.%s(config)' % className)
            metadata['configuration'] = config.requests
            os.chdir(self.startDir)
        if createTarball:
            self.createTarball()
        if not os.path.isfile(self.spkg):
            self.errors.append("spkg file %s does not exist." % self.spkg)
            return FAIL
        checksum = getChecksum(self.spkg)
        metadata['install']['fullName'] = self.fullname
        metadata['install']['md5sum'] = checksum
        packageData = self.server.serviceYamlRequest(PACKAGES_PATH + PACKAGES_FILE)
        print "amending %s for %s" % (PACKAGES_FILE, self.packageName)
        try:
            packageData[self.packageName] = metadata
        except Exception, e:
            self.errors.append("unable to modify metadata database: %s" % e)
            return FAIL
        #print ">>>>>>>",yaml.dump(packageData)
        status = self.server.serviceYamlRequest(PACKAGES_PATH + PACKAGES_FILE, putData=packageData, debug=True)
        if status == OK:
            status = self.verifyYamlData(packageData)
        return status

    def createTarball(self):
        print "creating spkg (%s)..." % self.fullname
        os.chdir(self.tempDir)
        tar = tarfile.open(self.spkg, "w:gz")
        tar.add(self.fullname)
        tar.close()
        data = open(self.spkg, 'rb').read()
        print "uploading spkg...(bytes %s)" % len(data)
        self.server.serviceRequest(PACKAGES_PATH + self.spkg, putData=data)
        return OK

    def createScripts(self):
        print "exporting install scripts..."
        if not os.path.isdir(self.destDir):
            print "making directory %s" % self.destDir
            os.makedirs(self.destDir)
        cmd = "svn export "
        if self.svnConfig.has_key("username"):
            cmd += "--username %s " % self.svnConfig["username"]
        if self.svnConfig.has_key("password"):
            cmd += "--password %s " % self.svnConfig["password"]
        cmd += "%s/%s " % (self.svnConfig["root"], self.scriptName)
        cmd += "%s/scripts > output.txt 2>&1" % self.destDir
        print "executing %s" % cmd
        #print "pulling data from SVN"
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
        output += yaml.dump({"status":status})
        return output
                           
    def findVersion(self): # ^^ FIXME: Broken and deprecated
        files = os.listdir(os.path.join(DEPLOY_DIR, "packages"))
        maxVersion = 1

        for inode in files:
            if inode.endswith(".spkg"):
                if inode.startswith(self.packageName):
                    version = int(inode.split('-')[-1][:-5])
                    if version > maxVersion:
                        maxVersion = version
        return `maxVersion+1`


class TarCreator(PackageCreator):
    def __init__(self, tarFilename, packageName, scriptName, version, config):
        self.tarFilename = tarFilename
        self.packageName = packageName
        self.scriptName  = scriptName
        self.version     = version
        
        if self.version == None:
            self.findVersion()
            
        self.packageName = packageName
        self.fullname    = "%s-%s" % (self.packageName,self.version)
        self.spkg        = "%s-%s.spkg" % (self.packageName, self.version)
        print ">>>>>>>>>>>>  SPKG: ", self.spkg
        self.svnConfig   = config.get("svn")
        if self.svnConfig == None:
            print "No svn configuration in configuration file"
            sys.exit(1)
        PackageCreator.__init__(self, config)

    def _populateInjector(self):
        print  "copying injectors..."
        injectorDir = os.path.join(self.destDir, "injector")
        startDir = os.getcwd()
        os.mkdir(injectorDir)
        os.chdir(injectorDir)
        try:
            tar = tarfile.open(os.path.join(startDir, self.tarFilename), mode="r:gz")
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

    def makePackage(self):
        if self.prepare() == FAIL:
            self.errors.append("Failed in package creation preparation: check server")
            return self.sendWrapup(FAIL)
        if self.createScripts() == FAIL:
            self.errors.append("Failed in preparing the management scripts: check SVN")
            return self.sendWrapup(FAIL)
        if self._populateInjector() == FAIL:
            self.errors.append("Failed in populating the injectors: check tarball")
            return self.sendWrapup(FAIL)
        if self.updateMetaData() == FAIL:
            self.errors.append("Failed in creating the metadata: check the server")
            return self.sendWrapup(FAIL)
        self.cleanup()
        return self.sendWrapup(OK)


class SpkgCreator(PackageCreator):
    def __init__(self, spkgFilename, config):
        self.spkg     = spkgFilename
        self.fullname = self.spkg.split('.spkg')[0].split('/')[-1]
        self.packageName = '-'.join(self.fullname.split('-')[:-1])
        PackageCreator.__init__(self, config)

    def _explodeSpkg(self): # FIXME: refactor with populateInjector
        print  "opening spkg file..."
        startDir = os.getcwd()
        try:
            os.chdir(self.tempDir)
            tar = tarfile.open(os.path.join(startDir, self.spkg), mode="r:gz")
            entries = []
            for tarinfo in tar:
                entries.append(tarinfo)
                try:
                    tar.extract(tarinfo)
                except tarfile.ExtractError, e:
                    self.errors.append("Error with spkg file %s,%s "\
                                       "%s" % (self.spkg, tarinfo.name, e))
            tar.close()
        except tarfile.ReadError, e:
            self.errors.append( "Cannot unzip uploaded file -- %s" % e )
            os.chdir(startDir)
            return FAIL
        except Exception, e: # tarfile's exceptions are lame, like "error" -pbanka
            self.errors.append( "Cannot untar spkg file -- %s" % e )
            os.chdir(startDir)
            return FAIL
        if len(entries) == 0:
            errmsg = "There were no valid entries in the spkg file."
            self.warnings.append(errmsg)
        else:
            print "number of files in the spkg file: %s" % len(entries)
        os.chdir(startDir)
        return OK

    def makePackage(self): # ^^^ FIXME: refactor with TarCreator to base class
        if self.prepare() == FAIL:
            self.errors.append("Failed in package creation preparation: check server")
            return self.sendWrapup(FAIL)
        if self._explodeSpkg() == FAIL:
            self.errors.append("Failed to open tarball")
            return self.sendWrapup(FAIL)
        if self.updateMetaData(createTarball = False) == FAIL:
            self.errors.append("Failed in creating the metadata: check the server")
            return self.sendWrapup(FAIL)
        self.cleanup(False)
        return self.sendWrapup(OK)
        

def displayHelp():
    usage = """
%s: Prepare the bombardier server to deploy a package

USAGE:
 To create an .spkg from a tarball and an svn repository:
 %s <-f tar filename> <-n package name> [-s script name] [-v package version] [-c config file]

 To import an existing .spkg:
 %s <-i spkg filename> [-c config file]

    <-f tar filename>  the name of the tarfile which contains the injector data
    <-n package name>  the name that the package should be called (minus package version)
    [-s script name]   the name of the scripts to check out of svn
    [-v package ver]   the version number of the package itself
    [-c config file]   the path for the configuration file which specifies svn server
                       url and repository url (default is ./build-config.yml

    <-i spkg filename> the name of the spkg which contains the complete spkg file

    """ % (sys.argv[0], sys.argv[0], sys.argv[0])
    print usage
    sys.exit(1)

if __name__ == "__main__":
    try:
        options,args = getopt.getopt(sys.argv[1:], "i:f:n:s:v:h?",
                                     ["import", "filename" "name", "script", "version", "help"])
    except getopt.GetoptError:
        print "\nERROR: Unable to parse options."
        displayHelp()

    spkgFilename = None
    scriptName   = None
    tarFilename  = None
    packageName  = None
    version      = None
    for opt,arg in options:
        if opt in ['-h','-?','--help']: 
            displayHelp()
        elif opt in ['-i', '--import']:
            spkgFilename = arg
        elif opt in ['-f', '--filename']:
            tarFilename = arg
        elif opt in ['-s', '--script']:
            scriptName = arg
        elif opt in ['-n', '--name']:
            packageName = arg
        elif opt in ['-v', '--version']:
            version = arg
        else:
            print "Unknown Option",opt
            displayHelp()

    config = yaml.loadFile("build-config.yml").next()

    if tarFilename:
        if not os.path.isfile(tarFilename):
            print '\nERROR: File "%s" does not exist' % tarFilename
            displayHelp()
        if packageName == None:
            packageName = tarFilename.split('.tar')[0]
            packageName = packageName.split('/')[-1]
        if scriptName == None:
            scriptName = packageName
        packageCreator = TarCreator(tarFilename, packageName, scriptName, version, config)
    elif spkgFilename:
        if not os.path.isfile(spkgFilename):
            print '\nERROR: File "%s" does not exist' % spkgFilename
            displayHelp()
        packageCreator = SpkgCreator(spkgFilename, config)
    else:
        print "\nERROR: Must specify either the -f flag or the -i flag\n\n"
        displayHelp()
        
    wrapup = packageCreator.makePackage()
    print "\n\n\n",wrapup
