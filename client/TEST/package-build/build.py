#!/usr/bin/python2.4

#^ CASE-sensitivity

import os, shutil, sys, md5
import optparse, glob
import yaml
import tarfile
import ConfigParser
import commands
sys.path.append("../../client")
sys.path.append("../../client/site-root")

import bombardier.Server as Server
import bombardier.Linux
import bombardier.Filesystem as Filesystem
import bombardier.Spkg as Spkg
import bombardier.utility as utility

PACKAGES_FILE = "packages.yml"
PACKAGES_PATH = "/var/www/deploy/packages/"
DEPLOY_DIR    = "/var/www/deploy"

OK          = 0
FAIL        = 1

def getChecksum( fullPath ):
    fp = open( fullPath, 'rb' )
    checkSum = md5.new( fp.read() ).hexdigest()
    fp.close()
    return( checkSum )

def findClassName(scriptName):
    ignoredList = ["installer.py", "uninstaller.py", "configure.py", "verify.py", "backup.py"]
    if os.path.isfile( '../scripts/%s.py' %scriptName ):
        return scriptName
    for filename in os.listdir('../scripts'):
        if filename.endswith(".py"):
            if filename not in ignoredList:
                print "Class file: %s" % filename[:-3]
                return filename[:-3]
    return ''

def extractTar(tarPath, extractPath):
    errors   = []
    startDir = os.getcwd()
    if os.path.isdir(extractPath):
        print "removing directory %s..." % extractPath
        shutil.rmtree(extractPath)
    os.mkdir(extractPath)
    os.chdir(extractPath)
    entries = []
    try:
        tar = tarfile.open(tarPath, mode="r:gz")
        for tarinfo in tar:
            entries.append(tarinfo)
            try:
                tar.extract(tarinfo)
            except tarfile.ExtractError, e:
                errors.append("Error with package %s -- %s" % (tarinfo.name, e))
        tar.close()
    except tarfile.ReadError, e:
        errors.append( "Cannot unzip uploaded file -- %s" % e )
        os.chdir(startDir)
        return FAIL, entries, errors
    except Exception, e: # tarfile's exceptions are lame, like "error" -pbanka
        errors.append( "Cannot untar uploaded file -- %s" % e )
        os.chdir(startDir)
        return FAIL, entries, errors
    os.chdir(startDir)
    return OK, entries, errors

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
            print "MD: (name:%s) key: %s" % (self.name, key)
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

    def get_raw(self, section, option, default=''):
        if not self.requests.has_key(section):
            self.requests[section] = {}
        self.requests[section][option] = default
        return default

    def get_dict(self, section, option, default={}):
        if not self.requests.has_key(section):
            self.requests[section] = {}
        self.requests[section][option] = default
        return default

    def getRequests(self):
        requests = self.requests
        print requests
        dataRequests = self.data.getRequests()
        for key in dataRequests.keys():
            requests[key] = dataRequests[key]
        return requests

class TarCreator:

    def __init__(self, tarFilename, packageName, scriptName, config, className=''):
        self.tarFilename = tarFilename
        self.packageName = packageName
        self.scriptName  = scriptName
        self.className   = className
        self.version     = self.findVersion()
            
        self.packageName = packageName
        self.fullname    = "%s-%s" % (self.packageName,self.version)
        self.spkg        = "%s-%s.spkg" % (self.packageName, self.version)
        self.svnConfig   = config.get("svn")
        if self.svnConfig == None:
            print "No svn configuration in configuration file"
            sys.exit(1)

        self.server      = Server.Server(Filesystem.Filesystem(), config["server"])
        self.tempDir     = config["site"]["tmpPath"]
        self.destDir     = self.tempDir +'/'+ self.fullname
        self.status      = OK
        self.warnings    = []
        self.errors      = []
        self.startDir    = os.getcwd()

    def cleanup(self, deleteOld):
        print "cleaning up..."
        os.unlink(self.spkg)
        os.chdir(self.startDir)
        try:
            shutil.rmtree(self.destDir)
        except OSError:
            self.warnings.append("Unable to clean up directory %s" % self.destDir)

        files = glob.glob("%s/%s*.spkg" % (PACKAGES_PATH, self.packageName) )
        for f in files:
            if self.spkg in f:
                continue
            if deleteOld:
                os.unlink(f)
                print "Deleting %s" % f
            else:
                print "You may want to delete %s" % f
        return OK

    def verifyYamlData(self, packageData):
        checkData = yaml.load(open(PACKAGES_PATH + PACKAGES_FILE).read())
        if checkData == packageData:
            return OK
        return FAIL

    def updateMetaData(self, createTarball = True):
        iniPath = os.path.join(self.destDir,'scripts','package.ini')
        metadata = {}
        if os.path.isfile(iniPath):
            configParser = ConfigParser.ConfigParser()
            configParser.read(iniPath)
            for section in configParser.sections():
                metadata[section] = {}
                for option in configParser.options(section):
                    metadata[section][option] =  configParser.get(section, option)
            metadata['package-version'] = 1
        else:
            injectorDir = os.path.join( self.destDir, 'injector' )
            scriptsDir  = os.path.join( self.destDir, 'scripts' )
            os.chdir(injectorDir)
            if self.className == '':
                self.className = findClassName(self.scriptName)
            if self.className == '':
                self.errors.append("cannot find suitable class file.")
                return FAIL
            sys.path.append(scriptsDir)
            exec("import %s as Pkg" % self.className )
            if not hasattr(Pkg, "metadata"):
                self.warnings.append("Package does not contain any metadata")
            else:
                metadata = Pkg.metadata
            config = MockConfig()
            if metadata['package-version'] == 2:
                exec ('object = Pkg.%s(config)' % self.className)
            else:
                exec ('object = Pkg.%s(config, [], lambda:False)' % self.className)
            metadata['configuration'] = config.getRequests()
            os.chdir(self.startDir)
        if createTarball:
            self.createTarball()
        if not os.path.isfile(self.spkg):
            self.errors.append("spkg file %s does not exist." % self.spkg)
            return FAIL
        checksum = getChecksum(self.spkg)
        metadata['install']['fullName'] = self.fullname
        metadata['install']['md5sum'] = checksum
        metadata['install']['className'] = self.className
        packageData = yaml.load(open(PACKAGES_PATH + PACKAGES_FILE).read())
        print "amending %s for %s" % (PACKAGES_FILE, self.packageName)
        packageData[self.packageName] = metadata
        open(PACKAGES_PATH + PACKAGES_FILE, 'w').write(yaml.dump(packageData, default_flow_style=False))
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
        cmd = "cp %s %s/packages" % (self.spkg, DEPLOY_DIR)
        os.system(cmd)
        return OK

    def prepare(self):
        print "Building %s" % self.spkg
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
                           
    def findVersion(self):
        files = glob.glob(PACKAGES_PATH+'/*.spkg')
        maxVersion = 0

        for filename in files:
            baseFilename = filename.split('/')[-1]
            if baseFilename.startswith(self.packageName):
                version = int(baseFilename.split('-')[-1][:-5])
                if version > maxVersion:
                    maxVersion = version
        return `maxVersion+1`


    def populateInjector(self):
        print  "copying injectors..."
        injectorDir = os.path.join(self.destDir, "injector")
        status, entries, errors = extractTar(self.tarFilename, injectorDir)
        if len(entries) == 0:
            errmsg = "There were no valid entries in the uploaded tarfile."
            self.warnings.append(errmsg)
        else:
            print "number of files in the tarball: %s" % len(entries)
        return status

    def createScripts(self):
        print "exporting install scripts..."
        if not os.path.isdir(self.destDir):
            print "making directory %s" % self.destDir
            os.makedirs(self.destDir)
        cmd = "svn export "
        displayCmd = cmd
        if self.svnConfig.has_key("username"):
            cmd        += "--username %s " % self.svnConfig["username"]
            displayCmd += "--username USER_NAME "
        if self.svnConfig.has_key("password"):
            cmd        += "--password %s " % self.svnConfig["password"]
            displayCmd += "--password PASSWORD "
        cmd        += "%s/%s " % (self.svnConfig["root"], self.scriptName)
        displayCmd += "%s/%s " % (self.svnConfig["root"], self.scriptName)
        cmd        += "%s/scripts > output.txt 2>&1" % self.destDir
        displayCmd += "%s/scripts > output.txt 2>&1" % self.destDir
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

    def makePackage(self, cleanOld):
        if self.prepare() == FAIL:
            self.errors.append("Failed in package creation preparation: check server")
            return self.sendWrapup(FAIL)
        if self.createScripts() == FAIL:
            self.errors.append("Failed in preparing the management scripts: check SVN")
            return self.sendWrapup(FAIL)
        if self.populateInjector() == FAIL:
            self.errors.append("Failed in populating the injectors: check tarball")
            return self.sendWrapup(FAIL)
        if self.updateMetaData() == FAIL:
            self.errors.append("Failed in creating the metadata: check the server")
            return self.sendWrapup(FAIL)
        self.cleanup(cleanOld)
        return self.sendWrapup(OK)

def cleanScripts(scripts, adminFiles):
    for script in scripts:
        if not script:
            continue
        os.unlink(script)
    for f in adminFiles:
        os.unlink(f)
        
if __name__ == "__main__":
    parser = optparse.OptionParser("usage: %prog [options]")
    parser.add_option("-f", "--file", dest="file", default="/var/blob/emptyInjector.tar.gz",
                      help="name of tar file for injector", metavar="FILE.tar.gz")
    parser.add_option("-n", "--name", dest="name", help="name of the package", metavar="NAME")
    parser.add_option("-s", "--script", dest="script", help="name of the script, if different from package", metavar="SCRIPT")
    parser.add_option("-c", "--clean", action="store_true", dest="cleanup", help="clean up", default=False)

    (options, args) = parser.parse_args()

    scriptName   = options.script
    tarFilename  = options.file
    packageName  = options.name

    if not packageName:
        print "need to provide the package name"
        parser.print_help()
        sys.exit(1)

    if packageName.endswith('/'):
        packageName = packageName.split('/')[0]
    className    = '' # delete
    config = yaml.load(open("build-config.yml", 'r').read())

    if not os.path.isfile(tarFilename):
        print "need to provide a valid filname. %s doesn't exist." % tarFilename
        parser.print_help()
        sys.exit(1)

    if packageName == None:
        packageName = tarFilename.split('.tar')[0]
        packageName = packageName.split('/')[-1]
    if scriptName == None:
        scriptName = packageName
    packageCreator = TarCreator(tarFilename, packageName, scriptName, config, className)
        
    wrapup = packageCreator.makePackage(options.cleanup)
    print "\n\n\n",wrapup
