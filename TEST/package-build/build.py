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
        print "MD: ",self.dicts
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
        print "dataRequests:",dataRequests
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

    def processIncludes(self):
        try:
            startDir = os.getcwd()
            print "exporting include files..."
            scriptsDir = os.path.join( self.destDir, "scripts" )
            if not os.path.isdir(scriptsDir):
                print "making directory %s" % scriptsDir
                os.makedirs(scriptsDir)
            os.chdir( scriptsDir )
            credStr = ""
            displayCredStr = credStr
            if self.svnConfig.has_key("username"):
                credStr        += "--username %s " % self.svnConfig["username"]
                displayCredStr += "--username USER_NAME "
            if self.svnConfig.has_key("password"):
                credStr        += "--password %s " % self.svnConfig["password"]
                displayCredStr += "--password PASSWORD "
            svnPackagesUrl = self.svnConfig["root"]
            for includeDir in self.includes:
                svnIncludeUrl = "%s%s" % (svnPackagesUrl, includeDir)
                cmdTemplate = "for i in $(svn %s ls %s); do svn export %s %s/$i; done > %s/includeOutput.txt 2>&1"
                cmd        = cmdTemplate %(credStr,        svnIncludeUrl, credStr,        svnIncludeUrl, startDir) 
                displayCmd = cmdTemplate %(displayCredStr, svnIncludeUrl, displayCredStr, svnIncludeUrl, startDir) 
                print "executing %s" % displayCmd
                if sys.platform == "win32":
                    status = os.system(cmd)
                else:
                    status, output = commands.getstatusoutput(cmd)
                if status != OK:
                    self.errors.append("error exporting include scripts from %s" % includeDir)
                    if os.path.isfile("output.txt"):
                        for line in open("output.txt").readlines():
                            self.errors.append("debug: %s" % line.strip())
                    return FAIL
            return OK
        finally:
            os.chdir(startDir)

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
        print "executing %s" % displayCmd
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
    def __init__(self, tarFilename, packageName, scriptName, version, config, includes=[], className=''):
        self.tarFilename = tarFilename
        self.packageName = packageName
        self.scriptName  = scriptName
        self.version     = version
        self.includes    = includes
        self.className   = className
        
        if self.version == None:
            self.findVersion()
            
        self.packageName = packageName
        self.fullname    = "%s-%s" % (self.packageName,self.version)
        self.spkg        = "%s-%s.spkg" % (self.packageName, self.version)
        self.svnConfig   = config.get("svn")
        if self.svnConfig == None:
            print "No svn configuration in configuration file"
            sys.exit(1)
        PackageCreator.__init__(self, config)

    def _populateInjector(self):
        print  "copying injectors..."
        injectorDir = os.path.join(self.destDir, "injector")
        status, entries, errors = extractTar(self.tarFilename, injectorDir)
        if len(entries) == 0:
            errmsg = "There were no valid entries in the uploaded tarfile."
            self.warnings.append(errmsg)
        else:
            print "number of files in the tarball: %s" % len(entries)
        return status

    def makePackage(self):
        if self.prepare() == FAIL:
            self.errors.append("Failed in package creation preparation: check server")
            return self.sendWrapup(FAIL)
        if self.createScripts() == FAIL:
            self.errors.append("Failed in preparing the management scripts: check SVN")
            return self.sendWrapup(FAIL)
        if len( self.includes ) > 0:
            if self.processIncludes() == FAIL:
                self.errors.append("Failed in processing include files: check SVN")
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

def cleanScripts(scripts, adminFiles):
    for script in scripts:
        if not script:
            continue
        os.unlink(script)
    for file in adminFiles:
        os.unlink(file)
        
def makeTarFromSvnPath(svnPath, packageName, tarFileName, config):
    import getpass, re
    tempDir     = config["site"]["tmpPath"]
    os.chdir(tempDir)
    if not os.path.isdir(packageName):
        os.mkdir(packageName)
    os.chdir(packageName)
    if tarFileName:
        status, entries, errors = extractTar(tarFileName, os.getcwd())
        if status == FAIL:
            print "unable to extract %s" % tarFileName
            return FAIL 
    status = OK
    adminFiles = ["SCRIPTS", "database", "old-version", "new-version", "supportFiles"]
    user     = raw_input("SVN User: ")
    password = getpass.getpass()
    server   = '/'.join(svnPath.split('/')[0:3])
    path     = '/'.join(svnPath.split('/')[3:])
    config   = {"address": server, "username": user, "password":password}
    svnServer = Server.Server(Filesystem.Filesystem(), config)
    if not packageName:
        displayHelp("package name must be specified with -d option")
    directory = svnServer.serviceRequest(path)
    scripts = re.compile('href\=\"(\S+\.sql)\"').findall(directory)
    order   = svnServer.serviceRequest(path+"/SCRIPTS")
    if order:
        scripts = [ filename.strip() for filename in order.split('\n') ]

    for script in scripts:
        if not script:
            continue
        try:
            data = svnServer.serviceRequest("%s/%s" % (path, script))
            open(script, 'w').write(data)
        except:
            print "ERROR: could not download %s" % script
            status = FAIL
    for adminFile in adminFiles:
        try:
            data = svnServer.serviceRequest("%s/%s" % (path, adminFile))
        except:
            print "Could not download file '%s'" % adminFile
            data = raw_input("Enter data for [%s]: " % adminFile)
        open(adminFile, 'w').write(data)

    cmd = "tar -czvf %s.tar.gz *" % (packageName)
    status2 = os.system( cmd )
    cleanScripts(scripts, adminFiles)
    if status2 != OK:
        return FAIL
    return status

def displayHelp(errmsg = None):
    if errmsg:
        print "\n\n============================================"
        print "ERROR: %s" % errmsg
        print "============================================"

    base = sys.argv[0]
    usage = """
%s: Prepare the bombardier server to deploy a package

USAGE:
 To create an .spkg from a tarball and an svn repository:
 %s <-f tar filename> <-n package name> [-s script name] [-v package version] [-c config file]

 To import an existing .spkg:
 %s <-i spkg filename> [-c config file]

 To import a database script from an svn tag:
 %s <-d url> <-n package name> [-c config file] [-s script name]

    <-f tar filename>  the name of the tarfile which contains the injector data
    <-n package name>  the name that the package should be called (minus package version)
    [-s script name]   the name of the scripts to check out of svn
    [-v package ver]   the version number of the package itself
    [-c config file]   the path for the configuration file which specifies svn server
                       url and repository url (default is ./build-config.yml

    <-i spkg filename> the name of the spkg which contains the complete spkg file

    <-d url>           the svn path to the scripts directory
    """ % (base, base, base, base)
    print usage
    sys.exit(1)

if __name__ == "__main__":
    try:
        options,args = getopt.getopt(sys.argv[1:], "i:f:n:s:v:h?d:",
                                     ["import=", "filename=" "name=", "script=", "version=", "help", "db=", "classname=", "include="])
    except getopt.GetoptError:
        displayHelp("Unable to parse options.")

    spkgFilename = None
    scriptName   = None
    tarFilename  = None
    packageName  = None
    version      = None
    svnPatchUrl  = None
    className    = ''
    includes     = []
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
        elif opt in ['-d', '--db']:
            svnPatchUrl = arg
        elif opt in ['--classname']:
            className = arg
        elif opt in ['--include']:
            includes.append( arg )
        else:
            displayHelp("Unknown Option %s" % opt)

    config = yaml.loadFile("build-config.yml").next()

    if tarFilename and not svnPatchUrl:
        if not os.path.isfile(tarFilename):
            displayHelp('File "%s" does not exist' % tarFilename)
        if packageName == None:
            packageName = tarFilename.split('.tar')[0]
            packageName = packageName.split('/')[-1]
        if scriptName == None:
            scriptName = packageName
        packageCreator = TarCreator(tarFilename, packageName, scriptName, version, config, includes, className)
    elif spkgFilename:
        if not os.path.isfile(spkgFilename):
            displayHelp('File "%s" does not exist' % spkgFilename)
        packageCreator = SpkgCreator(spkgFilename, config)
    elif svnPatchUrl:
        if not packageName:
            displayHelp("When downloading from SVN, you must include the package name")
        if makeTarFromSvnPath(svnPatchUrl, packageName, tarFilename, config) == OK:
            tarFilename = "%s.tar.gz" % packageName
            tmp = config["site"]["tmpPath"]
            os.system("mv %s %s" % ( tarFilename, tmp) )
            os.chdir(tmp)
            tarPath = os.path.join(tmp, tarFilename)
            packageCreator = TarCreator(tarPath, packageName, scriptName, version, config, includes, className)
        else:
            print "Improper files for patch. Exiting."
            sys.exit(FAIL)
    else:
        displayHelp("Must specify either the -f flag or the -i flag\n\n")
        
    wrapup = packageCreator.makePackage()
    print "\n\n\n",wrapup
