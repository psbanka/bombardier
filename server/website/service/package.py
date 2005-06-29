#!/usr/bin/python2.3

from website.service import *
import threading, os, string, shutil, sys
import twisted.internet.utils
from twisted.protocols import http
from twisted.web import server
import shelve
import webUtil, webops
import yaml
import tarfile
import socket
import commands
import time

from static import *


# types
DBPATCH  = "dbpatch"
HOTFIX   = "hotfix"
GENERIC  = "generic"
PKG_DIR  = webUtil.getDeployPath() 
PACKAGES_FILE = os.path.join(webUtil.getDeployPath(), "packages.yml")

class PackageCreationThread(threading.Thread):

    def __init__(self, request, errlog, packageName, releaseInfo=None, installScript = None):
        threading.Thread.__init__(self)
        self.name    = packageName
        self.errlog  = errlog
        self.request = request
        if installScript == None:
            self.installScript = packageName
        else:
            self.installScript = installScript
        self.version = self.findVersion()
        if releaseInfo:
            self.name = "%s-%s" % (packageName, releaseInfo)
        else:
            self.name = packageName
        self.fullName = self.name+'-'+self.version
        self.spkg = self.name+"-"+self.version+".spkg"
        self.destDir = os.path.join(TMP_DIR, self.fullName)
        self.status   = OK
        self.warnings = []
        self.errors   = []

    def cleanup(self):
        self.request.write("  - cleaning up...\n")
        try:
            shutil.rmtree(self.destDir)
        except OSError:
            self.warnings.append("Unable to clean up directory %s" % self.destDir)
        files = os.listdir(PKG_DIR)
        for file in files:
            if file == self.spkg:
                continue
            if file.endswith(".spkg"):
                if file.startswith(self.name):
                    self.warnings.append("You should remove old package %s/%s..." % (PKG_DIR,file))
        return OK

    def metaDataTweak(self, dict):
        return dict

    def createMetaData(self):
        iniPath = os.path.join(self.destDir,'scripts','package.ini')
        if not os.path.isfile(iniPath):
            self.errors.append("%s file does not exist" % iniPath)
            return FAIL
        if not os.path.isfile(PACKAGES_FILE):
            self.errors.append("%s does not exist. Please re-create it." % PACKAGES_FILE)
            return FAIL
        output = yaml.loadFile(PACKAGES_FILE).next()
        d = {}
        config = ConfigParser.ConfigParser()
        config.read(iniPath)
        for section in config.sections():
            d[section] = {}
            for option in config.options(section):
                d[section][option] =  config.get(section, option)
        d['install']['fullName'] = self.fullName
        s, o = commands.getstatusoutput("/usr/bin/md5sum %s/%s.spkg" % (webUtil.getDeployPath(), self.fullName))
        checksum = o.split()[0]
        d['install']['md5sum'] = checksum
        d = self.metaDataTweak(d)
        self.request.write("  - amending packages.yml for %s\n" % self.name)
        #self.request.write("  - metadata: %s\n" % d )
        try:
            output[self.name] = d
        except Exception, e:
            self.errors.append("unable to modify metadata database: %s" % e)
            return FAIL
        status = self.writeYamlSecurely(output, PACKAGES_FILE)
        return status

    def createTarball(self):
        currDir = os.getcwd()
        self.request.write("  - creating spkg (%s)...\n" % self.fullName)
        os.chdir(TMP_DIR)
        tar = tarfile.open(self.spkg, "w:gz")
        tar.add(self.fullName)
        tar.close()
        self.request.write("  - moving into place...\n")
        shutil.copyfile(self.spkg, os.path.join(PKG_DIR, self.spkg))
        os.unlink(self.spkg)
        os.chdir(currDir)
        return OK

    def createScripts(self):
        self.request.write("  - exporting install scripts...\n")
        scriptsDir = os.path.join(self.destDir, "scripts")
        cmd = "svn export --username %s --password %s %s/%s %s/scripts > output.txt 2>&1" \
              % (SVN_USERNAME, SVN_PASSWORD, SVN_ROOT, self.installScript, self.destDir)
        self.request.write("  - executing %s\n" % cmd)
        if sys.platform == "win32":
            status = os.system(cmd)
        else:
            status, output = commands.getstatusoutput(cmd)
        if status != OK:
            self.errors.append("error exporting scripts for %s" % self.name)
            if os.path.isfile("output.txt"):
                for line in open("output.txt").readlines():
                    self.errors.append("debug: %s" % line.strip())
            return FAIL
        return OK

    def populateInjector(self):
        self.request.write( "  - copying injectors...\n")
        injectorDir = os.path.join(self.destDir, "injector")
        startDir = os.getcwd()
        os.mkdir(injectorDir)
        os.chdir(injectorDir)
        try:
            tar = tarfile.open("stuff", mode="r:gz", fileobj=self.request.content)
            entries = []
            for tarinfo in tar:
                entries.append(tarinfo)
                try:
                    #self.request.write("  - extracting %s to %s\n" % (tarinfo, os.getcwd()))
                    tar.extract(tarinfo)
                except tarfile.ExtractError, e:
                    self.errors.append("Error with package %s,%s "\
                                       "%s" % (fullPackageName, tarinfo.name, e))
            tar.close()
        except tarfile.ReadError, e:
            self.errors.append( "Cannot unzip uploaded file -- %s" % e )
            os.chdir(startDir)
            return FAIL
        except Exception, e: # tarfile's exceptions are a little lame, like "error" -pbanka
            self.errors.append( "Cannot untar uploaded file -- %s" % e )
            os.chdir(startDir)
            return FAIL
        if len(entries) == 0:
            self.warnings.append("There were no valid entries in the uploaded tarfile.")
        else:
            self.request.write("  - number of files in the tarball: %s\n" % len(entries))
        os.chdir(startDir)
        return OK

    def prepare(self):
        self.request.write("  - Building %s\n" % self.spkg)
        if os.path.isfile(os.path.join(PKG_DIR, self.spkg)):
            self.errors.append("file %s already exists in the repository" % self.spkg)
            return FAIL
        self.request.write("  - creating temporary directory...\n")
        if os.path.isdir(self.destDir):
            try:
                shutil.rmtree(self.destDir)
            except OSError:
                self.errors.append("Unable to clean out the temp directory.")
                return FAIL
        os.makedirs(self.destDir)
        return OK

    def sendWrapup(self, status):
        outputYaml = yaml.dump({"fullname":self.fullName, "warnings":self.warnings,
                                "errors":self.errors, "status":status}).split('\n')
        self.request.write(string.join(outputYaml[1:], '\n'))
        self.request.finish()
                           
    def run(self):
        self.request.write("---\n")
        self.request.write("logdata:\n")
        if sys.platform == "win32":
            import pythoncom
            pythoncom.CoInitialize()
        if self.prepare() == FAIL:
            self.errors.append("Failed in package creation preparation: check server")
            self.sendWrapup(FAIL)
            return
        if self.createScripts() == FAIL:
            self.errors.append("Failed in preparing the management scripts: check SVN")
            self.sendWrapup(FAIL)
            return
        if self.populateInjector() == FAIL:
            self.errors.append("Failed in populating the injectors: check tarball")
            self.sendWrapup(FAIL)
            return
        if self.createTarball() == FAIL:
            self.errors.append("Failed in taring up the .spkg: check the server")
            self.sendWrapup(FAIL)
            return
        if self.createMetaData() == FAIL:
            self.errors.append("Failed in creating the metadata: check the server")
            self.sendWrapup(FAIL)
            return
        self.cleanup()
        self.sendWrapup(OK)

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
                self.errlog.error("Unable to obtain a lock on %s -- aborting operation to write" % filename)
                return FAIL
        fh = open(lockfile, 'w')
        fh.write('lock')
        fh.close()
        return OK

    def unlock(self, filename):
        lockfile = "%s-lock" % filename
        if not os.path.isfile(lockfile):
            self.errlog.warning("lock on %s was not set, asking to be removed" % filename)
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
        files = os.listdir(PKG_DIR)
        maxVersion = 1
        for file in files:
            if file.endswith(".spkg"):
                if file.startswith(self.name):
                    version = int(file.split('-')[-1][:-5])
                    if version > maxVersion:
                        maxVersion = version
        return `maxVersion+1`


class DbPatchCreationThread(PackageCreationThread):
    def __init__(self, request, errlog, dbname, version, dependency):
        PackageCreationThread.__init__(self, request, errlog, dbname,
                                       releaseInfo="patch-"+version, installScript="dbpatch")
        self.dependency = dependency

    def metaDataTweak(self, dict):
        dict["dependencies"]["dep3"] = self.dependency
        return dict

class HotfixCreationThread(PackageCreationThread):

    def __init__(self, request, errlog, url, cveNumber, patches=''):
        PackageCreationThread.__init__(self, request, errlog, "Hotfix-%s" % cveNumber,
                                       installScript = "hotfix_template")
        self.url       = url
        self.cveNumber = cveNumber
        self.patches   = patches

    def metaDataTweak(self, dict):
        if self.patches:
            dict["dependencies"]["postdep0"] = self.patches
        return dict

##     def populateInjector(self):
##         injectorDir = os.path.join(self.destDir, "injector")
##         os.mkdir(injectorDir)
##         self.request.write("  - Downloading hotfix from %s...\n" % self.url)
##         filename = urlparse.urlparse(self.url)[2].split('/')[-1]
##         c = webops.prepareCurlObject(self.url, {})
##         try:
##             injectorDir = os.path.join(self.destDir, "injector")
##             if not os.path.isdir(injectorDir):
##                 os.mkdir(injectorDir)
##             fileHandle = open(os.path.join(injectorDir, filename), 'wb')
##             c.setopt(pycurl.WRITEFUNCTION, fileHandle.write)
##         except Exception, e:
##             self.errors.append("ERROR (%s)" % e)
##             self.errors.append("Unable to open file %s for writing" % filename)
##             return FAIL
##         c.perform()
##         fileHandle.flush()
##         fileHandle.close()
##         return OK

def doc():
    return """
get:
    (no arguments):           Will return all packages available in the system
    (no arguments) <type>:    same as before, only in yaml format

put: 
    [packagename] <releaseinfo> <installscript>: (submitting a compressed tarball)
         Creates reates a new 'generic' package. 
         - packagename: the name of the new package. 
         - releaseinfo: additional information to add to the final
                        package name
         - installscript: the name of what to check out of svn for
                          package management scripts. Will default to
                          packagename if left blank.

    [packagename] [type=dbpatch] [version] [database]: (submitting a compressed tarball)
         Creates a new database patch package.
         - version:  indicates the version number of the new patch
         - database: indicates what database the patch will be applied to

    [packagename] [type=hotfix] <packagetopatch>: (submitting a url path for the patch)
         Creates a new Microsoft hotfix patch.
         - packagetopatch: will apply this hotfix only if this package
           is applied to the system
    """

def put(request, logger, errlog):
    """ put method in package.py
    """
    errlog.error("testing the error log")
    print "-----------IN Package.py: PUT"
    path = __name__.split('.')
    status, config = webUtil.processOptions(request, errlog, ["packagename"], ["type"])
    if status == FAIL:
        return webUtil.err400(request, errlog, "Must include at least the packagename")
    type        = config["type"]
    packageName = config["packagename"]
    if not type or type == GENERIC:
        logger.info( "Package generic package request" )
        optionalList   = ["releaseinfo", "installscript"]
        status, config = webUtil.processOptions(request, errlog, [], optionalList)
        packageThread = PackageCreationThread(request, errlog, packageName,
                                              releaseInfo=config["releaseinfo"],
                                              installScript=config["installscript"])
    elif type == DBPATCH:
        status, config = webUtil.processOptions(request, errlog, ["version", "database"], [])
        if status == FAIL:
            errmsg = "Did not include all required fields: version and database"
            return webUtil.err400(request, logger, errmsg)
        packageThread = DbPatchCreationThread(request, errlog, 
                                              packageName,
                                              version=config["version"],
                                              dependency=config["database"])
    elif type == HOTFIX:
        location = request.content.read()
        status, config = webUtil.processOptions(request, errlog, [], ["packagetopatch"])
        if status == FAIL:
            errmsg = "Did not include all required fields: --"
            return webUtil.err400(request, logger, errmsg)
        packageThread = HotfixCreationThread(request, errlog, cveNumber=packageName,
                                             url=location, patches=config["packagetopatch"])
    else:
        return webUtil.err400(request, errlog, "unknown type", "type %s is not understood" % type)
    packageThread.start()
    return server.NOT_DONE_YET

def get(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    status, config = webUtil.processOptions(request, errlog, [],
                                            ["hostname", "packagename", "type", "__doc__"])
    if config.get("__doc__"):
        return doc()
    if config.get("type") and config["type"].upper() == "YAML":
        fh = open(PACKAGES_FILE, 'r')
        output = fh.read()
        fh.close()
        return output
    try:
        pdata = yaml.loadFile(PACKAGES_FILE).next()
    except:
        errlog.error("package path %s unreadable" % pkgPath)
        return "---\nERROR"
    output = ""
    packageNames = pdata.keys()
    packageNames.sort()
    output = string.join(packageNames, '\n')
    return output
    
if __name__ == "__main__":

    class Request:
        def __init__(self):
            self.fh = sys.stdout

        def read(self):
            return self.fh.read()

        def write(self, text):
            return self.fh.write(text)

        def finish(self):
            print "ALL DONE!"

    import sys
    request = Request()
    packageName = "testPackage"
    type = GENERIC
    installScript = "testPackage"
    releaseInfo = ''
    packageThread = PackageCreationThread(request, errlog, packageName, type, releaseInfo, installScript)
    packageThread.run()
