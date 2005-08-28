#!/usr/bin/python2.3

import cherrypy
import Root

import os, shutil, sys
import webUtil
import yaml
import tarfile
import commands
import time
import ConfigParser

from static import *

class PackageCreationThread:

    def __init__(self, packagename, releaseinfo=None, installscript = None):
        self.name    = packagename
        self.output  = []
        if installscript == None:
            self.installscript = packagename
        else:
            self.installscript = installscript
        self.version = self.findVersion()
        if releaseinfo:
            self.name = "%s-%s" % (packagename, releaseinfo)
        else:
            self.name = packagename
        self.fullname = self.name+'-'+self.version
        self.spkg = self.name+"-"+self.version+".spkg"
        self.destDir = os.path.join(TMP_DIR, self.fullname)
        self.status   = OK
        self.warnings = []
        self.errors   = []

    def cleanup(self):
        self.output.append("cleaning up...")
        try:
            shutil.rmtree(self.destDir)
        except OSError:
            self.warnings.append("Unable to clean up directory %s" % self.destDir)
        files = os.listdir( webUtil.getDeployPath() )
        for inode in files:
            if inode == self.spkg:
                continue
            if inode.endswith(".spkg"):
                if inode.startswith(self.name):
                    warning = "You should remove old package "\
                              "%s/%s..." % (webUtil.getDeployPath(),inode)
                    self.warnings.append(warning)
        return OK

    def metaDataTweak(self, dict):
        return dict

    def createMetaData(self):
        iniPath = os.path.join(self.destDir,'scripts','package.ini')
        if not os.path.isfile(iniPath):
            self.errors.append("%s file does not exist" % iniPath)
            return FAIL
        if not os.path.isfile(webUtil.getPackagesPath()):
            self.errors.append("%s does not exist. "\
                               "Please re-create it." % webUtil.getPackagesPath())
            return FAIL
        output = yaml.loadFile(webUtil.getPackagesPath()).next()
        d = {}
        configParser = ConfigParser.ConfigParser()
        configParser.read(iniPath)
        for section in configParser.sections():
            d[section] = {}
            for option in configParser.options(section):
                d[section][option] =  configParser.get(section, option)
        d['install']['fullName'] = self.fullname
        cmd = "/usr/bin/md5sum %s/%s.spkg" % (webUtil.getDeployPath(), self.fullname)
        s, o = commands.getstatusoutput(cmd)
        checksum = o.split()[0]
        d['install']['md5sum'] = checksum
        d = self.metaDataTweak(d)
        self.output.append("amending packages.yml for %s" % self.name)
        try:
            output[self.name] = d
        except Exception, e:
            self.errors.append("unable to modify metadata database: %s" % e)
            return FAIL
        status = self.writeYamlSecurely(output, webUtil.getPackagesPath())
        return status

    def createTarball(self):
        currDir = os.getcwd()
        self.output.append("creating spkg (%s)..." % self.fullname)
        os.chdir(TMP_DIR)
        tar = tarfile.open(self.spkg, "w:gz")
        tar.add(self.fullname)
        tar.close()
        self.output.append("moving into place...")
        shutil.copyfile(self.spkg, os.path.join(webUtil.getDeployPath(), self.spkg))
        os.unlink(self.spkg)
        os.chdir(currDir)
        return OK

    def createScripts(self):
        self.output.append("exporting install scripts...")
        cmd = "svn export --username %s --password %s %s/%s "\
              "%s/scripts > output.txt 2>&1" \
              % (SVN_USERNAME, SVN_PASSWORD, SVN_ROOT, self.installscript, self.destDir)
        self.output.append("executing %s" % cmd)
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
        self.output.append( "copying injectors...")
        injectorDir = os.path.join(self.destDir, "injector")
        startDir = os.getcwd()
        os.mkdir(injectorDir)
        os.chdir(injectorDir)
        try:
            tar = tarfile.open("stuff", mode="r:gz", fileobj=cherrypy.request.body)
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
            self.output.append("number of files in the tarball: %s" % len(entries))
        os.chdir(startDir)
        return OK

    def prepare(self):
        self.output.append("Building %s" % self.spkg)
        if os.path.isfile(os.path.join(webUtil.getDeployPath(), self.spkg)):
            self.errors.append("file %s already exists in the repository" % self.spkg)
            return FAIL
        self.output.append("creating temporary directory...")
        if os.path.isdir(self.destDir):
            try:
                shutil.rmtree(self.destDir)
            except OSError:
                self.errors.append("Unable to clean out the temp directory.")
                return FAIL
        os.makedirs(self.destDir)
        return OK

    def sendWrapup(self, status):
        outputYaml = yaml.dump({"logdata":self.output, "fullname":self.fullname,
                                "warnings":self.warnings, "errors":self.errors,
                                "status":status}).split('\n')
        return "\n".join(outputYaml)
                           
    def run(self):
        if sys.platform == "win32":
            import pythoncom
            pythoncom.CoInitialize()
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
                cherrypy.log(errmsg)
                return FAIL
        fh = open(lockfile, 'w')
        fh.write('lock')
        fh.close()
        return OK

    def unlock(self, filename):
        lockfile = "%s-lock" % filename
        if not os.path.isfile(lockfile):
            cherrypy.log("lock on %s was not set, asking to be removed" % filename)
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
        files = os.listdir(webUtil.getDeployPath())
        maxVersion = 1

        for inode in files:
            if inode.endswith(".spkg"):
                if inode.startswith(self.name):
                    version = int(inode.split('-')[-1][:-5])
                    if version > maxVersion:
                        maxVersion = version
        return `maxVersion+1`


class DbPatchCreationThread(PackageCreationThread):
    def __init__(self, dbname, version, dependency):
        PackageCreationThread.__init__(self, dbname,
                                       releaseinfo="patch-"+version,
                                       installscript="dbpatch")
        self.dependency = dependency

    def metaDataTweak(self, dict):
        dict["dependencies"]["dep3"] = self.dependency
        return dict

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

    """


class Package(Root.Root):

    known_methods = ["GET", "PUT"]

    def PUT(self, packagename, type=None,
            releaseinfo=None, installscript=None,
            version=None, database=None):

        cherrypy.response.headerMap["Content-type"] = "text/plain"
        if not type or type == GENERIC:
            cherrypy.log( "Package: generic request" )
            packageThread = PackageCreationThread(packagename, releaseinfo,
                                                  installscript)
        elif type == DBPATCH:
            if version == None or database==None:
                cherrypy.response.status = 400
                errmsg = "Did not include all required fields: version and database"
                return errmsg
            packageThread = DbPatchCreationThread(packagename, version,
                                                  dependency=database)
        else:
            cherrypy.response.status = 400
            return "unknown type", "type %s is not understood"
        return packageThread.run()

    def GET(self, type=None):
        cherrypy.response.headerMap["Content-type"] = "text/plain"
        if type != None and type.upper() == "YAML":
            fh = open(webUtil.getPackagesPath(), 'r')
            output = fh.read()
            fh.close()
            return output
        try:
            pdata = yaml.loadFile(webUtil.getPackagesPath()).next()
        except:
            cherrypy.log("package database %s unreadable" % webUtil.getPackagesPath())
            return "---\nERROR"
        output = ""
        packagenames = pdata.keys()
        packagenames.sort()
        output = "\n".join(packagenames)
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

    request = Request()
    packagename = "testPackage"
    installscript = "testPackage"
    releaseinfo = ''
    packageThread = PackageCreationThread(packagename, releaseinfo, installscript)
    packageThread.run()
