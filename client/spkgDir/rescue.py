#!/cygdrive/c/Python24/python.exe

# rescue.py: Tool for installing or rescuing an existing bombardier installation

# Copyright (C) 2005 Peter Banka, Shawn Sherwood

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

import sys, os, shutil

OK   = 0
FAIL = 1
RUNNING       = 1
NOT_RUNNING   = 2
PENDING       = 3
STOPPED       = 4
spkgPath      = r"c:\spkg"
tmpPath       = r"c:\spkgtmp"
BASE_FILE     = "bombardier-0.4"

try:
    import bombardier.Config
    spkgPath = bombardier.Config.getSpkgPath()
except:
    pass

class Logger:
    def info(self, string):
        print "info:",string
        sys.stdout.flush()
    def debug(self, string):
        print "debug:",string
        sys.stdout.flush()
    def warning(self, string):
        print "warning:",string
        sys.stdout.flush()
    def error(self, string):
        print "error:",string
        sys.stdout.flush()
    def critical(self, string):
        print "critical:",string
        sys.stdout.flush()
    def write(self, string):
        print "info:",string
DWIZ  = 1
SETUP = 2
INNO  = 3

def setPath():
    # FIXME: this is bad
    src = os.path.join(sys.prefix,"python24.dll")
    dst = os.path.join(os.environ['WINDIR'], "system32", "python24.dll")
    if not os.path.isfile(dst):
        shutil.copyfile(src, dst)

class Rescue:

    """ This is intended to be an indestructible tool to get the
    system installed. It'll do whatever mercenary thing it has to do
    to get a working bombardier installation."""

    def __init__(self, conf):
        self.logger = Logger()
        self.conf   = conf

    def performRescue(self):
        if not os.path.isfile(BASE_FILE + ".tar.gz"):
            self.getTarball()
        self.unzip()
        self.unTar()
        os.system("net stop bombardieragent")
        self.runSetup()
        if self.conf['installServices']:
            self.startService()

    def unzip(self):
        infile = BASE_FILE + ".tar.gz"
        outfile = BASE_FILE + ".tar"
        import gzip
        gzipFile = gzip.open(infile)
        outputFile = open(outfile, 'wb')
        data = '1'
        self.logger.info( "2: UNCOMPRESSING" )
        sys.stdout.flush()
        while data:
            try:
                data = gzipFile.read(10000)
            except IOError:
                self.logger.critical( "Error Reading tarball" )
                sys.exit(1)
            except Exception:
                self.logger.critical( "Corrupt tarball" )
                sys.exit(1)
            outputFile.write(data)
        outputFile.close()
        gzipFile.close()

    def unTar(self):
        import tarfile
        fileName = BASE_FILE + ".tar"
        tar = tarfile.open(fileName, "r")
        tar.errorlevel = 2
        self.logger.info( "3: UNTARRING" )
        sys.stdout.flush()
        for tarinfo in tar:
            try:
                tar.extract(tarinfo)
            except tarfile.ExtractError:
                pass
            except IOError:
                pass
        tar.close()


    ### TESTED, but lacking.
    def wget(self, source, filename):
        import urllib2, socket
        if not source.endswith('/'):
            source += '/'
        try:
            urlHandle = urllib2.urlopen(source+filename)
            data = 1
            try:
                fileHandle = open(filename, 'wb')
            except:
                self.logger.warning("Unable to open file %s" % filename)
                sys.exit(1)
            while data:
                try:
                    data = urlHandle.read(10000)
                except:
                    fileHandle.write(data)
                    break
                fileHandle.write(data)
            urlHandle.close()
            fileHandle.flush()
            fileHandle.close()
        except socket.error, e:
            erstr = "Connection problem downloading from %s" % source
            self.logger.warning(erstr)
            self.logger.debug("details: %s " % `e`)
            sys.exit(1)
        except urllib2.URLError, e:
            erstr = "Connection error to %s" % source
            self.logger.warning(erstr)
            if hasattr(e, "msg") and hasattr(e, "code"):
                self.logger.debug("details: %s/%s" % (e.code, e.msg))
            sys.exit(1)
        except urllib2.HTTPError, e:
            if e.code == 401:
                self.logger.error("Repository requires authorization")
                sys.exit(1)
            erstr = "File %s does not exist "\
                    "on repository %s." % (filename, source)
            self.logger.warning(erstr)
            self.logger.debug("Error: %s (%s)" % (`e.code`, e.msg))
            sys.exit(1)

    def getRepository(self):
        config = None
        repository = self.conf.get("repository")
        if not repository:
            try:
                import bombardier.Config
                config = bombardier.Config.Config()
                try:
                    import bombardier.Filesystem
                    filesystem = bombardier.Filesystem.Filesystem()
                    repository = config.getRepository(filesystem)["address"]
                except:
                    ermsg = "No repository found. Please specify"\
                            "the address on the command line."
                    self.logger.error(ermsg)
            except:
                self.logger.info( "error importing Config..." )
                repository = self.conf.get("repository")
        if not repository.endswith("deploy/"):
            repository += "/deploy/"
        self.logger.info( "using repository %s" % repository )
        sys.stdout.flush()
        return repository

    def getTarball(self):
        repository = self.getRepository()
        self.logger.info( "1: DOWNLOADING" )
        sys.stdout.flush()
        status = self.wget(repository, BASE_FILE + ".tar.gz")
        if status == 1:
            sys.exit(1)

    def installDwiz(self, name):
        curDir    = os.getcwd()
        libDir    = os.path.join(sys.prefix, "Lib", "site-packages")
        pythonExe = os.path.join(sys.prefix, "python.exe")
        os.chdir(name)
        if os.path.isdir("PLATLIB"):
            os.chdir("PLATLIB")
            for inode in os.listdir('.'):
                fullpath = os.path.join(libDir, inode)
                try:
                    if os.path.isfile(inode):
                        self.logger.info("copying file %s -> %s" % (inode, fullpath))
                        shutil.copyfile(inode, fullpath)
                    else:
                        self.logger.info("copying directory %s -> %s" % (inode, fullpath))
                        shutil.copytree(inode, fullpath)
                except OSError, e:
                    self.logger.warning("Copying error: %s" % e)
                except IOError, e:
                    self.logger.warning("Copying error: %s" % e)
            os.chdir("..")
        if os.path.isdir("SCRIPTS"):
            os.chdir("SCRIPTS")
            for inode in os.listdir('.'):
                if os.path.isfile(inode):
                    if inode.endswith('.py'):
                        os.system("%s %s -install -silent" % (pythonExe, inode))
            os.chdir("..")
        os.chdir(curDir)


    def runSetup(self):
        self.logger.info( "4: INSTALLING" )
        #scripts = map(string.strip, open('scriptfiles.dat', 'r').readlines())
        #except IOError:
        #scripts = []
        # READ THIS FROM A FILE
        todos = {"pycurl-ssl-zlib-7.14.0.win32-py2.4":DWIZ,
                 "wxPython2.6-win32-unicode-2.6.0.0-py24.exe":INNO,
                 "pyyaml-46":SETUP, "bombardierClient":SETUP,
                 "spkg":SETUP, "site-root":SETUP}
        #todos = {"bombardierClient":SETUP, "spkg":SETUP, "site-root":SETUP} # ^^DEBUGGING ONLY
        try:
            import win32com.client
        except ImportError:
            todos["pywin32-204.win32-py2.4"] = DWIZ
        baseDir = os.getcwd()
        os.chdir("bombardier-0.4")
        for todo in todos.keys():
            self.logger.info("Installing %s" % todo)
            if todos[todo] == DWIZ:
                self.installDwiz(todo)
            if todo == 'wxPython2.6-win32-unicode-2.6.0.0-py24.exe':
                removeWx()
            if todos[todo] == INNO:
                os.system('%s /VERYSILENT /NORESTART /LOG' % todo)
            if todos[todo] == SETUP:
                startDir = os.getcwd()
                os.chdir(todo)
                pythonExe = os.path.join(sys.prefix, "python.exe")
                os.system("%s setup.py install" % pythonExe)
                os.chdir(startDir)
        os.chdir(baseDir)
        
    def startService(self):
        self.logger.info( "5: STARTING" )
        sys.stdout.flush()
        pythonExe = os.path.join(sys.prefix, "python.exe")
        bombardierDir = os.path.join(sys.prefix, "Lib", "site-packages", "bombardier")
        os.chdir(bombardierDir)
        setPath()
        os.system("%s BombardierAgent.py install" % pythonExe)
        os.system("%s BombardierAgent.py --startup auto update" % pythonExe)
        os.system("%s BombardierAgent.py --interactive update" % pythonExe)
        os.system("%s BombardierAgent.py start" % pythonExe)

def usage():
    print "%s: the resourceful bombardier system rescuer" % sys.argv[0]
    print "usage: %s [-h] [-r repository]" % sys.argv[0]
    print "-g go ahead and run bc.py while you're at it"
    print "-r to specify a repository" 
    sys.exit(1)

def getConf():
    conf = {"repository":'', "go":False, 
            "installServices":True, "servicesOnly":False}
    for i in range(1,len(sys.argv)):
        option = sys.argv[i]
        if option == "-g":
            conf["go"] = True
        elif option == "-n":
            conf["installServices"] = False
        elif option == "-r":
            conf["repository"] = sys.argv[i+1]
        elif option == "-s":
            conf["servicesOnly"] = True
        elif option.startswith('-'):
            usage()
    return conf

def removeWx():
    print "Attempting to uninstall wx"
    try:
        from _winreg import OpenKey, QueryValueEx
        from _winreg import KEY_READ, HKEY_LOCAL_MACHINE
        print "Imported from _winreg correctly"
        subKey = r'SOFTWARE\Microsoft\Windows\CurrentVersion' + \
                     r'\Uninstall\wxPython2.6-unicode-py24_is1' 
        print subKey             
        k = OpenKey( HKEY_LOCAL_MACHINE, subKey, 0, KEY_READ )
        print "Opened registry key"             
        uninstallString = str( QueryValueEx( k, 'UninstallString' )[0] )
        print "UninstallString : %s" %uninstallString
        if os.path.isfile( uninstallString ):
            print "Found %s" %uninstallString
            os.system( "start /wait %s /SILENT /SUPPRESSMESSAGEBOXES /LOG" %uninstallString )
    except:
        pass

if __name__ == "__main__":
    """
         1. unpack bombardier
         2. Install dependencies, checking each first

            a. Install windows extensions, if they're not installed
            b. Install wxpython
            c. Install pycurl
            d. Install yaml

         3. Install bombardier, (site-packages and spkg directory)
         4. See if bombardier Agent is running,
            a. if not, start it
            b. if it is, leave it alone
         5. If we were run from bombardierClient,
            a. signal BombardierAgent to restart BombardierClient
            b. otherwise quit

    """  
    conf = getConf()
    rescue = Rescue(conf)
    if conf['servicesOnly']:
        try:
            rescue.startService()
            sys.exit( 0 )
        except:
            sys.exit( 1 )
    rescue.performRescue()
    if conf["go"]:
        os.chdir(spkgPath)
        os.system("bc.py -a")
