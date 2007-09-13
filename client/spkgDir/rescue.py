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

import sys, os, shutil, glob

OK   = 0
FAIL = 1
RUNNING       = 1
NOT_RUNNING   = 2
PENDING       = 3
STOPPED       = 4
spkgPath      = r"c:\spkg"
tmpPath       = r"c:\spkgtmp"
BASE_FILE     = "bombardier-0.5"

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

class Rescue:

    """ This is intended to be an indestructible tool to get the
    system installed. It'll do whatever mercenary thing it has to do
    to get a working bombardier installation."""

    def __init__(self):
        self.logger = Logger()
        self.baseFile = glob.glob(BASE_FILE + "*")[0].split( '.tar.gz' )[0]

    def unzip(self):
        infile = self.baseFile + ".tar.gz"
        outfile = self.baseFile + ".tar"
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
        fileName = self.baseFile + ".tar"
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

    def runSetup(self):
        self.logger.info( "4: INSTALLING" )
        todos = { "bombardierClient":SETUP,
                 "spkg":SETUP}
        baseDir = os.getcwd()
        os.chdir(self.baseFile)
        print os.listdir('.')
        print todos.keys()
        for todo in todos.keys():
            self.logger.info("Installing %s" % todo)
            if todos[todo] == DWIZ:
                self.installDwiz(todo)
            if todos[todo] == SETUP:
                startDir = os.getcwd()
                os.chdir(todo)
                pythonExe = os.path.join(sys.prefix, "python.exe")
                os.system("%s setup.py install" % pythonExe)
                os.chdir(startDir)
        os.chdir(baseDir)
        
def usage():
    print "%s: the resourceful bombardier system rescuer" % sys.argv[0]
    print "usage: %s [-h] [-r repository]" % sys.argv[0]
    print "-r to specify a repository" 
    sys.exit(1)

if __name__ == "__main__":
    rescue = Rescue()
    rescue.unzip()
    rescue.untar()
    rescue.runSetup()
