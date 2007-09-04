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

DWIZ  = 1
SETUP = 2
INNO  = 3

PY_CURL  = "pycurl-7.16.2.1-ssl-zlib.win32.py2.5"
PY_YAML  = "PyYAML-3.05"
PY_WIN32 = "pywin32-210.win32-py2.5"

BASE_FILE     = "bombardierPrereqs-0.5"

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
class Rescue:

    """ This is intended to be an indestructible tool to get the
    system installed. It'll do whatever mercenary thing it has to do
    to get a working bombardier installation."""

    def __init__(self):
        self.logger = Logger()
        self.baseFile = glob.glob(BASE_FILE + "*")[0].split( '.tar.gz' )[0]

    def performRescue(self):
        self.unzip()
        self.unTar()
        self.runSetup()

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
        # READ THIS FROM A FILE
        baseDir = os.getcwd()
        os.chdir(self.baseFile)
        todos = {PY_CURL:DWIZ, PY_WIN32:DWIZ, PY_YAML:SETUP}
        print os.listdir('.')
        print todos.keys()
        for todo in todos.keys():
            self.logger.info("Installing %s" % todo)
            if todo == PY_WIN32:
                try:
                    import win32com.client
                    continue
                except:
                    pass
            if todos[todo] == DWIZ:
                self.installDwiz(todo)
            if todos[todo] == SETUP:
                startDir = os.getcwd()
                os.chdir(todo)
                pythonExe = os.path.join(sys.prefix, "python.exe")
                os.system("%s setup.py install" % pythonExe)
                os.chdir(startDir)
        os.chdir(baseDir)
        
if __name__ == "__main__":
    rescue = Rescue()
    rescue.performRescue()
