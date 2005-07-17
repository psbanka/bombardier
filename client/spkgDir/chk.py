#!/cygdrive/c/Python24/python.exe

# chk.py: This is intended to be an indestructible tool to tell you
# exactly how a system is working.

# Copyright (C) 2005 Peter Banka

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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

import sys
class Logger:
    def info(self, string):
        print "info:",string
    def debug(self, string):
        print "debug:",string
    def warning(self, string):
        print "warning:",string
    def error(self, string):
        print "error:",string
    def critical(self, string):
        print "critical:",string
    def rmFileLogging(self):
        pass

def version():
    errors = 0
    try:
        import bombardier.staticData
        print "Bombarier version: %s" % bombardier.staticData.VERSION
        sys.stdout.flush()
    except:
        errors += 1
        print "** Cannot determine the version of bombardier"
        sys.stdout.flush()
    return errors

def repo():
    errors = 0
    try:
        import bombardier.Config
        config = bombardier.Config.Config()
        config.freshen()
        repo = config.get("system", "repository")
        print "Repository: %s" % repo
        sys.stdout.flush()
    except:
        print "** Cannot determine the repository"
        sys.stdout.flush()
        errors += 1
    return errors
        
def pkgGroups():
    errors = 0
    try:
        import bombardier.Config
        config = bombardier.Config.Config()
        config.freshen()
        groups = []
        try:
            data = config.get("system", "type")
            if data:
                groups = ["base", data]
        except:
            pass
        counter = 0
        while True:
            try:
                data = config.get("packageGroups", "group%s" % counter)
                groups.append(data)
                counter += 1
            except:
                break
        if len(groups) == 0:
            print "** No package groups defined"
            sys.stdout.flush()
            errors += 1
        else:
            print "Package Groups: %s" % groups
            sys.stdout.flush()
    except:
        print "** Cannot determine package groups"
        sys.stdout.flush()
        errors += 1
    return errors

def updateStatus():
    errors = 0
    try:
        import bombardier.BombardierClass
        logger=Logger()
        tooLittle, tooMuch = bombardier.BombardierClass.checkBom(logger) # BROKEN
        if tooLittle:
            print "** System does not have packages installed that it should: %s" % tooLittle
            sys.stdout.flush()
            errors += len(tooLittle)
        if tooMuch:
            print "** System has packages installed that should be removed: %s" % tooMuch
            sys.stdout.flush()
            errors += len(tooMuch)
        if not tooLittle and not tooMuch:
            print "All packages on the system are up to date"
            sys.stdout.flush()
    except:
        print "** Cannot determine if packages are up to date"
        sys.stdout.flush()
        errors += 1
    return errors

def daemonStatus():
    errors = 0
    try:
        import bombardier.Windows # BROKEN
        windows = bombardier.Windows.Windows()
        logger=Logger()
        bcs = windows.serviceStatus("bombardierclient", logger)
        if bcs == 0:
            print "Bombardier client is online."
            sys.stdout.flush()
        else:
            print "** Bombardier client is offline."
            sys.stdout.flush()
            errors += 1
        bas = windows.serviceStatus("bombardieragent", logger)
        if bas == 0:
            print "Bombardier Agent is online."
            sys.stdout.flush()
        else:
            print "** Bombardier Agent is offline."
            sys.stdout.flush()
            errors += 1
    except:
        print "unable to determine service status"
        sys.stdout.flush()
        errors += 2
    return errors

if __name__ == "__init__":
    errors = 0
    errors += version()
    errors += repo()
    errors += updateStatus()
    errors += daemonStatus()
    errors += pkgGroups()
    print "===================="
    print "Total problems: %s" % errors
