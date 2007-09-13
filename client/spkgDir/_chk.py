#!/usr/local/bin/python2.4

# bc2.py: This module is essentially a hacked version of 
# ReconcileThread.py, and is meant to be run on a linux machine.
# It could use some refinement, but seems to work now.
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

import sys, getopt

from bombardier.staticData import *
from bombardier.Logger import logger, addStdErrLogging
import bombardier.Filesystem, bombardier.Exceptions
import bombardier.Config, bombardier.OperatingSystem
import bombardier.Server, bombardier.CommSocket
import bombardier.Repository, bombardier.BombardierClass
import yaml

if __name__ == "__main__":
    try:
        options,args = getopt.getopt(sys.argv[1:], "p",
                                     ["password"])
    except getopt.GetoptError:
        print "ERROR: Unable to parse options."
        sys.exit(1)
    password = ''
    for opt,arg in options:
        if opt in ['-p', '--password']:
            character = ''
            while character != '\n':
                password += character
                character = sys.stdin.read(1)
    addStdErrLogging()
    filesystem = bombardier.Filesystem.Filesystem()
    server = bombardier.Server.Server(filesystem, password=password)
    os = bombardier.OperatingSystem.OperatingSystem()
    config = bombardier.Config.Config(filesystem, server, os)
    config.freshen()
    repository = bombardier.Repository.Repository(config, filesystem, server)
    repository.getPackageData()
    bc = bombardier.BombardierClass.Bombardier(repository, config,
                                                       filesystem, server, os)
    status = bc.checkSystem(lambda:False)
    print "======================\n\n%s\n" % yaml.dump(status)
