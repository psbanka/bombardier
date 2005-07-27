#!/cygdrive/c/Python24/python.exe

# dbutils.py: This module provides installation services for dealing
# with databases, works in conjuction with dmoweasel.

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

import sys, os
import ConfigParser
import miniUtility, Logger

#^ FIXME: REMOVE!!
sys.path = ["d:\\dev\\dmoweasel"] + sys.path

from staticData import *

try:
    import dbprocess2
except ImportError:
    print  "SQL DMO libraries are not installed. Aborting." 
    print  "This package is dependent upon DMOWEASEL being installed."
    sys.exit(1)

######################### Database stuff
def dbOwnership():
    packageConfig = ConfigParser.ConfigParser()
    Logger.info("Checking ownership of databases from package.ini...")
    iniPath = os.path.join('..', 'scripts', "package.ini")
    packageConfig.read(iniPath)
    try:
        databaseCsl = packageConfig.get("backup", "databases")
        databases = databaseCsl.split(',')
    except ConfigParser.NoSectionError, e:
        return None, None
    except ConfigParser.NoOptionError, e:
        return None, None
    role = DATA
    try:
        roleInfo = packageConfig.get("backup", "role")
        if roleInfo.upper().startswith("STRUCT"):
            role = STRUCTURE
            Logger.info( "This script is 'owner' for the following databases: %s" % databases )
        else:
            Logger.info( "This script will augment the following databases: %s" % databases )
    except ConfigParser.NoSectionError, e:
        pass
    except ConfigParser.NoOptionError, e:
        pass
    return databases, role

def getPaths(config):
    dataPath = ''
    logPath  = '' 
    try:
        dataPath = config.get(databaseName, "dataPath")
        if dataPath:
            logPath = config.get(databaseName, "logPath")
    except:
        pass
    return dataPath, logPath

def installDbPackage(config):
    hostname = os.environ['COMPUTERNAME']
    try:
        instance = config.get('sql', 'instance')
        port = config.get('sql', 'port')
    except:
        instance = ''
        port = ''
    connectionString = miniUtility.connectString(hostname, instance, port)
    dataPath, logPath = getPaths(config)
    databaseNames, role = dbOwnership()
    status = dbprocess2.installDbs(databaseNames, role, connectionString, dataPath, logPath)
    if status == FAIL:
        miniUtility.consoleSync(FAIL)
        sys.exit(1)
    miniUtility.consoleSync(OK)
    sys.exit(OK)

def uninstallDbPackage(config):
    hostname = os.environ['COMPUTERNAME']
    try:
        instance = config.get('sql', 'instance')
        port = config.get('sql', 'port')
    except:
        instance = ''
        port = ''
    connectionString = miniUtility.connectString(hostname, instance, port)
    dbOwnershipNames, role = dbOwnership()
    if role == STRUCTURE:
        Logger.info("As this is a structure package, uninstalling this package drops the database")
        status = dbprocess2.uninstallDbs(dbOwnershipNames, connectionString)
        if status == FAIL:
            miniUtility.consoleSync(FAIL)
            sys.exit(1)
    miniUtility.consoleSync(OK)
    sys.exit(OK)

def backupDbPackage(config):
    path = os.getcwd()
    packageName = ''
    if path.split('\\')[-1] == "injector" or path.split('\\')[-1] == "backup":
        packageName = path[-2]
    else:
        errmsg = "Unable to determine my package name. Expected to be in an injector or backup directory"
        Logger.error( errmsg )
        sys.exit(1)

    hostname = os.environ["COMPUTERNAME"]
    try:
        instance = config.get('sql', 'instance')
        port = config.get('sql', 'port')
    except:
        instance = ''
        port = ''
    connectionString = miniUtility.connectString(hostname, instance, port)
    databaseNames, role = dbOwnership()
    if role != DATA:
        Logger.info("This is not a data package -- not backing up")
        sys.exit(NO_BACKUP)
    status = dbprocess2.backupDb(databaseNames, connectionString)
    sys.exit(status)

def verifyDbPackage(config):
    Logger.info("verify not implemented")
    sys.exit(0)