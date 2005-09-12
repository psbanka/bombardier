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
import bombardier.miniUtility as miniUtility
import bombardier.Logger as Logger

#^ FIXME: REMOVE!!
sys.path = ["d:\\dev\\dmoweasel"] + sys.path

from bombardier.staticData import *

if 1 == 1:
#try:
    import dmoweasel.dbprocess2 as dbprocess2
    import dmoweasel.SQLDMOServer as SQLDMOServer
else:
#except ImportError:
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
    except ConfigParser.NoSectionError:
        return None, None
    except ConfigParser.NoOptionError:
        return None, None
    role = DATA
    try:
        roleInfo = packageConfig.get("backup", "role")
        if roleInfo.upper().startswith("STRUCT"):
            role = STRUCTURE
            errmsg = "This script is 'owner' for the following "\
                     "databases: %s" % databases 
            Logger.info( errmsg )
        else:
            errmsg = "This script will augment the following "\
                     "databases: %s" % databases 
            Logger.info( errmsg )
    except ConfigParser.NoSectionError:
        pass
    except ConfigParser.NoOptionError:
        pass
    return databases, role

def getPaths(config, databaseName):
    dataPath = ''
    logPath  = '' 
    try:
        dataPath = config.get(databaseName, "dataPath")
        if dataPath:
            logPath = config.get(databaseName, "logPath")
    except:
        pass
    return dataPath, logPath

def installDbPackage(config, logger=None):
    hostname = os.environ['COMPUTERNAME']
    try:
        instance = config.get('sql', 'instance')
        port = config.get('sql', 'port')
    except:
        instance = ''
        port = ''
    try:
        dbpassword = config.get('sql', 'sapassword')
        dbuser   = "sa"
    except:
        dbuser = None
        dbpassword = None
    connectionString = miniUtility.connectString(hostname, instance, port)
    databaseNames, role = dbOwnership()
    if len(databaseNames) == 1:
        dataPath, logPath = getPaths(config, databaseNames[0])
    else:
        dataPath = ''
        logPath  = ''
    status = dbprocess2.installDbs(databaseNames, role, connectionString,
                                   dbuser, dbpassword, Logger, dataPath, logPath)
    if status == FAIL:
        miniUtility.consoleSync(FAIL)
        sys.exit(1)
    miniUtility.consoleSync(OK)
    sys.exit(OK)

def verifyDbPackage(config, logger=None):
    hostname = os.environ['COMPUTERNAME']
    try:
        instance = config.get('sql', 'instance')
        port = config.get('sql', 'port')
    except:
        instance = ''
        port = ''
    try:
        dbpassword = config.get('sql', 'sapassword')
        dbuser   = "sa"
    except:
        dbuser = None
        dbpassword = None
    connectionString = miniUtility.connectString(hostname, instance, port)
    databaseNames, role = dbOwnership()
    if len(databaseNames) == 1:
        dataPath, logPath = getPaths(config, databaseNames[0])
    else:
        dataPath = ''
        logPath  = ''
    status = dbprocess2.verifyDbs(databaseNames, role, connectionString,
                                  dbuser, dbpassword, Logger)
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
        errmsg = "As this is a structure package, uninstalling this "\
                 "package drops the database"
        Logger.info( errmsg )
        status = dbprocess2.uninstallDbs(dbOwnershipNames, connectionString, Logger)
        if status == FAIL:
            miniUtility.consoleSync(FAIL)
            sys.exit(1)
    miniUtility.consoleSync(OK)
    sys.exit(OK)

def backupDbPackage(config):
    path = os.getcwd()
    if path.split('\\')[-1] == "injector" or path.split('\\')[-1] == "backup":
        packageName = path[-2]
    else:
        errmsg = "Unable to determine my package name. Expected "\
                 "to be in an injector or backup directory"
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
    status = dbprocess2.backupDb(databaseNames, connectionString, Logger)
    sys.exit(status)

def getServer(connectionString, user=None, password=None):
    server = SQLDMOServer.SQLDMOServer(connectionString, user, password)
    return server

def execQuery(connectionString, database, query, user=None, password=None):
    server = getServer(connectionString, user, password)
    results = server.exec_query(database, query)
    return results
    
