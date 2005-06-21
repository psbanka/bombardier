#!/cygdrive/c/Python23/python

import sys, os
import ConfigParser
import bombardier.miniUtility as miniUtility


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
def dbOwnership(logger):
    packageConfig = ConfigParser.ConfigParser()
    logger.info("Checking ownership of databases from package.ini...")
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
            logger.info( "This script is 'owner' for the following databases: %s" % databases )
        else:
            logger.info( "This script will augment the following databases: %s" % databases )
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

def installDbPackage(config, logger):
    hostname = os.environ['COMPUTERNAME']
    try:
        instance = config.get('sql', 'instance')
        port = config.get('sql', 'port')
    except:
        instance = ''
        port = ''
    connectionString = miniUtility.connectString(hostname, instance, port)
    dataPath, logPath = getPaths(config)
    databaseNames, role = dbOwnership(logger)
    status = dbprocess2.installDbs(databaseNames, role, connectionString, logger, dataPath, logPath)
    if status == FAIL:
        miniUtility.consoleSync(FAIL)
        sys.exit(1)
    miniUtility.consoleSync(OK)
    sys.exit(OK)

def uninstallDbPackage(config, logger):
    hostname = os.environ['COMPUTERNAME']
    try:
        instance = config.get('sql', 'instance')
        port = config.get('sql', 'port')
    except:
        instance = ''
        port = ''
    connectionString = miniUtility.connectString(hostname, instance, port)
    dbOwnershipNames, role = dbOwnership(logger)
    if role == STRUCTURE:
        logger.info("As this is a structure package, uninstalling this package drops the database")
        status = dbprocess2.uninstallDbs(dbOwnershipNames, connectionString, logger)
        if status == FAIL:
            miniUtility.consoleSync(FAIL)
            sys.exit(1)
    miniUtility.consoleSync(OK)
    sys.exit(OK)

def backupDbPackage(config, logger):
    path = os.getcwd()
    packageName = ''
    if path.split('\\')[-1] == "injector" or path.split('\\')[-1] == "backup":
        packageName = path[-2]
    else:
        errmsg = "Unable to determine my package name. Expected to be in an injector or backup directory"
        logger.error( errmsg )
        sys.exit(1)

    hostname = os.environ["COMPUTERNAME"]
    try:
        instance = config.get('sql', 'instance')
        port = config.get('sql', 'port')
    except:
        instance = ''
        port = ''
    connectionString = miniUtility.connectString(hostname, instance, port)
    databaseNames, role = dbOwnership(logger)
    if role != DATA:
        logger.info("This is not a data package -- not backing up")
        sys.exit(NO_BACKUP)
    status = dbprocess2.backupDb(databaseNames, connectionString, logger)
    sys.exit(status)

def verifyDbPackage(config, logger):
    logger.info("verify not implemented")
    sys.exit(0)
