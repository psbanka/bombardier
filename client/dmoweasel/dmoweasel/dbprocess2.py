#!/cygdrive/c/Python24/python.exe

# dbprocess.py: This library handles wholesale loading or dumping of
# data or structure into or out of a database.

# Copyright (C) 2005 Seth de l'Isle, Peter Banka

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

import SQLDMOServer
import SQLDMOBCP2
import SQLStore2
import UserManager
import Exceptions
import StringIO, traceback

import os, pywintypes

OK        = 0
FAIL      = 1
STRUCTURE = 1
DATA      = 2

def evalStatus(status, logger, component=''):
    if status == OK:
        logger.info( "===================%s SUCCEEDED" % component.upper() )
    else:
        logger.info( "===================%s FAILED" % component.upper() )


################ Database creation stuff

def setDbo(config, databaseNames, logger):
    server = SQLDMOServer.SQLDMOServer(config, logger)
    for databaseName in databaseNames:
        ownerFile = os.path.join("Databases", databaseName, "__owner")
        if not os.path.isfile(ownerFile):
            errmsg = "No __owner file for database in %s. Therefore, "\
                     "not setting dbo role" % databaseName
            logger.warning( errmsg )
            return OK
        owner = open(ownerFile).read().strip()
        if owner:
            cmd = "sp_addrolemember @rolename='db_owner', @membername='%s'" % owner
            server.exec_query(databaseName, cmd)
            logger.info( "setting ownership to %s" % owner )
        else:
            errmsg = "No owner set in %s. Therefore, not setting "\
                     "dbo role" % databaseName 
            logger.warning( errmsg )
    return OK

def createDbs(databaseNames, config, logger):
    for databaseName in databaseNames:
        if not os.path.isdir(os.path.join("Databases", databaseName)):
            errmsg = "Skipping creation of database %s (not a "\
                     "directory)" % databaseName 
            logger.warning( errmsg )
        try:
            query = "create database %s" % databaseName
            if config.get("dataPath"):
                query = "%s on ( NAME = %s_dat, FILENAME = '%s' )" % (query,
                                                                      databaseName,
                                                                      config["dataPath"])
            if config.get("logPath"):
                query = "%s log on ( NAME = %s_log, FILENAME = '%s' )" % (query,
                                                                          databaseName,
                                                                          config["logPath"])
                logger.info( "Creating database %s " % databaseName)
            server = SQLDMOServer.SQLDMOServer(config, logger)
            server.exec_query("master", query)
        except pywintypes.com_error, e:
            if e[2][0] == 1801:
                errmsg = "Database already exists, cowardly refusing to recreate it."
                logger.error( errmsg )
                logger.error( "Run uninstall first." )
            else:
                logger.error( `e` )
            return FAIL
    return OK

def loadDbs(databaseNames, role, config, logger, skipDict={}):
    for databaseName in databaseNames:
        logger.info( "===================LOADING DATABASE %s" % databaseName )
        status1 = OK
        status2 = OK
        if role == STRUCTURE:
            config["skip"] = skipDict.get(databaseName)
            status2 = process_users(config, databaseName, logger)
            evalStatus(status2, logger, "users")
            status1 = process_structure(config, databaseName, logger)
            evalStatus(status1, logger, "structure")
        status3 = process_data(config, databaseName, logger)
        evalStatus(status3, logger, "data")
    if status1 == OK and status2 == OK and status3 == OK:
        return OK
    return FAIL

def installDbs(databaseNames, role, connectionString, user, password,
               logger, dataPath='', logPath='', skipDict={}):
    try:
        logger.info( "Connecting to database server %s" % connectionString )
        config = {'direction': "load", 'truncate': False, 'verbose': True,
                  'repository': '.', 'user': user, 'password': password,
                  'connectionString': connectionString, 'dataPath':dataPath,
                  'logPath': logPath}
        if len(databaseNames) == 0:
            logger.error( "No databases to load" )
            return FAIL
        if role == STRUCTURE:
            status = createDbs(databaseNames, config, logger)
            if status == FAIL:
                return FAIL
        status1 = loadDbs(databaseNames, role, config, logger, skipDict)
        status2 = setDbo(config, databaseNames, logger)
        if status1 == FAIL or status2 == FAIL:
            return FAIL
    except Exception, e:
        ermsg = "Exception %s in installDbs"
        e = StringIO.StringIO()
        traceback.print_exc(file=e)
        e.seek(0)
        data = e.read()
        ermsg = ''
        for line in data.split('\n'):
            ermsg += "\n||>>>%s" % line
        logger.critical(ermsg)
        return FAIL
    return OK

################ Common installation/uninstallation stuff

def getCollectionNames(databaseName, tableNames, store, logger, thin=True):
    
    possibleCollectionNames = ["Databases.%s.Tables" %databaseName,
                               "Databases.%s.StoredProcedures" %databaseName,
                               "Databases.%s.Views" %databaseName,
                               "Databases.%s.UserDefinedFunctions" %databaseName,
                               "JobServer.Jobs"]
    
    indexPattern = "Databases.%s.Tables.%s.Indexes"
    possibleCollectionNames += ( possibleCollectionNames + 
                                map(lambda x: indexPattern %(databaseName, x) ,tableNames))
    indexPattern = "Databases.%s.Tables.%s.Triggers"
    possibleCollectionNames += ( possibleCollectionNames + 
                                map(lambda x: indexPattern %(databaseName, x) ,tableNames))

    if thin:
        collectionNames = []
        for collectionName in possibleCollectionNames:
            fsPath = store.db_path2fs_path(collectionName)
            if not os.path.isdir(fsPath):
                logger.warning("Skipping collection %s because "\
                               "there is no directory %s" % (collectionName, os.path.join(os.getcwd(), fsPath)))
            else:
                collectionNames.append(collectionName)
    else:
        collectionNames = possibleCollectionNames
    return collectionNames

def process_structure(config, databaseName, logger):
    status = OK
    failureLog = open('failures.log', 'w')
    store = SQLStore2.SQLStore(config, logger, failureLog)
    logger.info( "Processing structure for %s / %s " % (databaseName, config["connectionString"] ) )
    server = SQLDMOServer.SQLDMOServer(config, logger)
    if config['direction'] == "dump":
        move_data = store.dump_collection
        tableNames = server.fetch_path("Databases.%s.Tables" %databaseName).keys()
        if config.get("skip"):
            tableNames = list(set(tableNames) - set(config["skip"]))
        collectionNames = getCollectionNames(databaseName, tableNames, store, logger, thin=False)
    else: #loading
        if not databaseName in server.fetch_path("Databases").keys():
            server.exec_query('master', 'create database %s' %databaseName)
        move_data = store.load_collection
        tablePath = os.path.join(config['repository'], 'Databases',
                                 databaseName, 'Tables')
        tableNames = os.listdir(tablePath)
        collectionNames = getCollectionNames(databaseName, tableNames, store, logger)
            
    makingProgress = True
    collectionFailures = []
    while makingProgress and len(collectionNames) > 0:
        collectionSuccesses = []
        collectionFailures = []
        makingProgress = False
        for collectionName in collectionNames:
            if config['verbose']:
                if config['direction'] == 'dump':
                    logger.debug( "dumping %s" % collectionName )
                if config['direction'] == 'load':
                    logger.debug( "loading %s " % collectionName)
            try:
                status = move_data(collectionName)
                if status == OK:
                    collectionSuccesses.append(collectionName)
                    makingProgress = True
                else:
                    collectionFailures.append(collectionName)
            except Exceptions.CollectionExistenceAssumption, e:
                erstr = "\n\nCollectionExistenceAssumptionException! "\
                        "%s\n\n" % e.collectionName
                logger.error(erstr)
        collectionNames = collectionFailures
    if len(collectionFailures) > 0:
        errmsg = "%s collections failed. See failures.log "\
                 "for details." % len(collectionFailures)
        logger.warning( errmsg )
        logger.warning( collectionFailures)
        return OK
    return OK

def process_users(config, database, logger):
    server = SQLDMOServer.SQLDMOServer(config, logger)
    um = UserManager.UserManager(config['repository'], server.com, database)
    if config['direction'] == 'load':
        um.load_users(logger)
    else:
        um.dump_users(logger)
    return OK

def process_data(config, databaseName, logger, tableName=''):
    server = SQLDMOServer.SQLDMOServer(config, logger)
    database = server.fetch_path("Databases.%s" %databaseName)
    if tableName:
        tableObject = database["Tables"][tableName]
        tables = [(tableName, tableObject)]
    else:
        tables = database["Tables"].items()
        if config.get("skip"):
            skip = config.get("skip")
            logger.info( "Skipping: %s" %skip )
            for tableName, tableObject in tables:
                if tableName in skip:
                    tables.remove((tableName, tableObject))
    for tableName, tableObject in tables:
        toplevel = config['repository']
        if os.path.isdir(os.path.join('.', toplevel)):
            toplevel = os.path.join(os.getcwd(), toplevel)
        elif os.path.isdir(toplevel):
            pass
        elif config['direction'] == 'load':
            raise Exceptions.ConfigurationError("%s is not a directory" % toplevel)
        fullDirPath = os.path.join(toplevel, 'Databases', 
                                   databaseName, 'Tables', tableName)
        fullPath = os.path.join(fullDirPath, 'Data')
        if (hasattr(tableObject.com, 'SystemObject') and
           tableObject.com.SystemObject):
            logger.debug("System Object . . . skipping %s" % tableName)
            continue
        elif ( config['direction'] == 'load' and 
               not os.path.isfile(fullPath)):
            logger.debug("no file to load . . . skipping %s" % tableName)
            continue
        elif config['direction'] == 'load' and tableObject.com.Rows != 0:
            logger.debug("data already loaded . . . skipping %s" % tableName)
            continue
        elif config['direction'] == 'dump' and tableObject.com.Rows == 0:
            logger.debug("empty table . . . skipping %s" % tableName)
            continue
        logger.debug(tableName)
        if not os.path.exists(fullDirPath):
            os.makedirs(fullDirPath)
        bcpFile = SQLDMOBCP2.SQLDMOBCP(fullDirPath, server)
        if config['truncate']:
           tableObject.com.TruncateData()
        elif config['direction'] == "dump":
            bcpFile.dump_table(databaseName, tableObject)
        elif config['direction'] == "load":
            try:
                bcpFile.load_table(databaseName, tableObject)
            except Exception, e:
                logger.error( e )
                logger.error("Failed: %s\n" % tableName)
    return OK


################ Verification stuff

def updateDbs(databaseNames, skipDict, role, connectionString, user, password, logger):
    status = FAIL
    if user == None:
        logger.warning("Using administrative user to load database")
    try:
        config = {'connectionString': connectionString, 'verbose': True,
                  'repository': '.', 'user': user, 'password': password}
        if len(databaseNames) == 0:
            logger.error( "No databases to check" )
            return FAIL

        for databaseName in databaseNames:
            config["skip"] = skipDict.get(databaseName)
            logger.info( "===================MODIFYING DATABASE %s" % databaseName )
            if role == STRUCTURE:
                status = modify_structure(config, databaseName, logger)
                evalStatus(status, logger, "structure")
            else:
                logger.error( "CANNOT MODIFY Data packages" )
                return FAIL
    except Exception, e:
        ermsg = "exception caught %s" % e
        e = StringIO.StringIO()
        traceback.print_exc(file=e)
        e.seek(0)
        data = e.read()
        for line in data.split('\n'):
            ermsg += "\n||>>>%s" % line
        logger.critical(ermsg)
        return FAIL
    return status

def verifyDbs(databaseNames, role, connectionString, user, password, logger):
    status = FAIL
    try:
        config = {'verbose': True, 'repository': '.', 'user': user,
                  'password': password, 'connectionString': connectionString}
        if len(databaseNames) == 0:
            logger.error( "No databases to check" )
            return FAIL

        for databaseName in databaseNames:
            logger.info( "===================VERIFYING DATABASE %s" % databaseName )
            status = OK
            if role == STRUCTURE:
                status = verify_structure(config, databaseName, logger)
                evalStatus(status, logger, "structure")
            else:
                logger.error( "CANNOT VERIFY DATA PACKAGES" )
                return FAIL
    except Exception, e:
        ermsg = "exception caught %s" % e
        sio = StringIO.StringIO()
        traceback.print_exc(file=sio)
        sio.seek(0)
        data = sio.read()
        for line in data.split('\n'):
            ermsg += "\n||>>>%s" % line
        logger.critical(ermsg)
        return FAIL
    return status

def getTableNamesFromDb(databaseName, logger, config):
    try:
        server = SQLDMOServer.SQLDMOServer(config, logger)
    except pywintypes.com_error, e:
        if e[2][0] == 0:
            logger.error( "Database server is not responding" )
            return FAIL
        logger.error( e )
        return FAIL
    tableNames = server.fetch_path("Databases.%s.Tables" %databaseName).keys()
    return tableNames

def getTableNamesFromFs(databaseName, config):
    tablePath = os.path.join(config['repository'], 'Databases',
                             databaseName, 'Tables')
    tableNames = os.listdir(tablePath)
    return tableNames

def verify_structure(config, databaseName, logger):
    status = OK
    failureLog = open('failures.log', 'w')
    store = SQLStore2.SQLStore(config, logger, failureLog)
    logger.info( "Verifying structure for %s / %s " %  (databaseName, config["connectionString"] ) )
    tableNamesFromDb = set(getTableNamesFromDb(databaseName, logger, config))
    tableNamesFromFs = set(getTableNamesFromFs(databaseName, config))
    tableNames = list(tableNamesFromDb.union(tableNamesFromFs))
    collectionNames = getCollectionNames(databaseName, tableNames, store, logger)
    while "JobServer.Jobs" in collectionNames:
        collectionNames.remove("JobServer.Jobs")

    collectionFailures = []
    collectionSuccesses = []
    for collectionName in collectionNames:
        sys.exit(0)
        if config['verbose']:
            logger.debug( "checking %s" % collectionName )
        try:
            status = store.verify_collection(collectionName)
            if status == OK:
                collectionSuccesses.append(collectionName)
            else:
                collectionFailures.append(collectionName)
        except Exceptions.CollectionExistenceAssumption, e:
            erstr = "\n\nCollectionExistenceAssumptionException! "\
                    "%s\n\n" % e.collectionName
            logger.error(erstr)
    if len(collectionFailures) > 0:
        logger.warning( "%s collections failed to verify. " % len(collectionFailures))
        logger.warning( collectionFailures)
        return FAIL
    return OK

def modify_structure(config, databaseName, logger):
    status = OK
    failureLog = open('failures.log', 'w')
    store = SQLStore2.SQLStore(config, logger, failureLog)
    logger.info( "Updating structure for %s / %s " %  (databaseName, config["connectionString"] ) )

    tableNames = getTableNamesFromFs(databaseName, config)
    collectionNames = getCollectionNames(databaseName, tableNames, store, logger)

    collectionSuccesses = []
    collectionFailures = []
    for collectionName in collectionNames:
        if config['verbose']:
            logger.debug( "checking %s" % collectionName )
        try:
            status = store.modify_collection(collectionName)
            if status == OK:
                collectionSuccesses.append(collectionName)
            else:
                collectionFailures.append(collectionName)
        except Exceptions.CollectionExistenceAssumption, e:
            collectionFailures.append(collectionName)
            erstr = "\n\nCollectionExistenceAssumptionException! "\
                    "%s\n\n" % e.collectionName
            logger.error(erstr)
    if len(collectionFailures) > 0:
        logger.warning( "%s collections failed to verify. " % len(collectionFailures))
        logger.warning( collectionFailures)
        return FAIL
    return OK

def process_users(config, database, logger):
    server = SQLDMOServer.SQLDMOServer(config, logger)
    um = UserManager.UserManager(config['repository'], server.com, database)
    if config['direction'] == 'load':
        um.load_users(logger)
    else:
        um.dump_users(logger)
    return OK

def process_data(config, databaseName, logger, tableName=''):
    server = SQLDMOServer.SQLDMOServer(config, logger)
    database = server.fetch_path("Databases.%s" %databaseName)
    if tableName:
        tableObject = database["Tables"][tableName]
        tables = [(tableName, tableObject)]
    else:
        tables = database["Tables"].items()
        if config.get("skip"):
            skip = config.get("skip")
            logger.info( "Skipping: %s" %skip )
            for tableName, tableObject in tables:
                if tableName in skip:
                    tables.remove((tableName, tableObject))
    for tableName, tableObject in tables:
        toplevel = config['repository']
        if os.path.isdir(os.path.join('.', toplevel)):
            toplevel = os.path.join(os.getcwd(), toplevel)
        elif os.path.isdir(toplevel):
            pass
        elif config['direction'] == 'load':
            raise Exceptions.ConfigurationError("%s is not a directory" % toplevel)
        fullDirPath = os.path.join(toplevel, 'Databases', 
                                   databaseName, 'Tables', tableName)
        fullPath = os.path.join(fullDirPath, 'Data')
        if (hasattr(tableObject.com, 'SystemObject') and
           tableObject.com.SystemObject):
            logger.debug("System Object . . . skipping %s" % tableName)
            continue
        elif ( config['direction'] == 'load' and 
               not os.path.isfile(fullPath)):
            logger.debug("no file to load . . . skipping %s" % tableName)
            continue
        elif config['direction'] == 'load' and tableObject.com.Rows != 0:
            logger.debug("data already loaded . . . skipping %s" % tableName)
            continue
        elif config['direction'] == 'dump' and tableObject.com.Rows == 0:
            logger.debug("empty table . . . skipping %s" % tableName)
            continue
        logger.debug(tableName)
        if not os.path.exists(fullDirPath):
            os.makedirs(fullDirPath)
        bcpFile = SQLDMOBCP2.SQLDMOBCP(fullDirPath, server)
        if config['truncate']:
           tableObject.com.TruncateData()
        elif config['direction'] == "dump":
            bcpFile.dump_table(databaseName, tableObject)
        elif config['direction'] == "load":
            try:
                bcpFile.load_table(databaseName, tableObject)
            except Exception, e:
                logger.error( e )
                logger.error("Failed: %s\n" % tableName)
    return OK


################ Verification stuff

def updateDbs(databaseNames, skipDict, role, connectionString, user, password, logger):
    status = FAIL
    if user == None:
        logger.warning("Using administrative user to load database")
    try:
        config = {'connectionString': connectionString, 'verbose': True,
                  'repository': '.', 'user': user, 'password': password}
        if len(databaseNames) == 0:
            logger.error( "No databases to check" )
            return FAIL

        for databaseName in databaseNames:
            config["skip"] = skipDict.get(databaseName)
            logger.info( "===================MODIFYING DATABASE %s" % databaseName )
            if role == STRUCTURE:
                status = modify_structure(config, databaseName, logger)
                evalStatus(status, logger, "structure")
            else:
                logger.error( "CANNOT MODIFY Data packages" )
                return FAIL
    except Exception, e:
        ermsg = "exception caught %s" % e
        e = StringIO.StringIO()
        traceback.print_exc(file=e)
        e.seek(0)
        data = e.read()
        for line in data.split('\n'):
            ermsg += "\n||>>>%s" % line
        logger.critical(ermsg)
        return FAIL
    return status

def verifyDbs(databaseNames, role, connectionString, user, password, logger):
    status = FAIL
    try:
        config = {'verbose': True, 'repository': '.', 'user': user,
                  'password': password, 'connectionString': connectionString}
        if len(databaseNames) == 0:
            logger.error( "No databases to check" )
            return FAIL

        for databaseName in databaseNames:
            logger.info( "===================VERIFYING DATABASE %s" % databaseName )
            status = OK
            if role == STRUCTURE:
                status = verify_structure(config, databaseName, logger)
                evalStatus(status, logger, "structure")
            else:
                logger.error( "CANNOT VERIFY DATA PACKAGES" )
                return FAIL
    except Exception, e:
        ermsg = "exception caught %s" % e
        sio = StringIO.StringIO()
        traceback.print_exc(file=sio)
        sio.seek(0)
        data = sio.read()
        for line in data.split('\n'):
            ermsg += "\n||>>>%s" % line
        logger.critical(ermsg)
        return FAIL
    return status

def getTableNamesFromDb(databaseName, logger, config):
    try:
        server = SQLDMOServer.SQLDMOServer(config, logger)
    except pywintypes.com_error, e:
        if e[2][0] == 0:
            logger.error( "Database server is not responding" )
            return FAIL
        logger.error( e )
        return FAIL
    tableNames = server.fetch_path("Databases.%s.Tables" %databaseName).keys()
    return tableNames

def getTableNamesFromFs(databaseName, config):
    tablePath = os.path.join(config['repository'], 'Databases',
                             databaseName, 'Tables')
    tableNames = os.listdir(tablePath)
    return tableNames

def verify_structure(config, databaseName, logger):
    status = OK
    failureLog = open('failures.log', 'w')
    store = SQLStore2.SQLStore(config, logger, failureLog)
    logger.info( "Verifying structure for %s / %s " %  (databaseName, config["connectionString"] ) )
    tableNamesFromDb = set(getTableNamesFromDb(databaseName, logger, config))
    tableNamesFromFs = set(getTableNamesFromFs(databaseName, config))
    tableNames = list(tableNamesFromDb.union(tableNamesFromFs))
    collectionNames = getCollectionNames(databaseName, tableNames, store, logger)
    if "JobServer.Jobs" in collectionNames:
        collectionNames.remove("JobServer.Jobs")

    collectionSuccesses = []
    collectionFailures = []
    for collectionName in collectionNames:
        if config['verbose']:
            logger.debug( "checking %s" % collectionName )
        try:
            status = store.verify_collection(collectionName)
            if status == OK:
                collectionSuccesses.append(collectionName)
            else:
                collectionFailures.append(collectionName)
        except Exceptions.CollectionExistenceAssumption, e:
            collectionFailures.append(collectionName)
            erstr = "\n\nCollectionExistenceAssumptionException! "\
                    "%s\n\n" % e.collectionName
            logger.error(erstr)

    if len(collectionFailures) > 0:
        logger.warning( "%s collections failed to verify. " % len(collectionFailures))
        logger.warning( collectionFailures)
        return FAIL
    return OK

################ Uninstallation stuff

def uninstallDbs(dbOwnershipNames, connectionString, username, password, logger):
    try:
        config = {'verbose': True, 'user': username, 'password': password,
                  'connectionString': connectionString}
        try:
            server = SQLDMOServer.SQLDMOServer(config, logger)
        except pywintypes.com_error, e:
            if e[2][0] == 0:
                logger.error( "Database server is not responding" )
                return FAIL
            logger.error( e )
            return FAIL
        databaseNames = os.listdir('Databases')
        if len(databaseNames) == 0:
            logger.error( "No databases to uninstall" )
            return FAIL
        for databaseName in databaseNames:
            if databaseName in dbOwnershipNames:
                logger.warning( "Dropping database %s" % databaseName )
                try:
                    server.exec_query("master", "drop database %s" % databaseName)
                except pywintypes.com_error, e:
                    if e[2][0] == 3701:
                        logger.error( "database %s does not exist" % databaseName)
                        return OK
                    logger.error( e )
                    return FAIL
    except:
        return FAIL
    return OK

################ Backup stuff

def backupDb(databaseNames, skipDict, config, logger):
    config["direction"] = "dump"
    config["truncate"]  = False
    config["verbose"]   = True
    config["repository"] = '.'
    overallStatus = OK
    try:
        for databaseName in databaseNames:
            config["skip"] = skipDict.get(databaseName)
            logger.info( "===================DUMPING DATABASE %s" % databaseName )
            if config["type"] == "data":
                logger.info(" ******** DATA BACKUP ***********" )
                status = process_data(config, databaseName, logger)
            else:
                logger.info(" ******** STRUCTURE BACKUP ***********" )
                status = process_structure(config, databaseName, logger)
            if status == FAIL:
                overallStatus = FAIL
    except Exception, e:
        ermsg = "Exception caught: %s"% e
        e = StringIO.StringIO()
        traceback.print_exc(file=e)
        e.seek(0)
        data = e.read()
        ermsg = ''
        for line in data.split('\n'):
            ermsg += "\n||>>>%s" % str(line)
        logger.critical(ermsg)
        return FAIL
        
    if not os.path.isdir("Databases"):
        errmsg = "Databases cannot be backed up: no 'Database' directory created by dump" 
        logger.error( errmsg )
        return FAIL
    return overallStatus


