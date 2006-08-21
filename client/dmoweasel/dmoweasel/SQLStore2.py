#!/cygdrive/c/Python24/python.exe
# Version 0.41-168

# SQLStore.py: Handles getting things into and out of the database.

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

import SQLObjectType2, Exceptions, SQLDMOBCP2
import SQLDMOServer

import os, cPickle, time, sys, pywintypes, difflib

MAX_TRIES = 10
OK        = 0
FAIL      = 1

def getTableConstraints(tableName, databaseName, config, logger):
    keyDict        = {}
    tableNames     = SQLDMOServer.SQLDMOServer(config, logger).fetch_path("Databases.%s.Tables" %databaseName).keys()
    keyPattern     = "Databases.%s.Tables.%s.Keys"
    keyCollections = map(lambda x: keyPattern %(databaseName, x) ,tableNames)
    for keyCollectionName in keyCollections:
       table = keyCollectionName.split('.')[3]
       keyCollection = SQLDMOServer.SQLDMOServer(config,logger).fetch_path(keyCollectionName)
       for keyName in keyCollection.keys():
          obj = keyCollection[keyName]
          if hasattr(obj.com, 'SystemObject') and obj.com.SystemObject:
             continue
          keyType = obj.com.Type
          if keyType == 3:
             dependentTable = obj.com.ReferencedTable
             objectName = dependentTable.split('.')[1][1:-1]
             if not keyDict.has_key(objectName):
                keyDict[objectName] = []
             keyDict[objectName].append(table)
    return keyDict

class StoredObject:
   def __init__(self, server, path, config, dir, logger, failureLog):
      self.config = config
      self.path = path
      self.obj = None
      self.objtype = path.split('.')[2]
      self.dir = dir
      self.failureLog = failureLog
      self.logger = logger
      self.server = server
      self.fsPath = self.dbPath2fsPath()
      self.scriptPath = os.path.join(self.fsPath, 'Script')
      self.depsPath   = os.path.join(self.fsPath, 'deps')
      self.nodes = self.path.split(".")
      if self.nodes[0] == "Databases":
         self.database = self.nodes[1]
      else:
         self.database = "master"
      self.dbscript = None
      self.fsscript = None

   def fetchObject(self):
      try:
         self.obj = self.server.fetch_path(self.path)
      except KeyError:
         raise Exceptions.NoDbObjectFound(self.path)

   def dbPath2fsPath(self):
      nodes = [self.dir] + self.path.split('.')
      return reduce(lambda x, y: os.path.join(x, y), nodes)

   def dumpData(self):
      if self.objtype == "Tables":
         bcpFile = SQLDMOBCP2.SQLDMOBCP(self.fsPath, self.server)
         bcpFile.dump_table(self.database, self.obj)

   def drop(self):
      self.logger.warning( "DROPPING %s" % self.path )
      casualties = []
      if not self.obj:
         self.fetchObject()
      if self.objtype == "Tables":
         constraints = getTableConstraints(self.obj.shortName(),
                                           self.database, self.config, self.logger)
         deps = constraints.get(self.obj.shortName())
         if deps:
            for dep in deps:
               baseName = '.'.join(self.path.split('.')[:-1])
               depName  = "%s.%s" % (baseName, dep)
               print ">>>>>>>>>>",depName
               depObject = StoredObject(self.server, depName, self.config,
                                        self.dir, self.logger, self.failureLog)
               newCasualties = depObject.drop()
               print "<<<",newCasualties,">>>"
               casualties   += newCasualties

         self.dumpData()
         query = "drop table %s\n\r" % (self.obj.shortName())
         open("tmpfile.sql", 'w').write(query)
      elif self.objtype == "StoredProcedures": #^ handle more stuff
         query = "drop proc %s\n\r" % (self.obj.shortName())
      else:
         raise Exceptions.UnusableObject(self.path, "drop")
      try:
         status = self.server.exec_query(self.database, queryPath="tmpfile.sql")
         if status == FAIL:
            raise Exceptions.CannotDropObject(self.path)
         self.server = SQLDMOServer.SQLDMOServer(self.config, self.logger)
         casualties.append(self.path)
         return casualties
      except pywintypes.com_error:
         raise Exceptions.CannotDropObject(self.path)

   def isSystemObject(self):
      if hasattr(self.obj.com, 'SystemObject') and self.obj.com.SystemObject:
         self.logger.debug("skipping . . . SystemObject")
         return True
      if self.nodes[-2] == 'Indexes':
         tablePath = reduce(lambda x, y: x + '.' + y, self.nodes[:-2]) 
         tableObj = self.server.fetch_path(tablePath) 
         if hasattr(tableObj.com, 'SystemObject'):
            if tableObj.com.SystemObject:
               return True
      return False
   
   def scriptExists(self):
      if os.path.isfile(os.path.join(self.fsPath, 'Script')):
         self.logger.info("skipping . . . Script exists")
         return True
      return False

   def isDumpable(self):
      if self.scriptExists():
         return False
      if self.isSystemObject():
         return False
      return True

   def readScriptFromDb(self):
      self.dbscript = self.obj.com.Script().encode('ascii', 'ignore')
      return self.dbscript
   
   def readScriptFromFile(self):
      if self.hasScriptFile():
         self.fsscript = open(self.scriptPath).read()
         return self.fsscript
      raise Exceptions.NoScriptFileFound(self.path)

   def hasScriptFile(self):
      if os.path.exists(self.scriptPath):
         return True
      return False

   def createScriptFileFromDb(self):
      self.readScriptFromDb()
      if self.dbscript and self.dbscript.strip(): 
         if not os.path.exists(self.fsPath):
            os.makedirs(self.fsPath)
         out = open(os.path.join(self.fsPath, 'Script'), 'w')
         out.write(self.dbscript)
      else:
         return FAIL
      return OK

   def loadData(self, verbose=False):
      returnStatus  = OK
      try:
         status = self.server.exec_query(self.database, queryPath=self.scriptPath, verbose=verbose)
         if status == FAIL:
            returnStatus = FAIL
      except pywintypes.com_error, e:
         errstr = e[2][2]
         trashString = "[Microsoft][ODBC SQL Server Driver][SQL Server]"
         trashLocation = errstr.rfind(trashString)
         if trashLocation != -1:
            errstr = errstr[trashLocation+len(trashString):]
         if errstr.startswith("Could not create constraint"):
            self.logger.info( "Waiting on %s for dependency resolution" % self.path )
            return FAIL
         if errstr.rfind("already") != -1:
            pass
         if "will still be created" in errstr:
            pass
         if "Could not create constraint" in errstr:
            pass
         else:
            if not "already" in errstr:
               errmsg = "--- %s | %s\n" % (self.path, `errstr`)
               self.failureLog.write(errmsg)
            return FAIL
      return returnStatus

   def createDbObjectFromScript(self, verbose=False):
      self.readScriptFromFile()
      status = self.loadData(verbose)
      self.server = SQLDMOServer.SQLDMOServer(self.config, self.logger)
      self.fetchObject()
      return status

   def compareScripts(self):
      if not self.dbscript:
         self.readScriptFromDb()
      if not self.fsscript:
         self.readScriptFromFile()
      if self.dbscript == self.fsscript:
         return OK
      # SQL Server likes to add mysterious newlines sometimes.
      dbscript2 = self.dbscript.replace('\r','')
      dbscript2 = dbscript2.replace('\n','')
      fsscript2 = self.fsscript.replace('\r','')
      fsscript2 = fsscript2.replace('\n','')
      if fsscript2 == dbscript2:
         return OK
      return FAIL

   def getScriptDifferences(self):
      result = list(difflib.context_diff(self.dbscript.split('\n'), self.fsscript.split('\n')))
      return result

   def getDeps(self):
      depPaths = []
      for dep in self.obj.deps():
         if dep:
            name, typeName = dep  #sometimes it lies about the type.
         else:
            continue
         depPath = SQLObjectType2.type2path(self.path, typeName, name)
         depPaths.append(depPath)
      return depPaths

class SQLStore:
   def __init__(self, config, logger, failureLog):
      self.dir = config["repository"]
      self.connectionString = config["connectionString"]
      self.verbose = config["verbose"]
      self.config = config
      self.skip = config.get("skip")
      self.loaded = []
      self.dumped = []
      self.logger = logger
      self.server = SQLDMOServer.SQLDMOServer(config, self.logger)
      self.failureLog = failureLog

   def db_path2fs_path(self, path): # SCHEDULED FOR DEMOLITION
      nodes = [self.dir] + path.split('.')
      return reduce(lambda x, y: os.path.join(x, y), nodes)

   def checkSkip(self, path):
      if type(self.skip) != type([]):
         return False
      if path in self.skip:
         self.logger.warning( "skipping %s, because it is in the skip list" % path )
         return True
      return False
      
   def dump_object(self, path):
      self.logger.debug(". . %s" %  path)
      if self.checkSkip(path):
         return OK
      if path in self.dumped:
         return OK
      storedObject = StoredObject(self.server, path, self.config,
                                  self.dir, self.logger, self.failureLog)
      storedObject.fetchObject()
      if not storedObject.isDumpable():
         return OK
      storedObject.createScriptFileFromDb()
      self.dumped.append(path)
      depPaths = storedObject.getDeps()
      deps = []
      for depPath in depPaths:
         self.dump_object(depPath)
         depScriptFile = os.path.join(self.db_path2fs_path(depPath), 'Script')
         if os.path.isfile(depScriptFile):
            deps.append(depPath)
      if deps:
         out = open(os.path.join(storedObject.fsPath, 'deps'), 'w')
         cPickle.dump(deps, out)
      return OK

   def verify_object(self, path):
      self.logger.debug(". . %s" %  path)
      if self.checkSkip(path):
         return OK
      storedObject = StoredObject(self.server, path, self.config,
                                  self.dir, self.logger, self.failureLog)
      storedObject.fetchObject()
      if storedObject.isSystemObject():
         return OK
      if not storedObject.hasScriptFile():
         self.logger.error( "FAILURE to verify %s " % path )
         return FAIL

      storedObject.fetchObject()
      try:
         if storedObject.compareScripts() == FAIL:
            result = storedObject.getScriptDifferences()
            self.failureLog.write("MISMATCH in %s: \n" % path)
            self.failureLog.write("Difference %s" % "\n".join(result))
            return FAIL
      except Exceptions.NoScriptFileFound:  # Need to be able to distinguish file or server
         self.failureLog.write("%s not found in filesystem\n" % storedObject.scriptPath)
         return FAIL
      except Exceptions.NoDbObjectFound:  # Need to be able to distinguish file or server
         self.failureLog.write("%s not found in database\n" % storedObject.path)
         return FAIL
      return OK

   def modify_object(self, path):
      self.logger.debug(". . %s" %  path)
      if self.checkSkip(path):
         return OK
      storedObject = StoredObject(self.server, path, self.config,
                                  self.dir, self.logger, self.failureLog)
      storedObject.fetchObject()
      if storedObject.isSystemObject():
         return OK
      if not storedObject.hasScriptFile():
         self.logger.info("skipping . . . Script does not exist")
         return FAIL

      try:
         if storedObject.compareScripts() == OK:
            return OK
      except Exceptions.NoScriptFileFound:  # Need to be able to distinguish file or server
         query = "drop %s" % storedObject.obj # ^^ FIXME
         status = self.server.exec_query(storedObject.database, query)
         if status != OK:
            self.failureLog.write("%s exists on the server that shouldn't\n" % path)
            return FAIL
         return OK
      except Exceptions.NoDbObjectFound:
         self.failureLog.write("%s does not exist on the server\n" % path)
         return FAIL
      
      casualties = storedObject.drop()
      casualties.reverse() # Restore them in the opposite order
      self.logger.warning( "DROPPED %s" % path )
      if storedObject.objtype == "Tables":
         overallStatus = OK
         for tableName in casualties:
            self.logger.info( "Restoring table %s... " % tableName )
            depTable = StoredObject(self.server, tableName, self.config,
                                    self.dir, self.logger, self.failureLog)
            if depTable.createDbObjectFromScript(verbose=True) == OK:
               depTable.fetchObject()
               self.logger.info( "Loading data for %s (%s)" % (tableName, depTable.obj))
               bcpFile = SQLDMOBCP2.SQLDMOBCP(depTable.fsPath, self.server)
               status = bcpFile.load_table(depTable.database, depTable.obj, verbose=True)
               self.logger.info( "Status of load: %s" %status )
               if status == FAIL:
                  overallStatus = FAIL
            else:
               self.logger.error( "Unable to restore Table %s from script" % tableName )
      return overallStatus

   def useOsql(self, database, scriptPath, path):
        cmd = "osql -S %s -d %s -i %s -n -U %s -P %s "\
              "-o output.txt" % (self.connectionString, database, scriptPath,
                                 self.server.login, self.server.password)
        status = os.system( cmd )
        if status != OK:
            self.logger.error("Error in running command %s" % cmd)
            return FAIL
        output = open("output.txt", 'r').read().strip()
        output = " ".join(output.split())
        if output:
            if "There is already" in output:
                status = OK
            if "will still be created." in output:
                status = OK
            if "Could not create constraint" in output:
                status = FAIL
        if status == FAIL:
            errmsg = "%s: (%s)\n" % (path, output)
            self.failureLog.write(errmsg)
        return status

   def useDmo(self, database, scriptPath, path):
        try:
            status = self.server.exec_query(database, queryPath=scriptPath)
            if status == OK:
               self.loaded.append(path)
            return status
        except pywintypes.com_error, e:
            errstr = e[2][2]
            trashString = "[Microsoft][ODBC SQL Server Driver][SQL Server]"
            trashLocation = errstr.rfind(trashString)
            if trashLocation != -1:
               errstr = errstr[trashLocation+len(trashString):]
            if errstr.startswith("Could not create constraint"):
               self.logger.info( "Waiting on %s for dependency resolution" % path )
               return FAIL
            if errstr.rfind("already") != -1:
                pass
            if "will still be created" in errstr:
                pass
            if "Could not create constraint" in errstr:
                pass
            else:
                if not "already" in errstr:
                    errmsg = "--- %s | %s\n" % (path, `errstr`)
                    self.failureLog.write(errmsg)
                return FAIL
        return OK
    
   def load_object(self, path):
      self.logger.debug(". . %s" %  path)
      if self.checkSkip(path):
         return OK
      storedObject = StoredObject(self.server, path, self.config,
                                  self.dir, self.logger, self.failureLog)
      if os.path.isfile(storedObject.depsPath):
         deps = cPickle.load(open(storedObject.depsPath))
         try:
            for dep in deps:
               if type(dep) == type("string"):
                  if dep not in self.loaded:
                     self.load_object(dep)
                  continue
               typeName = dep[1]
               name = dep[0]
               try:
                  depPath = SQLObjectType2.type2path(path, typeName, name)
                  if depPath not in self.loaded:
                     self.load_object(depPath)
               except Exceptions.UnknownType:
                  errmsg = "UNKNOWN TYPE: %s" % (dep)
                  self.failureLog.write( errmsg + "\n")
                  self.logger.error(errmsg)
         except ValueError, e:
            print "ERROR in dependencies: %s (%s)" % (`deps`, e)
            sys.exit(1)
      scriptPath = os.path.join(storedObject.fsPath, 'Script')
      try:
         self.server.fetch_path(path)
         objectExistsOnServer = True
      except: # FIXME: what is being caught?
         objectExistsOnServer = False
      if objectExistsOnServer:
         self.logger.info( "Object exists on server: %s" %path )
         return OK
      elif os.path.exists(scriptPath):
         script = open(scriptPath).read()
         scripts = script.split('GO\r')
         for script in scripts:
            status = self.useDmo(storedObject.database, scriptPath, path)
            return status
      return OK

   def get_collection(self, path):
      tries = 0
      while tries < MAX_TRIES:
         try:
            return self.server.fetch_path(path)
         except KeyError:
            tries += 1
            self.logger.info( "SLEEPING TO GET %s" % path )
            time.sleep(1)
      raise Exceptions.CollectionExistenceAssumption(path)

   def dump_collection(self, path):
      collection = self.get_collection(path)
      for name in collection.keys():
         self.dump_object(path + "." + name)
      return OK

   def modify_collection(self, path):
      fsPath = self.db_path2fs_path(path)
      if os.path.isdir(fsPath):
         names = os.listdir(fsPath)
      else:
         self.failureLog.write("%s does not exist, skipping %s\n" % (fsPath, path))
         return OK

      collection = self.get_collection(path)
      overallStatus = OK
      overallStatus2 = OK
      for name in collection.keys():
         if name in names:
            names.remove(name)
         status = self.modify_object(path + "." + name)
         if status == FAIL:
            overallStatus = FAIL
      if names:
         for name in names:
            status = self.load_object(path + "." + name)
            if status == FAIL:
               self.failureLog.write("Cannot load item %s" % name)
               overallStatus2 = FAIL
      if overallStatus == FAIL or overallStatus2 == FAIL:
         return FAIL
      return OK


   def verify_collection(self, path):
        fsPath = self.db_path2fs_path(path)
        if os.path.isdir(fsPath):
            names = os.listdir(fsPath)
        else:
            self.failureLog.write("%s does not exist, skipping %s" % (fsPath, path))
            return FAIL

        collection = self.get_collection(path)
        overallStatus = OK
        for name in collection.keys():
            if name in names:
                names.remove(name)
            status = self.verify_object(path + "." + name)
            if status == FAIL:
                overallStatus = FAIL
        if names:
            errmsg = "MISSING ITEMS IN COLLECTION: %s\n" % path
            errmsg += "\n".join(names)
            self.logger.error( errmsg )
            self.failureLog.write( errmsg + "\n")
            return FAIL
        return overallStatus

   def load_collection(self, path):
        # Note that it is fruitless to try to determine
        # if path exists before loading. - pbanka
        fsPath = self.db_path2fs_path(path)
        if os.path.isdir(fsPath):
            names = os.listdir(fsPath)
        else:
            names = []
        makingProgress = True
        while makingProgress and len(names) > 0:
            loadFailures = []
            makingProgress = False
            for name in names:
                dirName = os.path.join(fsPath, name)
                if os.path.isdir(dirName):
                    status = self.load_object(path + "." + name)
                    if status == OK:
                        makingProgress = True
                    else:
                        self.logger.info('queuing "%s" for loading later...' % name)
                        loadFailures.append(name)
                else:
                   self.logger.error( "WEIRDNESS: %s doesn't exist." % dirName )
            names = loadFailures
        if not makingProgress:
            print "========== GAVE UP ON THESE %s " % loadFailures
            return FAIL
        return OK

