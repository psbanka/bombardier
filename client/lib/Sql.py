from bombardier_common.Filesystem import rmScheduledFile
import os, time, re
from bombardier_common.staticData import OK, FAIL

class BadQuery(Exception):
    def __init__(self, qstr, database):
        e = Exception()
        Exception.__init__(e)
        self.qstr=qstr
        self.database=database
    def __repr__(self):
        return "Query (in database %s) returned an error (%s)" % (self.database, self.qstr)
    def __str__(self):
        return self.__repr__()

class DatabaseException(Exception):
    "Generic exception to throw when things are not what we expect."
    def __init__(self, database, msg):
        Exception.__init__(self)
        self.output = "%s in database: %s" % (msg, database)
    def __repr__(self):
        return self.output
    def __str__(self):
        return self.output

class Sql:

    def __init__(self, config, logger, database):
        host_name   = os.environ["HOSTNAME"].lower()
        host_config = config.dictionary("sql.servers")[host_name]
        self.logger   = logger
        self.database = database
        sqlPath       = host_config.get("sqlPath", "")
        if sqlPath == '':
            self.sqlcmd = 'sqlcmd'
        else:
            self.sqlcmd   = os.path.join(sqlPath, "Tools", "binn", "sqlcmd.exe")

    def set_database(self, database):
        self.database = database

    def has_database(self):
        "Tell me whether I have a database or not"
        query = "EXEC sp_msForEachDB 'PRINT ''?'''"
        status, output = self.queryWithOutput(query, noHeader=True)
        if status == FAIL:
            raise DatabaseException(self.database, "Cannot determine databases")
        databases = output.split()
        if self.database in databases:
            return True
        return False

    def bring_offline(self):
        "bring the database off-line before restoring it"
        if self.has_database():
            query = "alter database %s set offline with rollback immediate"
            status = self.query(query % self.database, dedicated=True, database = "msdb")
            if status != OK:
                msg = "Error trying to bring %s offline" % self.database
                self.logger.error(msg)
                query = "alter database %s set online with rollback immediate"
                status = self.query(query, self.database, dedicated=True, database = "msdb")
                return FAIL
            query = "EXEC sp_dboption '%s', 'single user', 'true'"
            status = self.query(query % self.database, dedicated=True, database = "msdb")
            if status != OK:
                msg = "Error trying to bring %s into single-user mode" % self.database
                self.logger.error(msg)
                self.bring_online()
                return FAIL
            query = "alter database %s set online with rollback immediate"
            status = self.query(query % self.database, dedicated=True, database = "msdb")
        return OK

    def bring_online(self):
        status = OK
        query1 = ("EXEC sp_dboption '%s', 'single user', 'false'", True)
        query2 = ("alter database %s set online with rollback immediate", True)
        query3 = ("restore database %s with recovery", False)
        for query, dflg in [query1, query2, query3]:
            self.logger.debug("Running: %s" % query)
            q_status = self.query(query % self.database, dedicated=dflg, database="msdb")
            self.logger.debug("finished.")
            if q_status == FAIL:
                status = FAIL
        if status == FAIL:
            msg = "Unable to bring %s online" % self.database
            self.logger.error(msg)
            return FAIL
        return OK

    def adoQuery(self, qstr, database=None):
        import win32com.client
        if database == None:
            database = self.database
        connexion = win32com.client.gencache.EnsureDispatch('ADODB.Connection')
        dbs = "Provider='SQLOLEDB';Data Source='localhost';Initial Catalog='%s';"\
              "Integrated Security=SSPI;" % (self.database)
        connexion.Open(dbs)
        recordset = connexion.Execute(qstr)[0]
        output = []
        while True:
            try:
                value = recordset.Fields(0).Value
                output.append(value)
                recordset.MoveNext()
            except:
                break
        connexion.Close()
        return output

    def simpleQuery(self, qstr, database=None):
        if database == None:
            database = self.database
        outputFile = "%s.out" % time.time()
        inputFile  = "%s.in" % time.time()
        self.logger.debug("Running %s" % qstr)
        open(inputFile, 'w').write(qstr)
        cmd =  '"%s" -E ' % self.sqlcmd
        cmd += '-i %s -o %s' % (inputFile, outputFile)
        if database:
            cmd += " -d %s" % database
        status = os.system(cmd)
        rmScheduledFile(inputFile)
        return status, outputFile

    def executeFileQuery(self, fileName, database=None, noHeader=False, dedicated=False):
        if database == None:
            database = self.database
        errchk     = re.compile("^Msg (\d+), Level (\d+), State (\d+)")
        outputFile = "%s.out" % time.time()
        cmd =  '"%s" ' % self.sqlcmd
        if dedicated:
            cmd += '-A '
        cmd += '-i %s -o %s ' % (fileName, outputFile)
        if database:
            cmd += "-d %s " % database
        if noHeader:
            cmd += "-h -1 "
        status = os.system(cmd)
        if status != OK:
            status = FAIL
            self.logger.error("sqlcmd returned an error")
            self.logger.error("CMD: %s" % cmd)
            self.logger.error("DATABASE: %s" % database)
        output = ''
        if os.path.isfile(outputFile): 
            for line in open(outputFile, 'r').readlines():
                if errchk.findall(line) != []:
                    number, level, state = errchk.findall(line)[0]
                    status = FAIL
                output += "%s\n" % line.strip()
            if status == OK:
                rmScheduledFile(outputFile)
        return status, output

    def queryWithOutput(self, qstr, database = '', noHeader=False, dedicated=False, debug=False):
        inputFile  = "%s.in" % time.time()
        open(inputFile, 'w').write(qstr)
        if debug:
            self.logger.info( "executing: %s" % qstr )
        if not database:
            database = self.database
        status, output = self.executeFileQuery(inputFile, database, noHeader, dedicated)
        if status == OK:
            rmScheduledFile(inputFile)
        return status, output

    def queryRegex(self, qstr, regex, database = None, debug=False):
        if not database:
            database = self.database
        status, output = self.queryWithOutput(qstr, noHeader=True, database = database, 
                                              debug = debug)
        if status == FAIL:
            self.logger.error("Couldn't find information from (%s)" % qstr)
            raise BadQuery(qstr, database)
        output_lines = output.split('\n')
        match_info = None
        for line in output_lines:
            match_info = regex.findall(line)
            if match_info:
                return match_info
        if not match_info:
            self.logger.error("Could not find any matches. Output returned:")
            for line in output_lines:
                self.logger.error(line)
            raise BadQuery(qstr, database)
        return match_info

    def get_all_databases(self):
        "Returns a list of databases SQL Server knows about"
        qstr = "select name from sys.databases"
        status, response = self.queryWithOutput(qstr, noHeader=True)
        databases = []
        for database in response.split('\n'):
            if not database.startswith('(') and database != '':
                databases.append(database)
        return databases

    def query(self, qstr, database = None, dedicated=False, debug=False):
        if not database:
            database = self.database
        status, output = self.queryWithOutput(qstr, database = database, dedicated=dedicated, debug=debug)
        if status != OK:
            output_lines = output.split('\n')
            for line in output_lines:
                self.logger.error(line)
        return status
