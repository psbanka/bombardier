from bombardier.Filesystem import rmScheduledFile
import os, time, re

OK                  = 0
FAIL                = 1

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

    def adoQuery(qstr, database=None):
        import win32com.client
        if database == None:
            database = self.database
        connexion = win32com.client.gencache.EnsureDispatch('ADODB.Connection')
        dbs = "Provider='SQLOLEDB';Data Source='localhost';Initial Catalog='%s';"\
              "Integrated Security=SSPI;" % (database)
        connexion.Open(dbs)
        recordset = connexion.Execute(query)[0]
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
        if self.database:
            cmd += "-d %s " % self.database
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
            rmScheduledFile(outputFile)
        return status, output

    def queryWithOutput(self, qstr, database = '', noHeader=False, dedicated=False, debug=False):
        inputFile  = "%s.in" % time.time()
        open(inputFile, 'w').write(qstr)
        if debug:
            self.logger.debug( "executing: %s" % qstr )
        if not database:
            database = self.database
        status, output = self.executeFileQuery(inputFile, database, noHeader, dedicated)
        rmScheduledFile(inputFile)
        return status, output

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
        if status == FAIL:
            self.logger.error("QUERY FAILED: (%s)" % output)
            self.logger.error("QUERY: (%s)" % qstr)
        return status
