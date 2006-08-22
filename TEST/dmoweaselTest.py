#!/c/Python24/python.exe

import os, sys, unittest, shutil, string, yaml, sets, threading, time, tarfile

sys.path = ["..\\client\\dmoweasel\\"] + sys.path
import dmoweasel.dbprocess2 as dbprocess2
import dmoweasel.SQLDMOServer as SQLDMOServer

if len(sys.argv) < 2:
    CONNECTIONSTRING = "(local)"
    print "Assuming that the database is local, unnamed instance..."
else:
    CONNECTIONSTRING = sys.argv[1]
    print "Using %s as the database to connect to..." % CONNECTIONSTRING

CONFIG = {"connectionString":CONNECTIONSTRING}
STRUCTURE     = 1
DATA          = 2
OK            = 0
FAIL          = 1


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

Categories2 = """CREATE TABLE [Categories] (\r\r\n\t[CategoryID] [int] IDENTITY (1, 1) NOT NULL ,\r\r\n\t[CategoryName] [nvarchar] (15) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,\r\r\n\t[Description] [ntext] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,\r\r\n\t[Picture] [image] NULL ,\r\r\n\t[Thingy] [nvarchar] (15) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,\r\r\n\tCONSTRAINT [PK_Categories] PRIMARY KEY  CLUSTERED \r\r\n\t(\r\r\n\t\t[CategoryID]\r\r\n\t)  ON [PRIMARY] \r\r\n) ON[PRIMARY] TEXTIMAGE_ON [PRIMARY]\r\r\n GO\r\r\n\r\r\n\r\r\n"""

EmployeeTerritories2 = 'CREATE TABLE [EmployeeTerritories] (\r\r\n\t[EmployeeID] [int] NOT NULL ,\r\r\n\t[TerritoryID] [nvarchar] (20) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,\r\r\n\t[Thingy] [nvarchar] (15) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,\r\r\n\tCONSTRAINT [PK_EmployeeTerritories] PRIMARY KEY  NONCLUSTERED \r\r\n\t(\r\r\n\t\t[EmployeeID],\r\r\n\t\t[TerritoryID]\r\r\n\t)  ON [PRIMARY] ,\r\r\n\tCONSTRAINT [FK_EmployeeTerritories_Employees] FOREIGN KEY \r\r\n\t(\r\r\n\t\t[EmployeeID]\r\r\n\t) REFERENCES [Employees] (\r\r\n\t\t[EmployeeID]\r\r\n\t),\r\r\n\tCONSTRAINT [FK_EmployeeTerritories_Territories] FOREIGN KEY \r\r\n\t(\r\r\n\t\t[TerritoryID]\r\r\n\t) REFERENCES [Territories] (\r\r\n\t\t[TerritoryID]\r\r\n\t)\r\r\n) ON [PRIMARY] \r\r\nGO\r\r\n\r\r\n\r\r\n'

Region2 = 'CREATE TABLE [Region] (\r\r\n\t[RegionID] [int] NOT NULL ,\r\r\n\t[RegionDescription] [nchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,\r\r\n\t[Thingy] [nvarchar] (15) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,\r\r\n\tCONSTRAINT [PK_Region] PRIMARY KEY  NONCLUSTERED \r\r\n\t(\r\r\n\t\t[RegionID]\r\r\n\t)  ON [PRIMARY] \r\r\n) ON [PRIMARY]\r\r\nGO\r\r\n\r\r\n\r\r\n'

SHORTCUT = 0

class WeaselTest(unittest.TestCase):

    def setUp(self):
        if os.path.isdir("new-structure"):
            shutil.rmtree("new-structure")
        if os.path.isdir("new-data"):
            shutil.rmtree("new-data")
        if SHORTCUT < 2:
            if os.path.isdir('Northwind-data'):
                shutil.rmtree('Northwind-data')
            if os.path.isdir('Northwind-structure'):
                shutil.rmtree('Northwind-structure')
            os.system('tar -xzf data.tar.gz')
            os.system('tar -xzf struct.tar.gz')
            os.system('osql -E -S %s -Q "drop database Northwind"' % CONNECTIONSTRING)
        self.startPath = os.getcwd()
            
    def tearDown(self):
        os.chdir(self.startPath)

    def testLoad(self):
        databaseNames = ["Northwind"]
        dataPath = ''
        logPath  = ''
        logger   = Logger()
        server = SQLDMOServer.SQLDMOServer(CONFIG, logger)

        def makeStructure():
            os.chdir("Northwind-structure")
            #### Structure
            status = dbprocess2.installDbs(databaseNames, STRUCTURE, CONNECTIONSTRING, "", "", logger)
            assert status == OK
            query  = server.exec_query("Northwind", "select * from Orders")
            output = query.output()
            assert output == [], output
            query  = server.exec_query("Northwind", "sp_helptext CustOrderHist")
            output = query.output()
            os.chdir('..')
            
        def verify1():
            os.chdir("Northwind-structure")
            status = dbprocess2.verifyDbs(databaseNames, STRUCTURE, CONNECTIONSTRING, "", "", logger)
            assert status == OK
            os.chdir('..')

        def loadData():
            os.chdir("Northwind-data")
            databaseNames = ["Northwind"]
            status = dbprocess2.installDbs(databaseNames, DATA, CONNECTIONSTRING, "", "", logger)
            assert status == OK
            query  = server.exec_query("Northwind", 'select * from Orders')
            output = query.output()
            assert len(output) == 829
            assert type(output) == type([]), output
            assert output[0]["OrderID"] == "1"
            os.chdir('..')

        def badVerifyDb():
            pass

        def badVerify():
            os.chdir("Northwind-structure")
            open("Databases/Northwind/Tables/EmployeeTerritories/Script", 'wb').write(EmployeeTerritories2)
            status = dbprocess2.verifyDbs(databaseNames, STRUCTURE, CONNECTIONSTRING, "", "", logger)
            assert status == FAIL
            os.chdir('..')

        def dumpStructure():
            os.mkdir('new-structure')
            os.chdir('new-structure')
            CONFIG["type"] = "structure"
            status = dbprocess2.backupDb(databaseNames, {}, CONFIG, logger)
            assert status == OK
            os.chdir('..')

        def dumpData():
            os.mkdir('new-data')
            os.chdir('new-data')
            CONFIG["type"] = "data"
            status = dbprocess2.backupDb(databaseNames, {}, CONFIG, logger)
            assert status == OK
            os.chdir('..')
        
        def simpleModification():
            os.chdir("Northwind-structure")
            open("Databases/Northwind/Tables/EmployeeTerritories/Script", 'wb').write(EmployeeTerritories2)
            status = dbprocess2.updateDbs(databaseNames, {}, STRUCTURE, CONNECTIONSTRING, "", "", logger)
            assert status == OK
            ## Need to perform a select out of that table
            os.chdir('..')

        def complexModification():
            os.chdir("Northwind-structure")
            open("Databases/Northwind/Tables/Region/Script", 'wb').write(Region2)
            status = dbprocess2.updateDbs(databaseNames, {}, STRUCTURE, CONNECTIONSTRING, "", "", logger)
            assert status == OK
            os.chdir('..')

        makeStructure()
        verify1()
        loadData()
        badVerify()
        dumpStructure()
        dumpData()
        simpleModification()
        complexModification()
        
        
if __name__ == "__main__":
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(WeaselTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
