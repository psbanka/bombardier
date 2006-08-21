#!/cygdrive/c/Python24/python.exe
# Version 0.41-168

# SQLDMOServer.py: Extends SQLDMOObject to handle the behavior of the
# server itself.

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

import win32com.client
import pywintypes, os
import _winreg as winreg
import SQLDMOQuery2
import SQLDMOObject

OK   = 0
FAIL = 1

class SQLDMOServer(SQLDMOObject.SQLDMOObject):
    def __init__(self, config, logger):
        DMOServer = win32com.client.Dispatch("SQLDMO.SQLServer")
        self.username         = config.get("username")
        self.password         = config.get("password")
        self.connectionString = config["connectionString"]
        self.logger           = logger
        try:
            if self.username:
                #self.logger.debug( "Using %s to connect to the database" % self.username )
                DMOServer.LoginSecure = False
                DMOServer.Connect(self.connectionString, self.username, self.password)
            else:
                #self.logger.debug( "Using local NT account to connect to the database" )
                DMOServer.LoginSecure = True
                DMOServer.Connect(self.connectionString)
        except pywintypes.com_error, e:
            errstr = e[2][2]
            trashString = "[Microsoft][ODBC SQL Server Driver][SQL Server]"
            trashLocation = errstr.rfind(trashString)
            if trashLocation != -1:
                errstr = errstr[trashLocation+len(trashString):]
            if errstr.rfind("SQL Server does not exist or access denied") != -1:
                raise Exceptions.AccessDenied(self.connectionString)
        self.com = DMOServer

    def add_network_alias(self, aliasName, connectionString):
        setValue = "DBMSSOCN,%s" % connectionString
        keyName1 = r"SOFTWARE\Microsoft\MSSQLServer\Client\ConnectTo"
        key1 = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                             keyName1, 0, winreg.KEY_SET_VALUE)
        winreg.SetValueEx(key1, aliasName, 0, winreg.REG_SZ, setValue)
        keyName2 = r"SOFTWARE\Microsoft\MSSQLServer\Client\TDS"
        key2 = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                             keyName2, 0, winreg.KEY_SET_VALUE)
        winreg.SetValueEx(key2, connectionString, 0, winreg.REG_SZ, "7.0")

    def add_linked_server(self, aliasName, username, password):
        linkedServerCollection = self.fetch_path("LinkedServers")
        linkedServer = win32com.client.Dispatch("SQLDMO.LinkedServer")
        linkedServer.Name = aliasName
        linkedServer.ProviderName = "SQLOLEDB"
        linkedServer.DataSource = aliasName
        linkedServer.ProductName = "SQL Server"
        linkedServer.DropLogins = False
        try:
                linkedServerCollection.com.Add(linkedServer)
        except pywintypes.com_error, e:
                errorString = e.args[2][2]
                if errorString.rfind("already exists") != -1:
                        return
                raise e
        lsl = self.com.LinkedServers[aliasName].LinkedServerLogins
        defaultLogin = lsl[0]
        defaultLogin.RemoteUser = username
        defaultLogin.RemotePassword = password

    def fetch_path(self, path):
        nodes = path.split(".")

        cursor = self

        for node in nodes:
            cursor = cursor[node]

        return cursor

    def exec_query(self, database, queryString="", queryPath="", verbose=False):
        if queryPath:
            cmd = 'osql -o output.txt -E -n -d %s -i "%s" -S %s' % (database, queryPath, self.connectionString)
            if verbose:
                self.logger.info( "===  EXECUTING %s" %cmd )
            status = os.system(cmd)
            if status == FAIL:
                return FAIL
            output = open("output.txt", 'r').read()
            if verbose:
                self.logger.info( output )
            os.unlink("output.txt")
            if output.startswith("Msg 2714"):
                return OK
            elif output.startswith("Msg"):
                #self.logger.debug( "You can probably ignore this: %s " % output )
                return FAIL
            return OK
        database = self.fetch_path("Databases.%s" %database)
        #print "QUERYString:", queryString
        query = database.com.ExecuteWithResults(queryString)
        return SQLDMOQuery2.SQLDMOQuery(query)
