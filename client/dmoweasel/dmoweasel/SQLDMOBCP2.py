#!/cygdrive/c/Python24/python.exe

# SQLDMOBCP.py: This library is used for copying data into and out of
# a database.

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

from getopt import getopt
import os
import win32com.client

OK   = 0
FAIL = 1

def dumpOutput():
    output = open("outfile.txt", 'r').read().strip()
    if output:
        print "============================= BCP OUTPUT"
        print output
    errors = open("errfile.txt", 'r').read().strip()
    if errors:
        print "============================= BCP ERRORS"
        print errors
        print "============================= "

class SQLDMOBCP:

    def __init__(self, fullPath, server):
        self.server = server
        self.fullPath = fullPath

    def addAuthString(self):
        username = self.server.username
        password = self.server.password
        if self.server.username == None:
            authString = " -T"
        else:
            authString = " -U %s -P %s" % (username, password)
        return authString

    def load_table(self, databaseName, table, verbose=False):
        authString = self.addAuthString()
        tableName  = str(table)
        tableName  = tableName[:tableName.rfind('/')]
        cs         = self.server.connectionString
        dataPath   = os.path.join(self.fullPath, 'Data')
        fmtPath    = os.path.join(self.fullPath, 'format.fmt')
        if not os.path.isfile(dataPath):
            print "Unable to locate data file %s" % dataPath
            return FAIL
        cmd = ""
        if os.path.isfile(fmtPath):
            cmdString = 'bcp "%s..%s" in %s -S %s -f %s %s -o outfile.txt -e errfile.txt'
            cmd = cmdString % ( databaseName, tableName, dataPath, cs, fmtPath, authString )
        else:
            print "(Not using format file because it could not be found)"
            cmdString = 'bcp "%s..%s" in %s -S %s -n %s -o outfile.txt -e errfile.txt'
            cmd = cmdString % ( databaseName, tableName, dataPath, cs, authString )
        if verbose:
            print "EXECUTING: %s" % cmd
        status     = os.system(cmd)
        if verbose:
            dumpOutput()
        return status # useless

    def dump_table(self, databaseName, table, verbose=False):
        cs         = self.server.connectionString
        authString = self.addAuthString()
        tableName  = str(table)
        tableName  = tableName[:tableName.rfind('/')]
        dataPath   = os.path.join(self.fullPath, 'Data')
        fmtPath    = os.path.join(self.fullPath, 'format.fmt')
        cmd1       = 'bcp "%s..%s" format fmt.txt -n -S %s -f %s %s -o outfile -e errfile' % ( databaseName, tableName, cs, fmtPath, authString )
        cmd2       = 'bcp "%s..%s" out "%s" -n -S %s %s -o outfile -e errfile' % ( databaseName, tableName, dataPath, cs, authString )
        if verbose:
            print "(1) EXECUTING: %s" % cmd1
        status1  = os.system(cmd1)
        if verbose:
            dumpOutput()
        if status1 == OK:
            if verbose:
                print "(2) EXECUTING: %s" % cmd2
            status2  = os.system(cmd2)
            if verbose:
                dumpOutput()
            if status2 == OK:
                return OK

