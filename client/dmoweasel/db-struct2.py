#!/cygdrive/c/Python24/python.exe
# Version 0.41-156

# db-struct.py: This executable is for loading or dumping database
# structure (table schema, indexes, triggers, stored procedures, and
# jobs) independent of the data

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

import dmoweasel.dbprocess2 as dbprocess2
import dmoweasel.SQLDMOServer as SQLDMOServer

from getopt import getopt
import sys

OK   = 0
FAIL = 1

def usage():
    usageString = \
"""db-struct.py will load or unload a Microsoft SQL database structure (table schema)
and stored procedures to and from the filesystem as a set of files in a directory structure.

Usage: %(progName)s [OPTION] [SERVER] [DATABASE]

Examples:
    %(progName)s -d -v -u su -p mypasswd sless01 OracleExports # Dump OracleExports on SLESS01
    %(progName)s -l kwmqadb01\\CDBA,1433 lenidb                # Load lenidb onto kwmqadb01, instance CDBA

Main operation mode:
    -d         dump a database to the repository directory
    -l         load a database from the repository directory

Operation modifiers:
    -u         specify the username that should be used to access the database
    -p         specify the password for that user
    -v         list each table as it is processed
    -r         specify the repository directory, defaults to current directory

Report bugs to seth.delisle@ge.com
""" % {"progName": sys.argv[0]}
    sys.stderr.write(usageString)
    sys.exit(1)


if __name__ == '__main__':

    import bombardier.Logger
    logger = bombardier.Logger.Logger()

    opts, args = getopt(sys.argv[1:], 'u:p:dlvr:')
    try:
        serverName = args[0] 
        databaseName = args[1]
    except IndexError:
        usage()

    config = {'direction': "dump", 'verbose': False,
              'user':None, 'password':None, 'repository':serverName}

    for o, a in opts:
        if o=="-d":
            config['direction'] = "dump" 

        elif o =='-l':
            config['direction'] = "load"

        elif o =='-v':
            config['verbose'] = True

        elif o =='-r':
            config['repository'] = a

        elif o =='-u':
            config['user'] = a

        elif o =='-p':
            config['password'] = a

    config['connectionString'] = serverName
    server = SQLDMOServer.SQLDMOServer(config, logger)
    status = dbprocess2.process_structure(config, databaseName, logger)
    sys.exit( status )
