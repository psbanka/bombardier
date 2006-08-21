#!/cygdrive/c/Python24/python.exe
# Version 0.41-168

# db-data2.py: This executable is for dumping or loading data from a
# database independent of its structure.

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
import sys

import dmoweasel.dbprocess2 as dbprocess2
import dmoweasel.SQLDMOServer as SQLDMOServer

def usage():
    print """Usage: %s [options] server database
Options:
  -d	 specifies that this operation should dump data 
	 from the db (default) to the repository directory
  -l 	 specifies that this option should load data 
	 to the db from the repository directory
  -t	 truncates data from the database (destructive operation)
  -v	 verbose mode
  -r	 specifies the repository directory, defaults to '.\\servername'
  -u	 username of database user
  -p	 password of database user

server	 connection string for the server you're attempting to reach

database name of the database you want to interact with""" % sys.argv[0]
    sys.exit(1)

if __name__ == '__main__':
    opts         = []
    args         = []
    serverName   = ''
    databaseName = ''
    import bombardier.Logger as Logger
    logger = Logger.Logger()

    try:
    	opts, args = getopt(sys.argv[1:], 'du:p:ltvr:')
        serverName = args[0] 
        databaseName = args[1]
    except:
	usage()

    config = {'direction': "dump", 'truncate': False, 'verbose': False,
              'repository': serverName, "username":None, "password":None}

    for o, a in opts:
        if o=="-d":
            config['direction'] = "dump" 

        elif o =='-l':
            config['direction'] = "load"

        elif o =='-t':
            config['truncate'] = True

        elif o =='-v':
            config['verbose'] = True

        elif o =='-r':
            config['repository'] = a

        elif o =='-u':
            config['username']  = a

        elif o =='-p':
            config['password'] = a

    tableName = ''
    if len(args) > 2:
        tableName = args[2]

    if config['username'] or config['password']:
        if not config['username'] and config['password']:
            print "Can't have username without password and vice versa/n/n"
            usage()
    config['connectionString'] = serverName
    status = dbprocess2.process_data(config, databaseName, logger)
    sys.exit(status)
