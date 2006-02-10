#!/cygdrive/c/Python24/python.exe

# db-users.py: This executable is for dumping and loading user
# information

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

import sys
from getopt import getopt

def usage():
    print """usage: %s [options] <servername> <database>
    options:

      direction:
      -l   : load users from the repository to the db
      -d   : dump user information from the database to the repository

      authentication:
      -u   : username
      -p   : password

      miscellaneous:
      -r   : specify the location of the repository. The repository is
             the directory structure which holds database information.
             This defaults to the current directory.
      -v   : verbose

    servername: the connection string to the SQL server

    database: the name of the database to dump or load
      
    """ % sys.argv[0]
    sys.exit(1)

if __name__ == "__main__":
    import bombardier.Logger
    logger = bombardier.Logger.Logger()

    opts, args = getopt(sys.argv[1:], 'ldu:p:r:')

    serverName = args[0]
    database = args[1]

    config = {'direction':'dump', 'repository':serverName,
              'user':None, 'password':None, 'verbose':False}

    for o, a in opts:
        if o == '-l':
            config['direction'] = 'load'
        elif o == '-d':
            config['direction'] = 'dump'
        elif o == '-r':
            config['repository'] = a 
        elif o == '-u':
            config['user'] = a
        elif o == '-p':
            config['password'] = a
        elif o == '-v':
            config['verbose'] = True

    config['connectionString'] = serverName
    dbprocess2.process_users(config, database, logger)

