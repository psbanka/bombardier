#!/cygdrive/c/Python24/python.exe

# db-browser.py: This executable is for browsing a database store for
# the purpose of discovering its contents

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


import dmoweasel.SQLDMOServer as SQLDMOServer
import dmoweasel.SQLDMOCollection as SQLDMOCollection
import dmoweasel.Exceptions as Exceptions

from getopt import getopt
import sys

# FIXME: Need to factor this out to dbprocess!
def parse_command_line():

    availableOpts = {'-u': 'username',
                     '-p': 'password',
                     '-s': 'script',
                     '-S': 'systemObjects'}

    availableArgs = ['server', 'path'] 
    config = {} 
    opts, args = getopt(sys.argv[1:], 'u:p:Ss')

    for o, a in opts:
        if availableOpts.has_key(o):
            config[availableOpts[o]] = a
        else:
            raise Exceptions.CommandLineError 

    for i in range(0, len(args)):
        try:
            config[availableArgs[i]] = args[i] 
        except:
            raise Exceptions.CommandLineError

    if not config.has_key('server'):
        raise Exceptions.CommandLineError("You _must_ specify a server")

    elif config.has_key('username') or config.has_key('password'):
        if not config.has_key('username') and config.has_key('password'):
            warning = "Username and password required " \
                      "for SQL authentication"
            raise Exceptions.CommandLineError(warning)
        else:
            server = SQLDMOServer.SQLDMOServer(config['server'],
                                               config['username'],
                                               config['password']) 
    else:
        server = SQLDMOServer.SQLDMOServer(config['server'])
    if config.has_key('path'):
        obj = server.fetch_path(config['path'])
    else:
        obj = server
    if config.has_key('script'):
        try:
            print obj.com.Script()
        except AttributeError:
            sys.stderr.write("Error: Unscriptable object")

    elif isinstance(obj, SQLDMOCollection.SQLDMOCollection):
        for key, value in obj.items():
            if config.has_key('systemObjects'):
                print key
            elif ( hasattr(value.com, 'SystemObject') and 
                 not value.com.SystemObject):
                 print key
    else:
        print obj

if __name__ == "__main__":
    try:
        parse_command_line()
    except Exceptions.CommandLineError, e:
       sys.stdout.write(`e` + '\n')

