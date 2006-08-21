#!/cygdrive/c/Python24/python.exe
# Version 0.41-168

# UserManager.py: Handles users in the database.

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

import Exceptions
import win32com.client
import sys, os, pywintypes

class UserManager:
    def __init__(self, repository, server, databaseName):
        self.passwd = os.path.join(repository, 'Databases',
                       databaseName, '__passwd')

        self.owner = os.path.join(os.path.dirname(self.passwd), '__owner')

        self.server=server

        self.database = server.Databases[databaseName]
        database=self.database

        self.logins = map(lambda x: x.Name, server.Logins)
        self.users = map(lambda x: x.Name, database.Users)

    def add_user(self, user, login=None):
        newUser = win32com.client.Dispatch("SQLDMO.User")
        newUser.Name = user

        if login:
            newUser.Login = login
        else:
            newUser.Login = user

        self.database.Users.Add(newUser)

    def add_login(self, user, password):
        newLogin = win32com.client.Dispatch("SQLDMO.Login")
        newLogin.Name = user

        if password == "[NT]":
            newLogin.Type = 0

        elif password == "[NT_GROUP]":
            newLogin.Type = 1

        else:
            newLogin.Type = 2

        try:
            self.server.Logins.Add(newLogin)
        except pywintypes.com_error, e:
            print e[2]

        if newLogin.Type == 2:
            newLogin.SetPassword("", password)

    def dump_users(self, logger):
        database = self.database
        users = database.Users

        userNames = map(lambda x: x.Name, users)
        loginNames = map(lambda x: x.Login, users)
        owner = self.database.Owner

        if users:
            dirname = os.path.dirname(self.passwd)
            if not os.path.isdir(dirname):
                os.makedirs(dirname)
            pwFile = open(self.passwd, 'w')
            ownerFile = open(self.owner, 'w')
            ownerFile.write(owner)

            for user, login in zip(userNames, loginNames):
                if login:
                    logger.info("dumping login: %s" % login)
                    loginObject = self.server.Logins[login]
                elif user:
                    loginObject = None
                if loginObject and loginObject.Type == 0:
                    logger.info("dumping NT user: %s" % user)
                    pwFile.write(user + ':' + owner + ':' + "[NT]" + '\n')
                elif loginObject and loginObject.Type == 1:
                    logger.info("dumping NT group: %s" % user)
                    pwFile.write(user + ':' + owner + ':' + "[NT_GROUP]" + '\n')
                else:
                    logger.info("dumping SQL user: %s" % user)
                    pwFile.write(user + ':' + ':' + '\n')
        else:
            logger.info("No users to dump")

    def load_users(self, logger):
        if os.path.isfile(self.owner):
            ownerLogin = open(self.owner).read()
        else:
            logger.info("No %s file, not loading owner" % self.owner)
            ownerLogin = None
        if os.path.isfile(self.passwd):
            passwd = open(self.passwd)
            while 1:
                userpass = passwd.readline().strip()
                if not userpass:
                    break
                user, login, password = userpass.split(':')
                if user in self.users:
                    pass
                elif login in self.logins:
                    logger.info("Adding login: %s" % user)
                    self.add_user(user, login)
                elif user in self.logins:
                    logger.info("Adding user: %s" % user)
                    self.add_user(user)
                elif not password:
                    logger.info("Skippnig %s, because there is no password" % user)
                    pass
                else:
                    logger.info("Adding user and login: %s" % user)
                    self.add_login(user, password)
                    self.add_user(user)
        else:
            logger.info("No %s file, not loading users" % self.passwd)
        
        if ownerLogin in self.logins:
            self.database.SetOwner(ownerLogin, False, True)

if __name__ == '__main__':
    import SQLDMOServer
    from getopt import getopt
    import bombardier.Logger
    logger = bombardier.Logger.Logger()

    config = {'direction':'dump', 'user':None, 'password':None}

    opts, args = getopt(sys.argv[1:], 'ldu:p:')

    for o, a in opts:
        if o == '-l':
            config['direction'] = 'load'

        elif o == '-d':
            config['direction'] = 'dump'

        elif o == '-u':
            config['user'] = a

        elif o == '-p':
            config['password'] = a 

    repository = args[0]
    if config['user'] and config['password']:
        server = SQLDMOServer.SQLDMOServer(args[1], config['user'],
                                           config['password'])

    elif config['user'] or config['password']:
        errmsg = "Both user and password required for SQL authentication"
        raise Exceptions.ConfigurationError(errmsg)

    else:
        server = SQLDMOServer.SQLDMOServer(args[1], config['user'],
                                           config['password'])
    databaseName = args[2]

    um = UserManager(repository, server.com, databaseName)

    if config['direction'] == 'load':
        um.load_users(logger)
    else:
        um.dump_users(logger)
