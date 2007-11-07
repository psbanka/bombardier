#!/cygdrive/c/Python24/python.exe

# Linux.py: Class to wrap all Linux OS commands.  level activity.

# Copyright (C) 2005 Peter Banka

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

import OperatingSystem, os
import Logger
from staticData import *
import re
from commands import getstatusoutput

def md5crypt(password, salt='', magic='$1$'):
    import md5, random
    # Based on FreeBSD src/lib/libcrypt/crypt.c 1.2
    # http://www.freebsd.org/cgi/cvsweb.cgi/~checkout~/src/lib/libcrypt/crypt.c?rev=1.2&content-type=text/plain

    # Original license:
    # * "THE BEER-WARE LICENSE" (Revision 42):
    # * <phk@login.dknet.dk> wrote this file.  As long as you retain this notice you
    # * can do whatever you want with this stuff. If we meet some day, and you think
    # * this stuff is worth it, you can buy me a beer in return.   Poul-Henning Kamp

    # This port adds no further stipulations.  I forfeit any copyright interest.
    itoa64 = './0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    if not salt:
        salt = ''.join(random.sample(itoa64, 8))
    m = md5.new()
    m.update(password + magic + salt)
    mixin = md5.md5(password + salt + password).digest()
    for i in range(0, len(password)):
        m.update(mixin[i % 16])
    i = len(password)
    while i:
        if i & 1:
            m.update('\x00')
        else:
            m.update(password[0])
        i >>= 1
    final = m.digest()
    for i in range(1000):
        m2 = md5.md5()
        if i & 1:
            m2.update(password)
        else:
            m2.update(final)
        if i % 3:
            m2.update(salt)
        if i % 7:
            m2.update(password)
        if i & 1:
            m2.update(final)
        else:
            m2.update(password)
        final = m2.digest()
    rearranged = ''
    for a, b, c in ((0, 6, 12), (1, 7, 13), (2, 8, 14), (3, 9, 15), (4, 10, 5)):
        v = ord(final[a]) << 16 | ord(final[b]) << 8 | ord(final[c])
        for i in range(4):
            rearranged += itoa64[v & 0x3f]; v >>= 6
    v = ord(final[11])
    for i in range(2):
        rearranged += itoa64[v & 0x3f]; v >>= 6
    return magic + salt + '$' + rearranged

def getUserAliases(data):
    c = re.compile("User_Alias (\S+) += *(.+)")
    output = {"ADMINS":set(), "DEVS":set()}
    groups = c.findall(data)
    for group in groups:
        groupName = group[0]
        output[groupName] = set([ x.strip() for x in group[1].split(',') ])
    return output

def setUserAliases(data, aliases):
    c = re.compile("User_Alias (\S+) += *(.+)")
    output = []
    for groupName in aliases:
        output.append("User_Alias %s = %s" % (groupName, ', '.join(aliases[groupName])))
    for line in data.split('\n'):
        groups = c.findall(line)
        if not groups:
            output.append(line)
    return output

def modifySudoers(username, type, filename="/etc/sudoers", remove=False):
    data = open(filename).read()
    aliases = getUserAliases(data)
    Logger.info("This user is of type (%s)" % type)
    if remove:
        for groupName in aliases:
            if username in aliases[groupName]:
                aliases[groupName].remove(username)
    else:
        if type == ADMIN_USER:
            aliases["ADMINS"].add(username)
        if type == DEV_USER:
            aliases["DEVS"].add(username)
    open(filename, 'w').write('\n'.join(setUserAliases(data, aliases)))


class Linux(OperatingSystem.OperatingSystem):

    """Provides capabilities of operating system-level functions for Linux machines"""

    def __init__(self):
        OperatingSystem.OperatingSystem.__init__(self)

    def run(self, fullCmd, abortIfTold, workingDirectory, console = False):
        status = OK
        abortIfTold()
        if fullCmd.split(' ')[0].endswith(".py"):
            fullCmd = "%s %s" % ("/usr/local/bin/python2.4", fullCmd)
        elif fullCmd.split(' ')[0].endswith(".sh"):
            fullCmd = "%s %s" % ("bin/bash %s" % fullCmd)
        else:
            Logger.error("unknown command type %s" % `fullCmd`)
            return FAIL
        status = self.execute(fullCmd, errorString="Unable to execute %s" % fullCmd,
                              workingDirectory = workingDirectory, captureOutput = True)
        return status
    
    def listAllUsers(self):
        status, output = getstatusoutput( "getent passwd | awk -F : '{print$1}'" )
        if status != OK:
            raise Exception( "Could not get user list." )
        return output.split( '\n' )
        
    def removeUser(self, username):
        modifySudoers(username, type=0, remove=True)
        status,output = getstatusoutput('userdel %s 2>&1' %username )
        if status != OK:
            raise Exception( "Error deleting %s: %s" %(username, output) )
        return status

    def createUser(self, username, password, type, comment=''):
        md5Pass = md5crypt(password)
        if type not in [SSH_USER, ADMIN_USER, DEV_USER]:
            shell = "/bin/false"
        else:
            shell = "/bin/bash"
        cmd = "useradd -p '%s' -s %s %s" % (md5Pass, shell, username)
        status,output = getstatusoutput(cmd)
        if status != OK:
            raise Exception( "Error creating %s: %s" %(username, output) )
        modifySudoers(username, type)
        return status
   
    def changeLocalUserPassword(self, username, password):
        if self.checkLocalUserCredentials(username, password) != OK:
            hashStr = md5crypt( password )
            cmd = "echo '%s:%s' | chpasswd -e" %(username, hashStr)
            status,output = getstatusoutput(cmd)
            if status != OK:
                Logger.error( "Error creating %s: %s" %(username, output) )
                return FAIL
        return OK
        
    def checkLocalUserCredentials(self, username, password):
        if password == '':
            return OK
        shadowLines = open('/etc/shadow').readlines()
        for shadowLine in shadowLines:
            name = shadowLine.split( ':' )[0]
            if name == username:
                foundHashStr = shadowLine.split( ':' )[1]
                if not '$' in foundHashStr:
                    return FAIL
                salt = foundHashStr.split('$')[2]
                break
        if not foundHashStr:
            Logger.error( "Error getting hash string for user: %s" %username )
            return FAIL
        newHashStr =  md5crypt( password, salt )
        if newHashStr != foundHashStr:
            return FAIL
        return OK

    def noRestartOnLogon(self): 
        pass

    def testConsole(self):
        return OK

    def noAutoLogin(self): 
        pass
