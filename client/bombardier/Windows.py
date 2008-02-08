#as;fhaskfjh!/cygdrive/c/Python24/python.exe

# Windows.py: This is used to simplify and wrap all Windows activity,
# and, similar to Filesystem.py and Server.py enables mocking of this
# activity for better unit-testing.

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

import _winreg as winreg
import win32net, win32netcon, win32con, win32security, win32service, win32file, winerror
import pywintypes, win32api, win32serviceutil, servicemanager, win32event, ntsecuritycon
import win32pipe, win32com.client, pythoncom
import win32net
import win32netcon

import threading, os, time, traceback

import miniUtility, Logger, RegistryDict, Exceptions, OperatingSystem
from win32process import CreateProcess, NORMAL_PRIORITY_CLASS, STARTUPINFO

from staticData import *

LOGIN_KEY_NAME = 'SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Winlogon'

keyMapping = {"HKLM": winreg.HKEY_LOCAL_MACHINE,
              "HKEY_LOCAL_MACHINE": winreg.HKEY_LOCAL_MACHINE,
              "HKKR": winreg.HKEY_CLASSES_ROOT,
              "HKEY_CLASSES_ROOT": winreg.HKEY_CLASSES_ROOT,
              "HKCU": winreg.HKEY_CURRENT_USER,
              "HKEY_CURRENT_USER": winreg.HKEY_CURRENT_USER,
              "HKU": winreg.HKEY_USERS,
              "HKEY_USERS": winreg.HKEY_USERS,
              "HKCC": winreg.HKEY_CURRENT_CONFIG,
              "HKEY_CURRENT_CONFIG": winreg.HKEY_CURRENT_CONFIG}

class guiThread(threading.Thread):

    def __init__(self):
        threading.Thread.__init__(self)
        time.sleep(1)
        self.ws = win32com.client.Dispatch("WScript.Shell")

    def run(self):
        self.ws.popup(TEST_TITLE, 10, TEST_TITLE)

def findInitialKey(path):
    initialKeyString = path.split('\\')[0]
    initialKey = keyMapping.get(initialKeyString.upper())
    if initialKey == None:
        initialKey = winreg.HKEY_LOCAL_MACHINE
    else:
        path = "\\".join(path.split("\\")[1:]) # pull the first one off
    return initialKey, path


class Windows(OperatingSystem.OperatingSystem):

    """This is another abstraction class to wrap all activities which
    require interaction with the system registry. -pbanka"""

    def __init__(self):
        OperatingSystem.OperatingSystem.__init__(self)

    def ShellExecuteSimple(self, runCmd):
        return win32api.ShellExecute(0, "open", runCmd, None, '', 1)

    def spawnReturnId( self, cmd ):
        procTuple = CreateProcess( None, cmd, None, None, 0, 
                                   NORMAL_PRIORITY_CLASS, os.environ, None, STARTUPINFO() )
        return procTuple

    def runProcess( self, workingDirectory, cmd ):
        fullCommand = 'cmd /c %s: && cd "%s" && %s' %( workingDirectory[0], workingDirectory, cmd )
        return self.spawnReturnId( fullCommand )

    def beginConsole(self): 
        Logger.info("Beginning monitored console-based installation")
        consoleFile = os.path.join(miniUtility.getSpkgPath(),CONSOLE_MONITOR)
        f = open(consoleFile, 'w')
        f.close()

    def watchForTermination(self, sleepTime = 10.0, timeout = 600, abortIfTold=None):
        start = time.time()
        consoleFile = os.path.join(miniUtility.getSpkgPath(),CONSOLE_MONITOR)
        logTime = 0
        while True:
            if abortIfTold != None:
                abortIfTold()
            elapsedTime = time.time() - start
            if elapsedTime > timeout:
                Logger.debug("%d seconds have elapsed (max "\
                             "of %d). Giving up." % (elapsedTime, timeout))
                return FAIL
            if not os.path.isfile(consoleFile):
                return FAIL
            data = open(consoleFile, 'r').read().strip()
            if data:
                if int(data) == OK:
                    return OK
                elif int(data) == FAIL:
                    return FAIL
                elif int(data) == REBOOT:
                    return REBOOT
                Logger.error("Invalid status received from log file: %d" % int(data))
                return FAIL
            left = timeout - elapsedTime
            if time.time() > logTime:
                Logger.debug("Watching status of console installation (%3.1f)" % left)
                logTime = time.time( ) + LOG_INTERVAL
            time.sleep(sleepTime)
        return FAIL

    def run(self, fullCmd, abortIfTold, workingDirectory, console = False):
        status = OK
        abortIfTold()
        if console:
            self.beginConsole()
        if fullCmd.split(' ')[0].endswith(".py"):
            pythonCmd = os.path.join(sys.prefix, "python.exe")
            fullCmd   = "%s %s" % (pythonCmd, fullCmd)
        elif fullCmd.split(' ')[0].endswith(".sh"):
            fullCmd   = "bash %s" % fullCmd
        else:
            Logger.error("unknown command type %s" % `fullCmd`)
            return FAIL
        if console:
            if self.runProcess(workingDirectory, fullCmd) == FAIL:
                return FAIL
            return self.watchForTermination(sleepTime=1, abortIfTold=abortIfTold)
        status = self.execute(fullCmd, errorString="Unable to execute %s" % fullCmd,
                              workingDirectory = workingDirectory, captureOutput = True)
        return status

    def OpenSCManager(self):
        hscm = None
        try:
            hscm = win32service.OpenSCManager(None, None,
                                              win32service.SC_MANAGER_ALL_ACCESS)
        except:
            pass
        return hscm

    def OpenService(self, serviceName, hscm = None):
        if hscm == None:
            hscm = self.OpenSCManager()
        if hscm == None:
            return None
        serviceHandle = None
        try:
            serviceHandle = win32service.OpenService(hscm, serviceName,
                                                     win32service.SERVICE_ALL_ACCESS)
        except:
            pass
        return serviceHandle

    ### TESTED
    def serviceStatus(self, serviceName):
        hscm          = self.OpenSCManager()
        serviceHandle = self.OpenService(serviceName)
        if serviceHandle == None:
            raise Exceptions.ServiceNotFound, serviceName
        serviceInfo   = win32service.QueryServiceStatus(serviceHandle)
        win32service.CloseServiceHandle(serviceHandle)
        win32service.CloseServiceHandle(hscm)
        status = PENDING
        timeout = 30
        while status == PENDING and timeout > 0:
            time.sleep(1)
            status = self.evalStatus(serviceInfo)
            timeout -= 1
        if status == RUNNING:
            return OK
        return FAIL

    def evalStatus(self, serviceInfo):
        svcType, svcState, svcControls, err, svcErr, svcCP, svcWH = serviceInfo
        if svcState==win32service.SERVICE_STOPPED:
            return STOPPED
        elif svcState==win32service.SERVICE_START_PENDING:
            return PENDING
        elif svcState==win32service.SERVICE_STOP_PENDING:
            return NOT_RUNNING
        elif svcState==win32service.SERVICE_RUNNING:
            return RUNNING
        return NOT_RUNNING

    def restartService(self, serviceName):
        status = self.stopService(serviceName)
        if status == FAIL:
            return FAIL
        status = self.startService(serviceName)
        return status    

    ### TESTED
    def stopService(self, serviceName):
        serviceHandle = self.OpenService(serviceName)
        if self.serviceStatus(serviceName) == OK:
            status = win32service.ControlService(serviceHandle,
                                                 win32service.SERVICE_CONTROL_STOP)
            return status
        return FAIL
    
    def startService(self, serviceName):
        hscm          = self.OpenSCManager()
        serviceHandle = self.OpenService(serviceName)
        if serviceHandle == None:
            raise Exceptions.ServiceNotFound
        if self.serviceStatus(serviceName) == OK:
            win32service.CloseServiceHandle(serviceHandle)
            win32service.CloseServiceHandle(hscm)
            return OK
        try:
            win32service.StartService(serviceHandle, None)
        except Exception, e:
            win32service.CloseServiceHandle(serviceHandle)
            win32service.CloseServiceHandle(hscm)
            if e[-1].rfind('already running') != -1:
                return OK
            return FAIL
        win32service.CloseServiceHandle(serviceHandle)
        win32service.CloseServiceHandle(hscm)
        return OK

    def installService( self, _svcPath, domain=None, userName=None, password=None, version="v1.1.4322" ):
        cmd = os.path.join(os.environ['windir'], "Microsoft.Net", 
                           "Framework", version, "Installutil.exe")
        if( userName != None ) and ( password != None ):
            if domain == '.' or domain == '' or domain == None:
                domain = os.environ['hostname']
            cmd += " /userName=%s\\%s /password=%s" %(domain, userName, password)
        Logger.info( "cmd = %s" %(cmd) )
        Logger.info( "_svcPath = %s" %(_svcPath)  )
        os.system("%s %s" % (cmd, _svcPath), "Unable to install " + _svcPath + " as a service", debug=1)

    def registerDlls(self, directory, dlls):
        Logger.info("Registering DLLs..")
        winDir = os.environ["windir"]
        status = OK
        for dll in dlls:
            dllPath = os.path.join(directory, "dll", dll)
            cmd = "%s %s /s" % (os.path.join(winDir, "system32", "regsvr32.exe"), dllPath)
            status = os.system(cmd)
            if status == FAIL:
                status = FAIL
                Logger.warning("Error registering %s." % dll)
        return status

    def unregisterDlls(self, directory, dlls):
        Logger.info("Un-registering DLLs..")
        winDir = os.environ["windir"]
        status = OK
        for dll in dlls:
            dllPath = os.path.join(directory, "dll", dll)
            cmd = "%s %s /s /u" % (os.path.join(winDir, "system32", "regsvr32.exe"), dllPath)
            status = os.system(cmd)
            if status == FAIL:
                Logger.warning("Error un-registering %s." % dll)
                status = FAIL
        return status

    def listAllUsers(self):
        server   = r'\\localhost'
        res      = 1
        users    = []
        userDict = {}
        newUsers = True

        while newUsers:
            newUsers = False
            (users,total,res2) = win32net.NetUserEnum(server,3,
               win32netcon.FILTER_NORMAL_ACCOUNT,res,win32netcon.MAX_PREFERRED_LENGTH)
            for u in users:
                login=str(u['name'])
                if login in userDict.keys():
                    continue
                newUsers = True
                infoDict=win32net.NetUserGetInfo(server, login, 3)
                fullName=str(infoDict['full_name'])
                userDict[login] = {"fullName": fullName, 
                                   "comment": str(u['comment']),
                                   "lastLogon": time.asctime(time.localtime(int(u["last_logon"]))),
                                   "passwordExpired": u["password_expired"],
                                   "homeDirectory": str(infoDict["home_dir"]),
                                   "expires": u["acct_expires"],
                                   "primaryGroup": u['primary_group_id'],
                                   "passwordAge": u['password_age'] }
            res = res2
        return userDict

    def removeScriptUser(self, username):
        win32net.NetUserDel(None, username)
        return OK

    def removeUser(self, username):
        win32net.NetUserDel(None, username)
        return OK

    def createUser(self, username, password, type, comment=''):
        username = username.strip().replace(' ', '.')
        if comment == None:
            comment = ''
        if type == SSH_USER:
            combinedFlags = win32netcon.UF_DONT_EXPIRE_PASSWD | \
                            win32netcon.UF_NORMAL_ACCOUNT
        else:
            combinedFlags = win32netcon.UF_NORMAL_ACCOUNT
            
        d = {'name': username,
             'password': password.strip(),
             'comment': comment.strip(),
             'flags': combinedFlags,
             'priv': win32netcon.USER_PRIV_USER}
        try:
            win32net.NetUserAdd(None, 1, d)
        except pywintypes.error, e:
            if e[2] != "The account already exists.":
                Logger.error("Unable to create user '%s'. [%s]" % (username, e[2]))
                return FAIL
        if type == ADMIN_USER:
            Logger.info("Setting user %s as administrator" % username)
            try:
                win32net.NetLocalGroupAddMembers(None, 'administrators',
                                                 3, [{"domainandname":username}])
            except pywintypes.error, e:
                if e[2].startswith('The specified account name is already'):
                    ermsg = "Unable to assign proper permissions to "\
                            "user %s [%s]", (username, e[2])
                    Logger.error(ermsg)
                    return FAIL
        if type == RDP_USER:
            Logger.info("Setting user %s as rdp user..." % username)
            try:
                win32net.NetLocalGroupAddMembers(None, 'Remote Desktop Users',
                                                 3, [{"domainandname":username}])
            except pywintypes.error, e:
                if e[2].startswith('The specified account name is already'):
                    ermsg = "Unable to assign proper permissions to "\
                            "user %s [%s]", (username, e[2])
                    Logger.error(ermsg)
                    return FAIL
        return OK


    def createScriptUser(self, username, password):
        combinedFlags = win32netcon.UF_DONT_EXPIRE_PASSWD | \
                        win32netcon.UF_PASSWD_CANT_CHANGE | \
                        win32netcon.UF_NORMAL_ACCOUNT
        d = {'name': username,
             'password': password,
             'comment': "Bombardier administrative user",
             'flags': combinedFlags,
             'priv': win32netcon.USER_PRIV_USER}
        try:
            win32net.NetUserAdd(None, 1, d)
        except pywintypes.error, e:
            if e[2] != "The account already exists.":
                Logger.error("Unable to create user '%s'. [%s]" % (username, e[2]))
                return FAIL
        try:
            win32net.NetLocalGroupAddMembers(None, 'administrators',
                                             3, [{"domainandname":username}])
        except pywintypes.error, e:
            if e[2].startswith('The specified account name is already'):
                ermsg = "Unable to assign proper permissions to "\
                        "user %s [%s]", (username, e[2])
                Logger.error(ermsg)
                return FAIL
        return OK

    def setGroups(self, userid, rights):
        groupList = win32net.NetUserGetLocalGroups("localhost", userid) 
        rightsMapping = {"admin": u"Administrators", "rdp": u"Remote Desktop Users"}
        for right in rights:
            if right not in rightsMapping:
                print "Unknown right: %s" % right
                continue
            if right in rights:
                if not rightsMapping[right] in groupList:
                    win32net.NetLocalGroupAddMembers(None, rightsMapping[right], 3, [{"domainandname":userid}])
                    print "Adding user membership to %s" % rightsMapping[right]
            else:
                if rightsMapping[right] in groupList:
                    win32net.NetLocalGroupDelMembers(None, rightsMapping[right], [userid])
                    print "removing user membership from %s" % rightsMapping[right]
            
    def changeLocalUserPassword(self, user, password):
        try:
            ads = win32com.client.GetObject("ADs:")
            userObject = ads.GetObject('','WinNT://localhost/%s' %user)
            userObject.SetPassword(password)
            userObject.SetInfo()
        except:
            Logger.error('Could not change password for user: %s' %user)
            return FAIL
        return OK
        
    def checkLocalUserCredentials(self, username, password):
        if password in [ '', CENSORED ]:
            return OK
        try:
             win32security.LogonUser( username, '.', password,
                                      win32security.LOGON32_LOGON_INTERACTIVE,
                                      win32security.LOGON32_PROVIDER_DEFAULT  )
        except:
            return FAIL
        return OK

    def LogMsg(self, type, event, info):
        try:
            return servicemanager.LogMsg(type, event, info)
        except:
            pass
        return None

    def ServiceFrameworkInit(self, serviceObject, args):
        win32serviceutil.ServiceFramework.__init__(serviceObject, args)

    def WaitForMultipleObjects(self, readers, writers, timeout):
        win32event.WaitForMultipleObjects(readers, writers, timeout)

    def ReadPipe(self, pipeName, timeout, waitHandles, overlappedHe):
        handle = self.connectPipe(pipeName, overlappedHe)
        command = None
        rc = win32event.WaitForMultipleObjects(waitHandles, 0, timeout)
        if rc == win32event.WAIT_OBJECT_0:
            raise Exceptions.ServiceShutdown
        else:
            status, command = self.ReadFilePlus(handle, 1)
        if command:
            src = self.getPipeSource(handle)
            message = "%s sent me %s" % (src, command)
            win32file.WriteFile(handle, message)
            #^ FIXME: verify the command is sane.
            return command
        return ''

    def sendNpMessage(self, pipe, message, logFunction, timeout=None):
        if timeout:
            timeout = time.time() + timeout
        while True:
            try:
                logFunction("||-- Sending '%s' message to %s\n" % (message, pipe))
                win32pipe.CallNamedPipe(pipe, message, 256, win32pipe.NMPWAIT_WAIT_FOREVER)
                return OK
            except pywintypes.error:
                time.sleep(1)
                if timeout:
                    if time.time() > timeout:
                        logFunction("||--- Giving up on service")
                        return FAIL
                logFunction("||--- Waiting for service to listen on %s.\n" % pipe)
                continue
        return FAIL

    def getPipe(self, pipeName):
        # When running as a service, we must use special security for the pipe
        openMode = win32pipe.PIPE_ACCESS_DUPLEX | win32file.FILE_FLAG_OVERLAPPED
        pipeMode = win32pipe.PIPE_TYPE_MESSAGE
        sa = pywintypes.SECURITY_ATTRIBUTES()
        sa.SetSecurityDescriptorDacl ( 1, None, 0 )
        # default buffers, and 6 second timeout.
        pipeHandle = win32pipe.CreateNamedPipe(pipeName, openMode, pipeMode,
                                               win32pipe.PIPE_UNLIMITED_INSTANCES,
                                               0, 0, 6000, sa)
        return pipeHandle

    ## NOT TESTED
    def connectPipe(self, pipeName, event):
        handle = self.getPipe(pipeName)
        tries = 0
        status = FAIL
        while tries < 10 and status == FAIL:
            try:
                hr = win32pipe.ConnectNamedPipe(handle, pywintypes.OVERLAPPED())
                status = OK
                if hr==winerror.ERROR_PIPE_CONNECTED:
                    # Client is fast, and already connected - signal event
                    win32event.SetEvent(event)
            except pywintypes.error:
                handle.Close()
                handle = self.getPipe(pipeName)
                tries += 1
                time.sleep(1)
        if status == FAIL:
            raise Exceptions.PipeNotListenable, pipeName
        return handle

    def getPipeSource(self, handle):
        pipeState = win32pipe.GetNamedPipeHandleState(handle)
        if len(pipeState) >= 4:
            return pipeState[4]
        return ''

    def ReadFilePlus(self, handle, bytes):
        try:
            return win32file.ReadFile(handle, bytes)
        except pywintypes.error:
            pass
        return '', ''

    ## REGISTRY FUNCTIONS

    def checkForKey(self, keyList, startKey=winreg.HKEY_LOCAL_MACHINE):
        outputList = []
        for examineKey in keyList:
            currentKey = '\\'.join(outputList)
            r = winreg.OpenKey(startKey, currentKey, 0, winreg.KEY_READ)
            index = 0
            while True:
                try:
                    newKey = winreg.EnumKey(r, index)
                    if newKey.lower() == examineKey.lower():
                        outputList.append(newKey)
                        break
                    index += 1
                except WindowsError:
                    return outputList
        return outputList

    def makeKeys(self, keyList, startKey=winreg.HKEY_LOCAL_MACHINE):
        if type(keyList) == type("string"):
            keyList = keyList.split('\\')
        if type(keyList) != type(['list']):
            print "unable to convert to list"
            return
        alreadyThere = self.checkForKey(keyList, startKey)
        for index in range(len(alreadyThere), len(keyList)):
            existingKey = '\\'.join(alreadyThere)
            newKey = keyList[index]
            base = winreg.OpenKey(startKey, existingKey, 0, winreg.KEY_SET_VALUE)
            winreg.CreateKey(base, newKey)
            alreadyThere.append(newKey)
            winreg.FlushKey(base)
            winreg.CloseKey(base)

    def queryKey(self, path):
        initialKey, path = findInitialKey(path)
        obj = path.split('\\')[-1]
        path = "\\".join(path.split("\\")[:-1]) # pull the last one off
        key = winreg.OpenKey(initialKey, path, 0, winreg.KEY_QUERY_VALUE)
        value, keytype = winreg.QueryValueEx(key, obj)
        return value, keytype

    def setKey(self, path, value):
        initialKey, path = findInitialKey(path)
        obj = path.split('\\')[-1]
        path = "\\".join(path.split("\\")[:-1]) # pull the last one off
        self.makeKeys(path)
        r = RegistryDict.RegistryDict(path, flags=win32con.KEY_ALL_ACCESS)
        r[obj] = value

    # This is the main removal function. keyName is a string
    def removeSubKeys( self, keyName ):
        try:
            keyList = self.getSubKeys( keyName )
            for k in keyList:
                self.removeKeyHKLM( k )
        except:
            pass

    def removeKeyHKLM( self, keyName ):
        Logger.info("Removing key %s" % keyName)
        try:
            winreg.DeleteKey( winreg.HKEY_LOCAL_MACHINE, keyName )
        except Exception, e:
            Logger.warning("Unable to delete key %s; (%s)" % (keyName, e))

    def getSubKeys( self, keyName ):
        keyList = []
        try:
            fullList = self.enumSubKeysRecursively( keyName, keyList )
        except Exception, e:
            Logger.warning("Unable to open key %s (%s)" % (keyName, e))
            return []
        strList = []
        self.popStrings( fullList, strList )
        strList.reverse()
        for s in strList:
            print s
        return( strList )
    
    def enumSubKeysRecursively( self, keyName, keyList ):
        key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE, keyName, 0, winreg.KEY_ALL_ACCESS )
        keysLeft = True
        i = 0
        while keysLeft:
            subList = []
            try:
                subKeyName = keyName + "\\" + winreg.EnumKey( key, i )
                subList.append( subKeyName )
                self.enumSubKeysRecursively( subKeyName, subList )
            except EnvironmentError:
                keysLeft = False
            if len( subList ) > 0:
                keyList.append( subList )
            i = i + 1
        return( keyList )

    def popStrings( self, list, strList ):
        for l in list:
            if repr( type( l ) ) == '<type \'list\'>':
                self.popStrings( l, strList )
            else:
                strList.append( l )

    # User commands

    def findUser(self, username):
        users = win32net.NetUserEnum('.', 0, win32netcon.FILTER_NORMAL_ACCOUNT)[0]
        for user in users:
            if user['name'].lower() == username.lower():
                return OK
        return FAIL

    def mkServiceUser(self, username, password, domain='', local=False, comment='', admin=True):
        if not local:
            if domain == '' or domain =='.':
                Logger.info("Making service user %s on the default domain" % username)
            else:
                Logger.info("Making service user %s on the %s domain" % ( username, domain))
        else:
            Logger.info("Making service user %s on the local system" % username)
        domainServer = self.getDc(domain, local)
        if not domainServer:
            return FAIL
        userdata = {}
        userdata['name']     = username
        userdata['password'] = password
        userdata['comment']  = comment
        userdata['flags']    = win32netcon.UF_DONT_EXPIRE_PASSWD | \
                               win32netcon.UF_PASSWD_CANT_CHANGE | \
                               win32netcon.UF_NORMAL_ACCOUNT
        userdata['priv']     = win32netcon.USER_PRIV_USER
        try:
            win32net.NetUserDel(domainServer, username)
        except pywintypes.error, e:
            pass
        try:
            win32net.NetUserAdd(domainServer, 1, userdata)
        except pywintypes.error, e:
            if e[2] != "The account already exists.":
                Logger.error("Unable to create user '%s'. [%s]" % (username, e[2]))
                return FAIL
        if admin:
            try:
                win32net.NetLocalGroupAddMembers(None, 'administrators',
                                                 3, [{"domainandname":username}])
            except pywintypes.error, e:
                if e[2] != 'The specified account name is already a member of the local group.':
                    Logger.error("Unable to assign proper permissions to user %s [%s]" % (username, e[2]))
                    return FAIL
        return OK

    def rmServiceUser(self, username, domain='', local=False):
        domainServer = self.getDc(domain, local)
        if not domainServer:
            return FAIL
        win32net.NetUserDel(domainServer, username)
        return OK

    def getDc(self, domain, local):
        if local:
            return os.environ["COMPUTERNAME"]
        try:
            domainServer = win32net.NetGetAnyDCName(None, domain)
        except pywintypes.error, e:
            if domain:
                Logger.warning( "ERROR setting up user %s" % e )
            else:
                Logger.warning( "No domain controller could be found." )
            return None
        return domainServer

    def AdjustPrivilege(self, priv, enable = 1): # Get the process token.
        flags = win32con.TOKEN_ADJUST_PRIVILEGES | win32con.TOKEN_QUERY
        fail = []
        htoken = win32security.OpenProcessToken(win32api.GetCurrentProcess(), flags)
        # Get the ID for the privilege.
        ident = win32security.LookupPrivilegeValue(None, priv)
        # Now obtain the privilege for this process.
        # Create a list of the privileges to be added.
        if enable:
            newPrivileges = [(ident, win32con.SE_PRIVILEGE_ENABLED)]
        else:
            newPrivileges = [(ident, 0)]
        # and make the adjustment.
        try:
            win32security.AdjustTokenPrivileges(htoken, 0, newPrivileges)
        except:
            fail.append(priv)

    def testCredentials(self, username, domain, password):
        if domain:
            self.domain = domain
        else:
            self.domain = '.'
        if not username:
            return
        self.username = username
        if password:
            self.password = password
        if self.logon() == OK:
            status = OK
            self.logoff()
        else:
            status = FAIL
        return status

    def logon(self):
        self.AdjustPrivilege(win32con.SE_CHANGE_NOTIFY_NAME)
        self.AdjustPrivilege(win32con.SE_TCB_NAME)
        self.AdjustPrivilege(win32con.SE_ASSIGNPRIMARYTOKEN_NAME)
        try:
            self.handle = win32security.LogonUser(self.username, self.domain,
                                                  self.password, win32con.LOGON32_LOGON_INTERACTIVE,
                                                  win32con.LOGON32_PROVIDER_DEFAULT)
            win32security.ImpersonateLoggedOnUser(self.handle)
            return OK
        except pywintypes.error, details:
            if details[0] == 1314:
                Logger.error("The account this service runs under does "\
                                  "not have the 'act as part of operating system right'")
            #else:
                #Logger.info("User '%s' cannot log in (%s)" % (self.username, details))
            return FAIL
        return OK
    
    def logoff(self):
        win32security.RevertToSelf(  ) # terminates impersonation
        self.handle.Close(  ) # guarantees cleanup

    def getRunKey(self): 
        try:
            return winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                                  RUN_KEY_NAME, 0, winreg.KEY_SET_VALUE)
        except WindowsError:
            return winreg.CreateKey(winreg.HKEY_LOCAL_MACHINE, RUN_KEY_NAME)


    def restartOnLogon(self):
        runKey = self.getRunKey()
        pythonExec = os.path.join(sys.prefix, "pythonw.exe")
        cmd = "%s %s -a" % (pythonExec, miniUtility.getBombardierPath())
        winreg.SetValueEx(runKey, "BombardierRun", 0, winreg.REG_SZ, cmd)

    def noRestartOnLogon(self): 
        try:
            runKey = self.getRunKey()
            winreg.DeleteValue(runKey, "BombardierRun")
        except WindowsError:
            pass

    def adjustPrivilege(self, priv, enable = 1):
        # Get the process token.
        flags = ntsecuritycon.TOKEN_ADJUST_PRIVILEGES | ntsecuritycon.TOKEN_QUERY
        htoken = win32security.OpenProcessToken(win32api.GetCurrentProcess(), flags)
        # Get the ID for the system shutdown privilege.
        ident = win32security.LookupPrivilegeValue(None, priv)
        # Now obtain the privilege for this process.
        # Create a list of the privileges to be added.
        if enable:
            newPrivileges = [(ident, ntsecuritycon.SE_PRIVILEGE_ENABLED)]
        else:
            newPrivileges = [(ident, 0)]
        # and make the adjustment.
        win32security.AdjustTokenPrivileges(htoken, 0, newPrivileges)

    def rebootSystem(self, message="Server Rebooting", timeout=5,
                     bForce=1, bReboot=1):
        if self.DEBUG:
            erstr = "REFUSING TO REBOOT SYSTEM DURING DEBUG MODE"
            Logger.info(erstr)
            sys.exit(0)
        self.adjustPrivilege(ntsecuritycon.SE_SHUTDOWN_NAME)
        try:
            win32api.InitiateSystemShutdown(None, message, timeout, bForce, bReboot)
        except pywintypes.error, details:
            if details[0] == 5:
                Logger.error("ACCESS DENIED in shutting down the system.")
            else:
                Logger.error("Unknown error: %s" % details)
            self.adjustPrivilege(ntsecuritycon.SE_SHUTDOWN_NAME, 0)
        self.adjustPrivilege(ntsecuritycon.SE_SHUTDOWN_NAME, 0)
        # ...and hang the thread, because bombardier is expecting this not to return.
        while 1 == 1:
            time.sleep(1)

    def abortReboot(self):
        self.adjustPrivilege(ntsecuritycon.SE_SHUTDOWN_NAME)
        try:
            win32api.AbortSystemShutdown(None)
        finally:
            # Now we remove the privilege we just added.
            self.adjustPrivilege(ntsecuritycon.SE_SHUTDOWN_NAME, 0)

    def CoInitialize(self):
        pythoncom.CoInitialize()

    # ============= CONSOLE CHECKER

    def testConsole(self):
        gt = guiThread()
        gt.start()
        try:
            aut = win32com.client.Dispatch( "AutoItX3.Control.1" )
        except pywintypes.com_error:
            Logger.error("AutoIt DLL error: attempting to register")
            comPath = os.path.join(miniUtility.getSpkgPath(), "AutoItX3.dll")
            regSvc  = os.path.join(os.environ["WINDIR"], "SYSTEM32", "regsvc")
            os.system("%s /s %s" % (regSvc, comPath))
            try:
                aut = win32com.client.Dispatch( "AutoItX3.Control.1" )
                Logger.info("Registered properly")
            except pywintypes.com_error:
                raise Exceptions.MissingComponent, "AutoIT"
        aut.winWait(TEST_TITLE, '', 60)
        tries = 3
        while tries:
            aut.WinActivate( TEST_TITLE )
            if not aut.WinActive( TEST_TITLE ) :
                tries -= 1
            else:
                break
        if tries == 0:
            Logger.info("There is no console detected (a)")
            return FAIL
        status = aut.ControlClick(TEST_TITLE, '', 'Button1')
        if status == 1:
            Logger.info("console detected")
            return OK
        else:
            Logger.info("There is no console detected (b)")
            return FAIL
    
    ## From Config
    # NOT TESTABLE
    def getWindowsType(self):
        keyName = "SYSTEM\CurrentControlSet\Control\ProductOptions"
        typeKey = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                                 keyName, 0, winreg.KEY_QUERY_VALUE)
        serverType,vartype = winreg.QueryValueEx(typeKey, "ProductType")

        keyName = "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion"
        typeKey = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                                 keyName, 0, winreg.KEY_QUERY_VALUE)
        systemVersion,vartype = winreg.QueryValueEx(typeKey, "ProductName")
        winType = ''
        winVer = ''
        if serverType == "WinNT":
            winType = PROFESSIONAL
        elif serverType == "LanmanNT" or serverType == "ServerNT":
            winType = SERVER
        else:
            winType = UNKNOWN
        if systemVersion.rfind('2000'):
            winVer = WIN2000
        elif systemVersion.rfind('2003'):
            winVer = WIN2003
        elif systemVersion.rfind('XP'):
            winVer = WINXP
        else:
            winVer = UNKNOWN
        return [winType, winVer]


    # FIXME: Move these to the Windows Class
    ### TESTED
    def checkAutoLogin(self): 
        if self.queryKey(LOGIN_KEY_NAME+"\\AutoAdminLogon") == "1":
            return True
        else:
            return False

    ### TESTED
    def noAutoLogin(self): 
        self.setKey(LOGIN_KEY_NAME+"\\AutoAdminLogon", '0')
        self.setKey(LOGIN_KEY_NAME+"\\DefaultUserName", "")
        self.setKey(LOGIN_KEY_NAME+"\\DefaultPassword", "")
        self.setKey(LOGIN_KEY_NAME+"\\DefaultDomainName", "")
        return OK

    def autoLogin(self, config):
        if self.testCredentials(config.username, config.domain, config.password) == FAIL:
            if not config.domain or config.domain=='.':
                status = self.mkServiceUser(config.username, config.password, "",
                                                    comment="Bombardier administrative user",
                                                    local=True)
            else:
                status = self.mkServiceUser(config.username, config.password, config.domain,
                                                    comment="Bombardier administrative user",
                                                    local=False)

            if status == FAIL:
                errmsg = "Unable to create service user (%s\\%s)" % \
                         (config.domain,config.username)
                Logger.error(errmsg)
                return FAIL
            if self.testCredentials(config.username, config.domain,
                                            config.password) == FAIL:
                errmsg = "Unable to set proper credentials for login. Giving up on autoLogin."
                Logger.error(errmsg)
                return FAIL
        if config.username:
            self.setKey(LOGIN_KEY_NAME+"\\DefaultUserName", config.username)
        if config.password:
            self.setKey(LOGIN_KEY_NAME+"\\DefaultPassword", config.password)
        if config.domain:
            self.setKey(LOGIN_KEY_NAME+"\\DefaultDomainName", config.domain)
        self.setKey(LOGIN_KEY_NAME+"\\AutoAdminLogon", "1")
        return OK

