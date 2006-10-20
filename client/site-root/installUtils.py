#!/cygdrive/c/Python24/python.exe

# installUtils.py: This provides installation services for packages
# which are installed by Bombardier.

# Copyright (C) 2005 Shawn Sherwood

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

WRITABLE_FILE_MODE      = 33206
WRITABLE_DIRECTORY_MODE = 16895

import os, sys, shutil

if sys.platform == "win32":
    import pywintypes, win32netcon, win32net
    from win32com.client import GetObject, Dispatch
    import _winreg as winreg
    from time import sleep
    import win32api, win32file
    from bombardier.Windows import Windows

from bombardier.miniUtility import consoleSync
from bombardier.utility import removeDirectory
import bombardier.Logger as Logger
import bombardier.Filesystem as Filesystem
from bombardier.staticData import *

def consoleFail( errorString="Failed, error unknown" ):
    Logger.error( errorString )
    consoleSync( FAIL )
    sys.exit(FAIL)

def restartIIS():
    os.system( "iisreset | cd ." )

def showProgress( targetName ):

    Logger.info( "# ============================================" )
    Logger.info( "Creating " + targetName )
    Logger.info( "# ============================================" )

def installService( _svcPath, domain=None, userName=None, password=None ):
    cmd = os.path.join(os.environ['windir'], "Microsoft.Net", 
                       "Framework", "v1.1.4322", "Installutil.exe")
    if( userName != None ) and ( password != None ):
        if domain == '.' or domain == '' or domain == None:
            domain = os.environ['hostname']
        cmd += " /userName=%s\\%s /password=%s" %(domain, userName, password)
    Logger.info( "cmd = %s" %(cmd) )
    Logger.info( "_svcPath = %s" %(_svcPath)  )
    Filesystem.Filesystem().execute("%s %s" % (cmd, _svcPath), "Unable to install " + _svcPath + " as a service", debug=1)

def removeFile( path ):
    if os.path.isfile( path ):
        os.remove( path )

def registerDlls(directory, dlls):
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

def unregisterDlls(directory, dlls):
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

def copyDirectory( _src, _dest ):
    try:
        if os.path.isdir(_dest):
            removeDirectory( _dest )
        shutil.copytree(_src, _dest)
    except Exception, e:
        errString = "Error creating %s directory:\n%s" %(_dest, e) 
        if Logger != None:
            consoleFail(errString)
        else:
            print errString
            
def copyFile( _src, _dest ):
    try:
        if os.path.isfile( _dest ):
            os.unlink( _dest )
        shutil.copy( _src, _dest )
    except:
        errString = "error copying from %s to %s" %( _src,  _dest ) 
        if Logger != None:
            consoleFail(errString)
        else:
            print errString

def makeDirWritable( dir ):
    os.chmod( dir, WRITABLE_DIRECTORY_MODE )

def makeFileWritable( file ):
    os.chmod( file, WRITABLE_FILE_MODE )

def makeWritableRecursive( rootDir ):
    for root, dirs, files in os.walk(rootDir, topdown=False):
        for name in files:
            makeFileWritable(os.path.join(root, name))
        for name in dirs:
            print "%s" %(os.path.join(root, name))
            makeDirWritable(os.path.join(root, name))
        makeDirWritable( root )

def deleteDirectory( path ):
    if os.path.isdir( path ):
        makeWritableRecursive( path )
        shutil.rmtree( path )

def rmScheduledFile(filename):
    try:
        win32api.MoveFileEx(filename, None,
                            win32file.MOVEFILE_DELAY_UNTIL_REBOOT)
    except pywintypes.error, e:
        Logger.error("Cannot remove file: %s (%s)" % (filename, e))

def rmScheduledDir(path):
        for root, dirs, files in os.walk(path):
                for name in files:
                        rmScheduledFile(os.path.join(root, name))
                for name in dirs:
                        rmScheduledFile(os.path.join(root, name))

def deleteDirectories( deletePaths ):
    reboot = False
    for path in deletePaths:
        if path == "\\" or path.lower() == "c:\\":
            Logger.error("Refusing to delete %s" % path)
            return FAIL
        if os.path.isdir(path):
            Logger.info( "Attempting to remove %s" %(path) )
            try:
                deleteDirectory(path)
            except:
                Logger.info("%s not deletable...scheduling for deletion." % path)
                rmScheduledDir(path)
                reboot = True
    if reboot:
        return REBOOT
    else:
        return OK

def removeOldDsn( dataSourceName ):
    windows = Windows()
    try:
        windows.removeKeyHKLM("SOFTWARE\\ODBC\\ODBC.INI\\%s" % dataSourceName)
    except WindowsError:
        pass
    try:
        key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE, r"SOFTWARE\ODBC\ODBC.INI\ODBC Data Sources",
                             0, winreg.KEY_ALL_ACCESS )
        winreg.DeleteValue(key, dataSourceName)
    except WindowsError:
        pass

def makeAccessDsn( dataSourceName, dbFilePath ):
    try:
        hKey = winreg.CreateKey( winreg.HKEY_LOCAL_MACHINE,
                                 "SOFTWARE\\ODBC\\ODBC.INI\\%s" % dataSourceName )
        driverPath = os.path.join( os.environ['windir'], 'system32', 'odbcjt32.dll' )
        winreg.SetValueEx( hKey, "Driver",           0, winreg.REG_SZ, driverPath )
        winreg.SetValueEx( hKey, "DBQ",              0, winreg.REG_SZ, dbFilePath )
        winreg.SetValueEx( hKey, "DriverId",         0, winreg.REG_DWORD, 19 )
        winreg.SetValueEx( hKey, "FIL",              0, winreg.REG_SZ, "MS Access;" )
        winreg.SetValueEx( hKey, "SafeTransactions", 0, winreg.REG_DWORD, 0 )
        winreg.SetValueEx( hKey, "UID",              0, winreg.REG_SZ, "" )
        winreg.CloseKey(hKey)

        hKey = winreg.CreateKey( winreg.HKEY_LOCAL_MACHINE,
                                 "SOFTWARE\\ODBC\\ODBC.INI\\%s\\Engines" %dataSourceName )
        winreg.CloseKey(hKey)
        hKey = winreg.CreateKey( winreg.HKEY_LOCAL_MACHINE,
                                 "SOFTWARE\\ODBC\\ODBC.INI\\%s\\Engines\\Jet" %dataSourceName )
        winreg.SetValueEx( hKey, "ImplicitCommitSync", 0, winreg.REG_SZ, "" )
        winreg.SetValueEx( hKey, "MaxBufferSize",      0, winreg.REG_DWORD, 800 )
        winreg.SetValueEx( hKey, "PageTimeout",        0, winreg.REG_DWORD, 5 )
        winreg.SetValueEx( hKey, "Threads",            0, winreg.REG_DWORD, 3 )
        winreg.SetValueEx( hKey, "UserCommitSync",     0, winreg.REG_SZ, "Yes" )
        winreg.CloseKey(hKey)

        hKey = winreg.CreateKey( winreg.HKEY_LOCAL_MACHINE,
                   "SOFTWARE\ODBC\ODBC.INI\ODBC Data Sources" )
        winreg.SetValueEx( hKey, dataSourceName, 0, winreg.REG_SZ, "Microsoft Access Driver (*.mdb)" )
        winreg.CloseKey( hKey )
    except Exception, e:
        Logger.Error( "Unable to create Access DSN:\n%s" %e )
        sys.exit( 1 )

def makeNewDsn( dataSourceName, dbName, defaultUser, serverName, desc=None ):
    try:
        hKey = winreg.CreateKey( winreg.HKEY_LOCAL_MACHINE,
                                 "SOFTWARE\\ODBC\\ODBC.INI\\%s" %dataSourceName )

        driverPath = os.path.join( os.environ['windir'], 'system32', 'SQLSRV32.dll' )
        winreg.SetValueEx( hKey, "Database",    0, winreg.REG_SZ, dbName )
        winreg.SetValueEx( hKey, "Driver",      0, winreg.REG_SZ, driverPath   )
        winreg.SetValueEx( hKey, "LastUser",    0, winreg.REG_SZ, defaultUser  )
        winreg.SetValueEx( hKey, "Server",      0, winreg.REG_SZ, serverName       )
        if desc != None:
            winreg.SetValueEx( hKey, "Description", 0, winreg.REG_SZ, desc  )

        winreg.CloseKey(hKey)

        hKey = winreg.CreateKey( winreg.HKEY_LOCAL_MACHINE,
                                 "SOFTWARE\ODBC\ODBC.INI\ODBC Data Sources" )
        winreg.SetValueEx( hKey, dataSourceName, 0, winreg.REG_SZ, "SQL Server" )
        winreg.CloseKey( hKey )
    except Exception, e:
        Logger.Error( "Unable to create DSN:\n%s" %e )
        sys.exit( 1 )

def removeSite( _siteIndex ):
    w3svcPath = "IIS://localhost/w3svc"
    sitePath = "%s/%s" %(w3svcPath, str( _siteIndex ))
    try:
        w3svc = GetObject( w3svcPath )
    except:
        Logger.warning( "Unable to get w3svc object" )
    try:
        GetObject( sitePath )
    except:
        Logger.warning( 'The path "%s" was not found' %sitePath )
        return

    Logger.info( "Removing %s" %sitePath )
    try:
        w3svc.Delete( 'IIsWebServer', str( _siteIndex ) )
    except:
        Logger.warning( "Error removing site" )

class SiteCreator:

    POOLED = 2
    MD_AUTH_MD5 = 16
    MD_AUTH_NT  = 4
    NT_AUTH = MD_AUTH_MD5 | MD_AUTH_NT
    
    def __init__(self, homeDir, sourceDir, siteIndex, title, 
                 ipAddress, port, securePort=None, permissions=513, authFlags = None):
        try:
            self.homeDir    =  homeDir
            self.sourceDir  =  sourceDir
            self.siteIndex  =  siteIndex
            self.title      =  title
            self.ipAddress  =  ipAddress
            self.port       =  port
            self.securePort =  securePort
            self.permissions = permissions
            self.authFlags   = authFlags
            self.serverBindings = "%s:%s:" %( self.ipAddress, self.port )
            if self.securePort != None:
                self.secureBindings = "%s:%s:" %( self.ipAddress, self.securePort )
            else:
                self.secureBindings = None
        except Exception, e:        
            Logger.info( "Exception creating SiteCreator: \n\t%s" %e )

    def setScriptMap(self, scripts): # scripts is a list of dictionaries
        RESERVED = 1 # I have no idea what this is
        bigSmString = []
        for script in scripts:
            extension = script["extension"]
            command   = script["command"]
            smString = '%s,%s,%s' % (extension, command, RESERVED)
            verbs     = script.get("verbs")
            if verbs:
                smString = "%s,%s" % (smString, ','.join(verbs))
            bigSmString.append(smString)
        root = GetObject( 'IIS://localhost/w3svc/%s/ROOT'  % self.siteIndex )
        root.ScriptMaps = bigSmString
        root.SetInfo()
    
    def getIisWebDirObject(self, dir):
        root = GetObject( 'IIS://localhost/w3svc/%s/ROOT'  % self.siteIndex )
        newDir = root.Create( 'IISWebDirectory', dir )
        return newDir

    def setWebDirPermissions(self, dir, permissions):
        newDir = self.getIisWebDirObject(dir)
        newDir.AccessRead = True # FIXME: what is this, really?
        newDir.AccessFlags = permissions
        newDir.SetInfo()

    def createApplicationRoot( self, dir, authFlags=None ):
        newDir = self.getIisWebDirObject(dir)
        newDir.AppCreate(True)
        newDir.AccessFlags = 513
        if authFlags:
            newDir.AuthFlags = authFlags
        newDir.SetInfo()

    def createVirtualDir( self, name, dir, permissions=None, authFlags=None ):
        if permissions == None:
            permissions = self.permissions
        root = GetObject( 'IIS://localhost/w3svc/%s/ROOT'  %self.siteIndex )
        newDir = root.Create( 'IISWebVirtualDir', name )
        newDir.Path = dir
        newDir.AccessRead = True
        newDir.SetInfo()
        newDir.AppCreate(True)
        newDir.AccessFlags = permissions
        if authFlags:
            newDir.AuthFlags = authFlags
        newDir.SetInfo()

    def createSite(self):
        Logger.info( "Creating site:" )
        Logger.info( "\thomeDir    : %s" %self.homeDir )
        Logger.info( "\tsourceDir  : %s" %self.sourceDir )
        Logger.info( "\tsiteIndex  : %s" %self.siteIndex )
        Logger.info( "\ttitle      : %s" %self.title )
        Logger.info( "\tipAddress  : %s" %self.ipAddress )
        Logger.info( "\tport       : %s" %self.port )
        Logger.info( "\tsecurePort : %s" %self.securePort )
    
        Logger.warning("Removing All files in directory %s" % self.homeDir)
        copyDirectory( self.sourceDir, self.homeDir )

        bindingString = "%s:%s:" % (self.ipAddress, self.port)
        Logger.info("Binding string = %s" %bindingString )
        try:
            w3svc =GetObject( 'IIS://localhost/w3svc' )
        except Exception, e:
            consoleFail( "Exception trying to get w3svc: \n%s" %e )
        Logger.info( "Created w3svc object, checking for duplicate sites" )    
        for webServer in w3svc:
            if webServer.Class == "IIsWebServer":
                Logger.info( 'Found web server "%s": %s' %( webServer.serverComment, webServer.ServerBindings ) )
                bindings = webServer.ServerBindings
                if bindings == bindingString:
                    Logger.error("Site being created conflicts with another site")
                    return FAIL
        try:
            webServer = GetObject( 'IIS://localhost/w3svc/%s' %self.siteIndex )
            siteFoundAtIndex = True
        except:
            siteFoundAtIndex = False

        if siteFoundAtIndex:     
            consoleFail( "Object found at intended site index %s, failing" %self.siteIndex )
        else:    
            Logger.info( "No duplicate sites found, creating site" )                    
            newSite = w3svc.Create("IIsWebServer", self.siteIndex)
            newSite.SetInfo()
            newSitePath='IIS://localhost/w3svc/%s' % self.siteIndex
            newSiteObject = GetObject(newSitePath)
            newSite.ServerBindings = bindingString
            newSite.ServerComment = self.title
            newSite.SetInfo()
            Logger.info( "Site created, creating root directory" )
            if self.siteIndex != 1:
                newRoot = newSite.Create("IIsWebVirtualDir", "ROOT")
                newRoot.Path = self.homeDir
                newRoot.AccessFlags = self.permissions
                newRoot.SetInfo()
                newSite.AccessFlags = self.permissions
                newSite.SetInfo()
                if self.authFlags:
                    newRoot.AuthFlags = self.authFlags
                    newRoot.SetInfo()
                Logger.info( "Root created, setting permissions" )
                newRoot.AppCreate2(SiteCreator.POOLED)
                self.setServerBindings()

            del newSite
            newSiteObject.Start()
        return OK

    def setServerBindings(self):
        try:
            Logger.info( "---------------------------" )
            Logger.info( "Index : " + self.siteIndex )
            Logger.info( "serverBindings : %s" %( self.serverBindings ) )
            Logger.info( "secureBindings : %s" %( str(self.secureBindings) ) )
    
            site = GetObject( "IIS://localhost/W3SVC/" + self.siteIndex )
            site.Put( 'ServerBindings', self.serverBindings )
            if self.secureBindings != None:
                site.Put( 'SecureBindings', self.secureBindings )
            site.SetInfo()
        except:
            consoleFail( "Could not set serverBindings" )
                
    
class AspUserAccount:
    def __init__(self, userName, userPass, computerName):
        self.userName = userName
        self.userPass = userPass
        self.computerName = computerName
        self.fs = Filesystem.Filesystem()

    def hobble( self ):
        self.removeFromAdminGroup()
        self.setNtRights()
        self.setFilePermissions()    

    def removeFromAdminGroup(self):
        try:
            win32net.NetLocalGroupDelMembers( None, 'administrators', 
                                              [self.computerName + "\\" + self.userName] )
        except:
            errString = "Unable to remove " + self.computerName + \
                        "\\" + self.userName + " from the administrators group" 
            Logger.warning( errString )

    def setNtRights(self):
        try:
            resourceKit = os.path.join("C:\\", "Progra~1", "Resour~1")
            ntrights = os.path.join(resourceKit, "ntrights.exe")
            self.fs.execute("%s /u %s +r SeNetworkLogonRight" % (ntrights, self.userName),
                   "Unable to set Network Logon rights")
            self.fs.execute("%s /u %s +r SeBatchLogonRight" % (ntrights, self.userName),
                   "Unable to set Batch Logon rights")
            self.fs.execute("%s /u %s +r SeServiceLogonRight" % (ntrights, self.userName),
                   "Unable to set Service Logon rights")

            self.fs.execute("%s /u %s +r SeDenyInteractiveLogonRight" % (ntrights, self.userName), 
                   "Unable to deny interactive logon rights")
        except Exception, e:
            errString = "Unable to set ntrights for %s\\%s\nException: %s" %( self.computerName, self.userName, e )  
            Logger.warning( errString )

    def setFilePermissions(self):
        try:  
            # Update NTFS permissions for the aspUser user
            winDir  = os.environ['WINDIR'] 
            tempDir = os.path.join(winDir, "temp")
            cacls   = os.path.join(winDir, "system32", "cacls.exe")
            frameworkPath = os.path.join(winDir, "Microsoft.NET", "Framework", "v1.1.4322")
            self.fs.execute('%s "%s" /t /e /p %s:f > cacls-spew.txt 2> cacls.txt' 
                   % (cacls, os.path.join(frameworkPath, "Temporary ASP.NET Files"), self.userName),
                   "can't change permissions")
            Logger.debug( '%s "%s" /t /e /p %s:rwc > cacls-spew.txt 2> cacls.txt'
                          % (cacls, tempDir, self.userName) )
            self.fs.execute('%s "%s" /t /e /p %s:c > cacls-spew.txt 2> cacls.txt'
                   % (cacls, tempDir, self.userName), "can't change permissions")
            self.fs.execute('%s "%s" /t /e /p %s:r > cacls-spew.txt 2> cacls.txt'
                   % (cacls, os.path.join(frameworkPath, "temp"), self.userName),
                   "can't change permissions")
            self.fs.execute('%s "%s" /t /e /p %s:r > cacls-spew.txt 2> cacls.txt'
                   % (cacls, os.path.join(os.environ["WINDIR"], "assembly"), self.userName),
                   "can't change permissions")
    
        except:
            errString = "Unable to set permissions for " + \
                        self.computerName + "\\" + self.userName  
            Logger.warning( errString )
    
    def create(self):
        
        d = {}
        d['name']      = self.userName
        d['password']  = self.userPass
        d['comment']   = "User for test page"
        d['flags']     = win32netcon.UF_DONT_EXPIRE_PASSWD | \
                         win32netcon.UF_PASSWD_CANT_CHANGE | \
                         win32netcon.UF_NORMAL_ACCOUNT
        d['priv']      = win32netcon.USER_PRIV_USER
    
        try:
            win32net.NetUserAdd( None, 1, d )
        except pywintypes.error, e:
            if e[2] != "The account already exists.":
                consoleFail( "Unable to create user '%s'. [%s]" % (self.userName, e[2]) )
    
        try:
            win32net.NetLocalGroupAddMembers( None, 'administrators', 
                                              3, [{"domainandname":self.userName}])
    
        except pywintypes.error, e:
            if e[2] != "The specified account name is already a member of the local group.":
                consoleFail( "Unable to assign proper permissions to user '%s'. [%s]" % (self.userName, e[2]) )
    

class AutoItControl:
    def __init__(self):
        self.aut = Dispatch( "AutoItX3.Control.1" )

    def verifyActivate( self, title ):
        Logger.debug( "AutoItControl activating window %s" %(title) )
        self.aut.WinActivate( title )
        if not self.aut.WinActive( title ) :
            consoleFail( "Could not activate " + title )
        return True
    
    def waitVerifyActivate( self, title, text="", delay=30 ):
        Logger.debug( "AutoItControl waiting for window %s" %(title) )
        if not self.aut.WinWait( title, text, delay ):
            consoleFail( "Wait failed on "+ title + "\nFailing" )
        self.verifyActivate( title )
    
class AutoWizard(AutoItControl):
    def __init__(self):
        AutoItControl.__init__(self)

    def wizWait(self, delay=3):
        sleep( delay )
    
    def wizNext( self, repeat=1 ):
        if repeat < 1: 
            return
        for r in range( repeat ):
            self.aut.Send( "{ENTER}" )
            self.wizWait()
    
    def wizCmds(self, cmdList):
        Logger.debug( str(cmdList) )
        for string in cmdList:
            if string == '':
                self.wizNext()
                print "WizNext"
            else:
                self.aut.Send( string )
                print string

    def closePropertyPage(self):
        self.wizNext()

    def openLocalAccountPersonalCerts(self):
    
        self.aut.Run(r"C:\WINNT\System32\mmc.exe ../scripts/LocalAccountPersonalCerts.msc")
        self.waitVerifyActivate( "LocalAccountPersonalCerts" )
    
    def openLocalAccountPersonalCertsOpen(self):
    
        self.aut.Run(r"C:\WINNT\System32\mmc.exe ../scripts/LocalAccountPersonalCertsOpen.msc")
        self.waitVerifyActivate( "LocalAccountPersonalCertsOpen" )
    
    def openDirectorySecurity(self):
        self.aut.Send( "+{TAB}{UP}{RIGHT}" )
        self.wizWait()
    
    def openWebsiteProperties(self):
        self.wizWait()
        self.verifyActivate( "Internet Information Services" )
        self.aut.Send("!ar")
    
    def openIISSnapInToWebsiteSelection(self):
        self.aut.Run(r"C:\WINNT\System32\mmc.exe C:\WINNT\System32\Inetsrv\iis.msc")
        self.waitVerifyActivate( "Internet Information Services", "Internet Information Services") 
        Logger.info( "Done waiting for IIS snap in" )
        self.wizWait()
        self.aut.Send("{DOWN}")
        self.wizWait()
    
    def closeIISSnapIn(self):
        title = "Internet Information Services"
        self.aut.WinActivate( title )
        if self.aut.WinActive( title ) :
            self.aut.Send( "!{F4}" )
    
    def openCertSnapIn(self):
        self.aut.Run(r"C:\WINNT\System32\mmc.exe ../scripts/LocalMachineCerts.msc")
        if not self.aut.WinWait("LocalMachineCerts", "", 3):
            consoleFail( "Could not open LocalMachineCerts.msc" )
    
    def openLocalMachinePersonalCerts(self):
        self.aut.Run(r"C:\WINNT\System32\mmc.exe ../scripts/LocalMachinePersonalCerts.msc")
        if not self.aut.WinWait("LocalMachinePersonalCerts", "", 3):
            consoleFail( "Could not open LocalMachinePersonalCerts.msc" )

class CertInstaller(AutoWizard):
    def __init__(self, parameterDictionary ):
        AutoWizard.__init__(self)
        self.title = parameterDictionary['title']
        self.commonName = parameterDictionary['commonName']
        self.siteName = parameterDictionary['siteName']
        self.caCertPath = parameterDictionary['caCertPath']
        self.caUrl = parameterDictionary['caUrl']
        self.caUser = parameterDictionary['caUser']
        self.caPassword = parameterDictionary['caPassword']
        self.pfxPath = parameterDictionary['pfxPath']
        self.signedCertPath = self.getSignedCertPath( self.commonName )
        self.certRequestPath = self.getCertRequestPath( self.commonName )

    def getCACert(self):
        credentials = ""
        if self.caUser and self.caPassword:
            credentials = " --user %s:%s " % (self.caUser, self.caPassword)
        Logger.debug( "curl --insecure %s %s/ca-cert.pem > %s" % (credentials, self.caUrl, self.caCertPath ) )
        print FAIL, OK
        status = os.system("curl --insecure %s %s/ca-cert.pem > %s" % (credentials, self.caUrl, self.caCertPath ))
        Logger.debug( status )
        return status
    
    def openCertificateWizard( self ):
    
        self.verifyActivate( self.title )
    
        self.aut.Send("!S")
        if not self.aut.WinWait("Welcome to the Web Server Certificate Wizard.", "", 30):
           consoleFail( "Could not open Certificate Wizard" )
    
    def installCACert(self):
        showProgress( "CA Certificate" )
    
        if self.getCACert() == FAIL:
            consoleFail( "Could not get CA cert from authority" )
    
        self.openCertSnapIn()
        self.verifyActivate( "LocalMachineCerts" )
        self.wizCmds( ["!ak{DOWN}{ENTER}"] )
        self.waitVerifyActivate( "Certificate Import Wizard" )
        self.wizCmds( ['', '!f%s'%self.caCertPath, '', '', '', '', '!{F4}'] )
    
    def exportLocalMachinePersonalCertAsPfx( self ):
        removeFile( self.pfxPath )
    
        self.openLocalMachinePersonalCerts()
        self.wizCmds( ["{TAB}!ake"] )
        
        self.waitVerifyActivate( "Certificate Export Wizard" )
        self.wizCmds( ['','','','', '!f%s'%self.pfxPath, '','','', '!{F4}'] )

    def installCert( self ):
        self.openWebsiteProperties()
        self.waitVerifyActivate( self.title )
        self.openDirectorySecurity()
        self.createPendingCert()
        self.signCertRequest()
        self.processPendingCertRequest()
        self.closePropertyPage()

    def processPendingCertRequest( self ):
        self.openCertificateWizard()
        self.wizCmds( ['', '!p', '', '!p%s'%self.signedCertPath, '','',''] )
    
    def signCertRequest(self):

        removeFile( self.signedCertPath )
        curlCmd = "curl --insecure "
        if self.caUser and self.caPassword:
            curlCmd += " --user %s:%s " % ( self.caUser, self.caPassword )
        cmd =  "%s %s/req/new " % ( curlCmd, self.caUrl )
        Logger.debug( cmd )
        output = os.popen( cmd ).read()
        ticketNum = output.split()[-1]
        Logger.debug( "ticketNum = %s" %(`ticketNum`) )
        cmd = "%s %s/req/%s/" % ( curlCmd, self.caUrl, ticketNum )
        Logger.debug( cmd )
        status = os.system( cmd )
        if status == FAIL:
            consoleFail( "Problem submitting ticket number to certificate server" )
        cmd = "%s -T%s %s/req/%s/csr" % ( curlCmd, self.certRequestPath, self.caUrl, ticketNum )
        Logger.debug( cmd )
        status = os.system( cmd )
        if status == FAIL:
            consoleFail( "Unable to PUT our certificate request onto the certificate server" )
        cmd = "%s %s/req/%s/crt > %s" % ( curlCmd, self.caUrl, ticketNum, self.signedCertPath)
        Logger.debug( cmd )
        status = os.system( cmd )
        if status == FAIL:
            consoleFail( "Unable to get our signed certificate request from the certificate server" )
        return OK
        
    def getCertRequestPath( self, commonName ):
        return( "C:\\" + commonName + "certrequest.req" )
    
    def getSignedCertPath( self, commonName ):
        return( "C:\\" + commonName + "signedcert.crt" )
    
    
    def createPendingCert( self ):
        removeFile( self.certRequestPath )

        self.openCertificateWizard()
        self.verifyActivate("Welcome to the Web Server Certificate Wizard." )
        self.wizNext()
        self.waitVerifyActivate( "IIS Certificate Wizard" )
    
        windowText = self.aut.ControlGetText( "IIS Certificate Wizard", "", "Static1" )
        if windowText == "Pending Certificate Request":
            self.deletePendingCert()
            self.createPendingCert()
            return
    
        Logger.info( "Creating pending cert" )
        self.wizCmds( ['C', '', 'P', '', '!m%s'%self.siteName, '',
                       '!oGE Security!uSupra -- IT', '', 
                       '!c%s'%self.commonName, '',
                       '!cUS!sOregon!lSalem', '',
                       '!f%s'%self.certRequestPath, '','',''] )
 
    def deletePendingCert(self):
        Logger.info( "Deleting pending certificate request" )
        self.wizCmds( ['!d', '','',''] )
