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

import os, sys, shutil

if sys.platform == "win32":
    import pywintypes, win32netcon, win32net
    from win32com.client import GetObject, Dispatch
    import _winreg as winreg
    from time import sleep
    import win32api, win32file
    from Windows import Windows

import Logger
from Exceptions import ConsoleException
from Filesystem import copyDirectory, removeFile
from staticData import *

class IisUtilsException(ConsoleException):
    pass

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
            raise IisUtilsException( "Exception trying to get w3svc: \n%s" %e )
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
            raise IisUtilsException( "Object found at intended site index %s, failing" %self.siteIndex )
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
            raise IisUtilsException( "Could not set serverBindings" )
                
class AutoItControl:
    def __init__(self):
        self.aut = Dispatch( "AutoItX3.Control.1" )

    def verifyActivate( self, title ):
        Logger.debug( "AutoItControl activating window %s" %(title) )
        self.aut.WinActivate( title )
        if not self.aut.WinActive( title ) :
            raise IisUtilsException( "Could not activate " + title )
        return True
    
    def waitVerifyActivate( self, title, text="", delay=30 ):
        Logger.debug( "AutoItControl waiting for window %s" %(title) )
        if not self.aut.WinWait( title, text, delay ):
            raise IisUtilsException( "Wait failed on "+ title + "\nFailing" )
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
            pyChucker(r)
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

    def showProgress(self, targetName):
        Logger.info( "# ============================================" )
        Logger.info( "Creating " + targetName )
        Logger.info( "# ============================================" )

    def getCACert(self):
        credentials = ""
        if self.caUser and self.caPassword:
            credentials = " --user %s:%s " % (self.caUser, self.caPassword)
        Logger.debug( "curl --insecure %s %s/ca-cert.pem > %s" % (credentials, self.caUrl, self.caCertPath ) )
        print FAIL, OK
        status = os.system("curl --insecure %s %s/ca-cert.pem > %s" % (credentials, self.caUrl, self.caCertPath ))
        Logger.debug( status )
        return status
    
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
            raise IisUtilsException( "Could not open LocalMachineCerts.msc" )
    
    def openLocalMachinePersonalCerts(self):
        self.aut.Run(r"C:\WINNT\System32\mmc.exe ../scripts/LocalMachinePersonalCerts.msc")
        if not self.aut.WinWait("LocalMachinePersonalCerts", "", 3):
            raise IisUtilsException( "Could not open LocalMachinePersonalCerts.msc" )
    def openCertificateWizard( self ):
    
        self.verifyActivate( self.title )
    
        self.aut.Send("!S")
        if not self.aut.WinWait("Welcome to the Web Server Certificate Wizard.", "", 30):
           raise IisUtilsException( "Could not open Certificate Wizard" )
    
    def installCACert(self):
        self.showProgress( "CA Certificate" )
    
        if self.getCACert() == FAIL:
            raise IisUtilsException( "Could not get CA cert from authority" )
    
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
            raise IisUtilsException( "Problem submitting ticket number to certificate server" )
        cmd = "%s -T%s %s/req/%s/csr" % ( curlCmd, self.certRequestPath, self.caUrl, ticketNum )
        Logger.debug( cmd )
        status = os.system( cmd )
        if status == FAIL:
            raise IisUtilsException( "Unable to PUT our certificate request onto the certificate server" )
        cmd = "%s %s/req/%s/crt > %s" % ( curlCmd, self.caUrl, ticketNum, self.signedCertPath)
        Logger.debug( cmd )
        status = os.system( cmd )
        if status == FAIL:
            raise IisUtilsException( "Unable to get our signed certificate request from the certificate server" )
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
