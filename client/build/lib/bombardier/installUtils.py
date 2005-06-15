#!/cygdrive/c/Python23/python.exe

import os, sys, shutil, string, commands
#from commonUtil import * 

import win32com.client
from time import sleep

import win32netcon, win32net

def consoleFail( errorString="Failed, error unknown" ):
    logger.error( errorString )
    consoleSync( FAIL )
    sys.exit(FAIL)

def restartIIS():
    system( "iisreset | cd ." )

def showProgress( targetName ):

    logger.info( "# ============================================" )
    logger.info( "Creating " + targetName )
    logger.info( "# ============================================" )

def installService( _svcPath, domain=None, userName=None, password=None ):
    cmd = os.path.join(os.environ['windir'], "Microsoft.Net", 
                       "Framework", "v1.1.4322", "Installutil.exe")
    if( userName != None ) and ( password != None ) and ( domain != None ):
        cmd += " /userName=%s\\%s /password=%s" %(domain, userName, password)
    logger.info( "cmd = %s" %(cmd) )
    logger.info( "_svcPath = %s" %(_svcPath)  )
    system("%s %s" % (cmd, _svcPath), "Unable to install " + _svcPath + " as a service", debug=1)

def removeFile( path ):
    if os.path.isfile( path ):
        os.remove( path )

def copyDirectory( _src, _dest ):
    try:
        if os.path.isdir(_dest):
            shutil.rmtree(_dest)
        shutil.copytree(_src, _dest)
    except:
        errString = "error creating " + _dest + " directory" 
        if logger != None:
            consoleFail(errString)
        else:
            print errString

class SiteCreator:
    def __init__(self, homeDir, sourceDir, siteIndex, title, ipAddress, port, securePort=None):
        import win32com.client
        self.homeDir    =  homeDir
        self.sourceDir  =  sourceDir
        self.siteIndex  =  siteIndex
        self.title      =  title
        self.ipAddress   =  ipAddress
        self.port       =  port
        self.securePort =  securePort
        self.serverBindings = self.ipAddress + ":" + self.port + ":"
        if self.securePort != None:
            self.secureBindings = self.ipAddress + ":" + self.securePort + ":"
        else:
            self.secureBindings = None

    def createSite(self):
        createWebSite(homeDirectory=self.homeDir, sourceFiles=self.sourceDir, 
                      siteIndex=self.siteIndex, ipAddress=self.ipAddress,
                      port=65534, title=self.title)
        self.setSiteInfo()
    
    def setSiteInfo(self):
        try:
            logger.info( "---------------------------" )
            logger.info( "Index : " + self.siteIndex )
            logger.info( "serverBindings : %s" %( self.serverBindings ) )
            logger.info( "secureBindings : %s" %( str(self.secureBindings) ) )
    
            site = win32com.client.GetObject( "IIS://localhost/W3SVC/" + self.siteIndex )
            site.Put( 'ServerBindings', self.serverBindings )
            if self.secureBindings != None:
                site.Put( 'SecureBindings', self.secureBindings )
            site.SetInfo()
        except:
            consoleFail( "Could not set site info" )
    
class AspUserAccount:
    def __init__(self, userName, userPass, computerName, logger=None):
        import sys, os, win32net, win32netcon
        from os.path import join
        self.userName = userName
        self.userPass = userPass
        self.computerName = computerName
        self.logger = logger

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
            self.logger.warning( errString )

    def setNtRights(self):
        try:
            resourceKit = os.path.join("C:\\", "Progra~1", "Resour~1")
            ntrights = os.path.join(resourceKit, "ntrights.exe")
            system("%s /u %s +r SeNetworkLogonRight" % (ntrights, self.userName),
                   "Unable to set Network Logon rights")
            system("%s /u %s +r SeBatchLogonRight" % (ntrights, self.userName),
                   "Unable to set Batch Logon rights")
            system("%s /u %s +r SeServiceLogonRight" % (ntrights, self.userName),
                   "Unable to set Service Logon rights")
            system("%s /u %s +r SeDenyInteractiveLogonRight" % (ntrights, self.userName), 
                   "Unable to deny interactive logon rights")
        except:
            errString = "Unable to set ntrights for " + \
                        self.computerName + "\\" + self.userName  
            logger.warning( errString )

    def setFilePermissions(self):
        try:  
            from os.path import join
            # Update NTFS permissions for the aspUser user
            winDir  = os.environ['WINDIR'] 
            tempDir = join(winDir, "temp")
            cacls   = join(winDir, "system32", "cacls.exe")
            frameworkPath = join(winDir, "Microsoft.NET", "Framework", "v1.1.4322")
            system('%s "%s" /t /e /p %s:f > cacls-spew.txt 2> cacls.txt' 
                   % (cacls, join(frameworkPath, "Temporary ASP.NET Files"), self.userName),
                   "can't change permissions")
            logger.debug( '%s "%s" /t /e /p %s:rwc > cacls-spew.txt 2> cacls.txt'
                          % (cacls, tempDir, self.userName) )
            system('%s "%s" /t /e /p %s:c > cacls-spew.txt 2> cacls.txt'
                   % (cacls, tempDir, self.userName), "can't change permissions")
            system('%s "%s" /t /e /p %s:r > cacls-spew.txt 2> cacls.txt'
                   % (cacls, join(frameworkPath, "temp"), self.userName),
                   "can't change permissions")
            system('%s "%s" /t /e /p %s:r > cacls-spew.txt 2> cacls.txt'
                   % (cacls, join(os.environ["WINDIR"], "assembly"), self.userName),
                   "can't change permissions")
    
        except:
            errString = "Unable to set permissions for " + \
                        self.computerName + "\\" + self.userName  
            logger.warning( errString )
    
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
        self.aut = win32com.client.Dispatch( "AutoItX3.Control.1" )

    def verifyActivate( self, title ):
        logger.debug( "AutoItControl activating window %s" %(title) )
        self.aut.WinActivate( title )
        if not self.aut.WinActive( title ) :
            consoleFail( "Could not activate " + title )
        return True
    
    def waitVerifyActivate( self, title, text="", delay=30 ):
        logger.debug( "AutoItControl waiting for window %s" %(title) )
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
        logger.debug( str(cmdList) )
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
    
    def openCertificateWizard( self ):
    
        self.verifyActivate( self.title )
    
        self.aut.Send("!S")
        if not self.aut.WinWait("Welcome to the Web Server Certificate Wizard.", "", 30):
           consoleFail( "Could not open Certificate Wizard" )
    
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
        logger.info( "Done waiting for IIS snap in" )
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
        logger.debug( "curl --insecure %s %s/ca-cert.pem > %s" % (credentials, self.caUrl, self.caCertPath ) )
        print FAIL, OK
        status = os.system("curl --insecure %s %s/ca-cert.pem > %s" % (credentials, self.caUrl, self.caCertPath ))
        logger.debug( status )
        return status
    
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
            curlCmd += "%s --user %s:%s " % ( self.caUser, self.caPassword )
        cmd =  "%s %s/req/new " % ( curlCmd, self.caUrl )
        logger.debug( cmd )
        output = os.popen( cmd ).read()
        ticketNum = output.split()[-1]
        logger.debug( "ticketNum = %s" %(`ticketNum`) )
        cmd = "%s %s/req/%s/" % ( curlCmd, self.caUrl, ticketNum )
        logger.debug( cmd )
        status = os.system( cmd )
        if status == FAIL:
            consoleFail( "Problem submitting ticket number to certificate server" )
        cmd = "%s -T%s %s/req/%s/csr" % ( curlCmd, self.certRequestPath, self.caUrl, ticketNum )
        logger.debug( cmd )
        status = os.system( cmd )
        if status == FAIL:
            consoleFail( "Unable to PUT our certificate request onto the certificate server" )
        cmd = "%s %s/req/%s/crt > %s" % ( curlCmd, self.caUrl, ticketNum, self.signedCertPath)
        logger.debug( cmd )
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
    
        logger.info( "Creating pending cert" )
        self.wizCmds( ['C', '', 'P', '', '!m%s'%self.siteName, '',
                       '!oGE Security!uSupra -- IT', '', 
                       '!c%s'%self.commonName, '',
                       '!cUS!sOregon!lSalem', '',
                       '!f%s'%self.certRequestPath, '','',''] )
 
    def deletePendingCert(self):
        logger.info( "Deleting pending certificate request" )
        self.wizCmds( ['!d', '','',''] )


