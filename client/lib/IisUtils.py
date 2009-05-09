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
from old_static_data import *

if sys.platform == "win32":
    import pywintypes, win32netcon, win32net
    from win32com.client import GetObject, Dispatch
    import _winreg as winreg
    from time import sleep
    import win32api, win32file
    from bombardier_common.Windows import Windows

from Exceptions import ConsoleException
from bombardier_common.Filesystem import copyDirectory, removeFile
from bombardier_common.Logger import Logger

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
                
