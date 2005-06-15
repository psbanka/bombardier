#^^ FIXME: This does not work yet.
def createWebSite2(homeDirectory, sourceFiles, siteIndex, 
                  ipAddress=None, siteName=None, logger=None, port=80, comment=""):
    
        cscript = os.path.join(os.environ['WINDIR'], "SYSTEM32", "cscript.exe")
        ADMINSCRIPTS = os.path.join("C:\\", "Inetpub", "AdminScripts")
        system(r'%s %s -a w3svc/1 -v' % (cscript, os.path.join(ADMINSCRIPTS, "startsrv.vbs")),
               "unable to stop the default site")
        if os.path.isdir(homeDirectory):
            if logger:
                logger.warning("Removing All files in directory %s" % homeDirectory)
            shutil.rmtree(homeDirectory)

        shutil.copytree(sourceFiles, homeDirectory)

        bindingString = "%s:%s:%s" % (ipAddress, port, siteName)
        adsi=win32com.client.Dispatch('ADsNameSpaces')
        servicePath='IIS://localhost/w3svc'
        try:
            w3svc=adsi.getobject("",servicePath)
        except pywintypes.com_error:
            print "IIS Not installed"
            return FAIL
        for webServer in w3svc:
            if webServer.Class == "IIsWebServer":
                bindings = webServer.ServerBindings
                if bindings == bindingString:
                    logger.error("Site being created conflicts with another site")
                    return FAIL
        newSite = w3svc.Create("IIsWebServer", 9)
        newSite.SetInfo()
        newSitePath='IIS://localhost/w3svc/%s' % siteIndex
        newSiteObject = adsi.getobject("",newSitePath)
        newSite.ServerBindings = bindingString
        newSite.ServerComment = comment
        newSite.SetInfo()
        newDir = newSite.Create("IIsWebVirtualDir", "ROOT")
        newDir.Path = homeDirectory
        newDir.AccessRead = True
        newDir.SetInfo()
        newDir.AppCreate(True)
        adsobj.AccessFlags = permissions
        adsobj.SetInfo()
        del adsobj

