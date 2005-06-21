#!/cygdrive/c/Python23/python

import os, sys, re, ConfigParser, shutil, string, md5

import _winreg as winreg
import win32com.client, win32api, win32file, pywintypes, win32pdh, win32con

import bombardier.miniUtility as miniUtility
import bombardier.RegistryDict as RegistryDict
from bombardier.staticData import *

def installFont(fontPath):
    baseName = fontPath.split('\\')[-1]
    fontDest = os.path.join(os.environ['WINDIR'], 'fonts', baseName)
    if not os.path.isfile(fontDest):
        shutil.copy(os.path.join("Deployment_Files", baseName), fontDest)
    keyName = "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Fonts"
    fontKey = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                             keyName, 0, winreg.KEY_SET_VALUE)
    winreg.SetValueEx(fontKey, "fontName", 0, winreg.REG_SZ, baseName)

def rmScheduledFile(filename):
    win32api.MoveFileEx(filename, None, win32file.MOVEFILE_DELAY_UNTIL_REBOOT)

def rmScheduledDir(path):
        for root, dirs, files in os.walk(path):
                for name in files:
                        rmScheduledFile(os.path.join(root, name))
                for name in dirs:
                        rmScheduledFile(os.path.join(root, name))

def removeDirectories( deletePaths ):
    reboot = False
    for path in deletePaths:
        if os.path.isdir(path):
            print "Attempting to remove %s" %(path)
            try:
                shutil.rmtree(path)
            except:
                rmScheduledDir(path)
                reboot = True
    if reboot:
        return REBOOT
    else:
        return OK


# ================================================== Simple utilities
def replaceInFile(regex, newData, filename, add = 1):
    """will open a file and parse through the whole thing, 
    looking for matches to the provided regex. If it finds 
    a match, it will replace the line with newData."""

    c = re.compile(regex)
    l = []
    newFile = []
    try:
        f = open(filename, 'r')
        l = f.readlines()
        f.close()
    except:
        return FAIL
    found = 0
    for line in l:
        m = c.match(line)
        if m:
            if newData != '':
                newFile.append(newData+'\n')
            found = 1
        else:
            newFile.append(line)
    if not found:
        if add:
            newFile.append(newData+'\n')
        else:
            return FAIL
    try:
        f = open(filename, 'w')
        for line in newFile:
            f.write(line)
        f.close()
    except:
        return FAIL
    return OK

# Note, this method was written by John Nielsen.
def procids():
    #each instance is a process, you can have multiple processes w/same name
    junk, instances = win32pdh.EnumObjectItems(None,None,'process', win32pdh.PERF_DETAIL_WIZARD)
    proc_ids=[]
    proc_dict={}
    for instance in instances:
        if instance in proc_dict:
            proc_dict[instance] = proc_dict[instance] + 1
        else:
            proc_dict[instance]=0
    for instance, max_instances in proc_dict.items():
        for inum in xrange(max_instances+1):
            hq = win32pdh.OpenQuery() # initializes the query handle
            win32pdh.CollectQueryData(hq) #collects data for the counter
            #type, val = win32pdh.GetFormattedCounterValue(counter_handle, win32pdh.PDH_FMT_LONG)
            #proc_ids.append((instance,str(val)))
            proc_ids.append((instance,inum))
            win32pdh.CloseQuery(hq)
    proc_ids.sort()
    return proc_ids

# ================================================== FILE OPERATIONS

def removeRoBit(directory):
    os.system(ATTRIB+' /S '+directory)

def makeWritable(path):
    chmod = r"C:\cygwin\bin\chmod.exe"
    status = os.system(chmod+" 666 "+path)
    return status

def xcopy(source, dest):
    cmd = r"c:\winnt\system32\xcopy.exe "+source+' '+dest+" /evqhry"
    os.system(cmd)

# ================================================== TOOLS FOR INSTALLING

def changeSqlListener(instance, port):
    keyName = ''
    if instance == '' or instance == 'MSSQLSERVER':
        keyName = "SOFTWARE\\Microsoft\\MSSQLServer"\
                  "\\MSSQLServer\\SuperSocketNetLib\\Tcp"
    else:
        keyName = "SOFTWARE\\Microsoft\\Microsoft SQL Server"\
                  "\\%s\\MSSQLServer\\SuperSocketNetLib\\Tcp" % instance
    portKey = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                             keyName, 0, winreg.KEY_SET_VALUE)
    if type(port) == type(10):
        port = `port`
    winreg.SetValueEx(portKey, "TcpPort", 0, winreg.REG_SZ, port)

class NTUser:
# Uses ADSI to change password under user privileges    
    def __init__(self, domain, userId):
        self.adsiNS = win32com.client.Dispatch('ADsNameSpaces')
        userPath = "WinNT://"+domain+"/" + userId + ",user"
        try:
            self.adsNTUser = self.adsiNS.GetObject("", userPath)
        except pywintypes.com_error:
            self.status = FAIL
            return
        self.status = OK
    def reset(self, OldPasswd, NewPasswd):
        try:
            self.adsNTUser.ChangePassword(OldPasswd, NewPasswd)
        except pywintypes.com_error:
            return FAIL
        return OK

class NTGroup:
    def __init__(self, domain, groupId, computer=''):
        self.adsiNS = win32com.client.Dispatch('ADsNameSpaces')
        groupPath = ''
        if computer:
            groupPath = "WinNT://%s/%s/%s,group" % (domain, computer, groupId)
        else:
            groupPath = "WinNT://%s/%s,group" % (domain,groupId)
        try:
            self.adsNTGroup = self.adsiNS.GetObject("", groupPath)
        except pywintypes.com_error:
            self.status = FAIL
            return
        self.status = OK

    def setMembership(self, user):
        if self.status != OK or user.status != OK:
            return FAIL
        try:
            self.adsNTGroup.Add(user.adsNTUser.ADsPath)
            self.adsNTGroup.Setinfo()
        except pywintypes.com_error:
            return FAIL
        return OK

# ################

def createHostEntry(siteName, ipAddress):
    # FIXME: support multiple names for IP address
    HOSTSFILE = os.path.join(os.environ['WINDIR'], "SYSTEM32", "DRIVERS", "ETC", "HOSTS")
    hostEntry = "%s\t%s" % (ipAddress, siteName)
    status = replaceInFile("(%s).*" % ipAddress, hostEntry, HOSTSFILE)
    return status

def createWebSite(homeDirectory, sourceFiles, siteIndex, title,
                  ipAddress=None, siteName=None, logger=None, port=80):
        cscript = os.path.join(os.environ['WINDIR'], "SYSTEM32", "cscript.exe")
        ADMINSCRIPTS = os.path.join("C:\\", "Inetpub", "AdminScripts")
        os.system(r'%s %s -a w3svc/1 -v' % (cscript, os.path.join(ADMINSCRIPTS, "startsrv.vbs")))
        if os.path.isdir(homeDirectory):
            if logger:
                logger.warning("Removing All files in directory %s" % homeDirectory)
            shutil.rmtree(homeDirectory)

        if sourceFiles:
            shutil.copytree(sourceFiles, homeDirectory)

        cmd = '%s %s -r %s -o %s -t "%s" -n %s -v' % (cscript, os.path.join(ADMINSCRIPTS, "mkw3site.vbs"),
                                                      homeDirectory, port, title, siteIndex)
        if siteName:
            cmd += ' -h %s' % (siteName)
        if ipAddress:
            cmd += ' -i %s' % (ipAddress)

        print cmd

        os.system(cmd)
        modifyWebPermissions(siteIndex, 513)
        cmd = '%s %s -a w3svc/%s -v' % (cscript, os.path.join(ADMINSCRIPTS, "startsrv.vbs"), siteIndex)
        os.system(cmd)

def modifyTemplate(inputFile, outputFile, configSection, config, encoding=None, processEscape = False):
        varMatch = re.compile("\%\((.*?)\)s")
        if encoding == None:
            configData = open(inputFile, 'r').read()
        else:
            configData = unicode( open(inputFile, 'rb').read(), encoding )

        variables = varMatch.findall(configData)
        output = []
        for line in configData.split('\n'):
            variables = varMatch.findall(line)
            configDict = {}
            for variable in variables:
                    configValue = config.get(configSection, variable)
                    if processEscape:
                        configDict[variable] = doubleEscape(configValue)
                    else:
                        configDict[variable] = configValue
            if configDict == {}:
                output.append(line)
            else:
                output.append(line % configDict)
        outputData = string.join(output, '\n')

        if encoding == None:
            open(outputFile, 'w').write(outputData)
        else:
            open(outputFile, 'wb').write(outputData.encode( encoding ))
        print "\nCreated: " + outputFile + "\nTemplate: " + inputFile + "\n"

# ################

def verifyActivate( aut, title ):
    aut.WinActivate( title )
    if not aut.WinActive( title ) :
        print "Could not activate " + title
        return FAIL
    else:
        return OK

def removeAccessDsn(dsnName):
    try:
        winreg.DeleteKey(winreg.HKEY_LOCAL_MACHINE, "SOFTWARE\\ODBC\\ODBC.INI\\%s\\Engines\\Jet" % dsnName)
        winreg.DeleteKey(winreg.HKEY_LOCAL_MACHINE, "SOFTWARE\\ODBC\\ODBC.INI\\%s\\Engines" % dsnName)
        winreg.DeleteKey(winreg.HKEY_LOCAL_MACHINE, "SOFTWARE\\ODBC\\ODBC.INI\\%s" % dsnName)
    except:
        print "key %s does not exist" % dsnName

def removeSqlDsn(dsnName):
    try:
        winreg.DeleteKey(winreg.HKEY_LOCAL_MACHINE, "SOFTWARE\\ODBC\\ODBC.INI\\%s" % dsnName)
    except:
        print "key %s does not exist" % dsnName
    key = RegistryDict.RegistryDict("SOFTWARE\\ODBC\\ODBC.INI\\ODBC Data Sources",
                                    flags=win32con.KEY_ALL_ACCESS)
    if dsnName in key.keys():
        del key[dsnName]
    return OK

def waitVerifyActivate( aut, title, text="", delay=30 ):
    if aut.WinActivate( title ): # screen popped up before we were ready
        print "Window already active"
        return verifyActivate(aut, title)
    if not aut.WinWait( title, text, delay ):
        print "Wait failed on "+ title + "\nFailing"
        return FAIL
    return( verifyActivate( aut, title ) )

def createSqlDsn(server, instance, port, username, password, dbName, dsnName):
    if instance:
        serverInst = server + '\\' + instance
    else:
        serverInst = server
    aut = win32com.client.Dispatch( "AutoItX3.Control.1" )
    aut.Run("c:\\WINNT\\system32\\odbcad32.exe")
    print "Screen 1"
    if waitVerifyActivate(aut, "ODBC Data Source Administrator", "", 60) == FAIL:
        return FAIL
    print "clicking on on the SysTabControl123"
    aut.AutoItSetOption("MouseCoordMode", 0)
    aut.MouseClick("left", 100, 40) # ugh, I hate this.
    #aut.ControlClick("ODBC Data Source Administrator", "", "SysTabControl321") # Pick System DSNs
    print "clicked on the sysTabControl"
    aut.MouseClick("left", 400, 90) # ugh, I hate this too.
    #aut.ControlClick("ODBC Data Source Administrator", "", "Button9") # 
    print "Screen 2"
    if waitVerifyActivate(aut, "Create New Data Source", "", 20) == FAIL:
        return FAIL
    aut.Send("s") # Go to SQL server DSNs
    aut.ControlClick("Create New Data Source", "", "Button4") # "Finish"

    print "Screen 3"
    if waitVerifyActivate(aut, "Create a New Data Source to SQL Server",
                          "This wizard will help you create an ODBC", 20) == FAIL:
        return FAIL
    aut.ControlSend("Create a New Data Source to SQL Server", "", "Edit1", dsnName)
    aut.ControlSend("Create a New Data Source to SQL Server", "", "Edit3", serverInst)

    print "Screen 4"
    if waitVerifyActivate(aut, "Create a New Data Source to SQL Server",
                          "This wizard will help you create an ODBC", 20) == FAIL:
        return FAIL
    aut.Send("!n") # Next

    print "Screen 5"
    if waitVerifyActivate(aut, "Create a New Data Source to SQL Server",
                          "How should SQL Server verify the authenticity", 20) == FAIL:
        if waitVerifyActivate(aut, "Microsoft SQL Server DSN Configuration", '', 5) != FAIL:
            print "The DSN exists...overwriting..."
            aut.Send("{ENTER}")
        else:
            return FAIL
    aut.ControlClick("Create a New Data Source to SQL Server", "", "Button2") # SQL Authentication
    aut.ControlSend("Create a New Data Source to SQL Server", "", "Edit1", "+{END}{DEL}")
    aut.ControlSend("Create a New Data Source to SQL Server", "", "Edit1", username)
    aut.ControlSend("Create a New Data Source to SQL Server", "", "Edit2", password)
    aut.Send("!t") # Next

    print "Screen 6"
    windowName = "Edit Network Library Configuration"
    if waitVerifyActivate(aut, windowName, "", 20) == FAIL:
        windowName = "Add Network Library Configuration"
        if waitVerifyActivate(aut, windowName, "", 20) == FAIL:
            return FAIL
    aut.Send("!d-") # UNCheck Dynamic ports 
    aut.ControlSend(windowName, "", "Edit4", "{END}+{HOME}{DEL}"+port)
    aut.Send("{ENTER}") # Next

    print "Screen 7"
    # <<< Back to screen 5
    if verifyActivate( aut, "Create a New Data Source to SQL Server" ) == FAIL:
        return FAIL
    aut.Send("!n") # Next

    print "Screen 8"
    if waitVerifyActivate(aut, "Create a New Data Source to SQL Server",
                          "Change the &default database to", 20) == FAIL:
        return FAIL
    aut.Send("!d+") # Use a specific database, check.
    aut.ControlSend("Create a New Data Source to SQL Server", "", "Edit1", "+{END}{DEL}")
    aut.ControlSend("Create a New Data Source to SQL Server", "", "Edit1", dbName)
    aut.Send("!n")
    aut.Send("{ENTER}")

    print "Screen 9"
    if waitVerifyActivate(aut, "ODBC Microsoft SQL Server Setup",
                          "A new ODBC data source", 20) == FAIL:
        if waitVerifyActivate("Microsoft SQL Server Login", 10):
            print "INVALID LOGIN CREDENTIALS: %s" % \
                  `[server, instance, port, username, password, dbName, dsnName]`
            if waitVerifyActivate(aut, "Create a New Data Source to SQL Server", "", 20) != FAIL:
                aut.Send("!{F4}")
        return FAIL
    aut.Send("{TAB}{ENTER}")

    print "Screen 10"
    # <<< Back to Screen 1
    if verifyActivate( aut, "ODBC Data Source Administrator" ) == FAIL:
        return FAIL
    aut.Send("{ENTER}")
    return OK

def createAccessDsn(dsnName, path):
    aut = win32com.client.Dispatch( "AutoItX3.Control.1" )
    aut.Run("c:\\WINNT\\system32\\odbcad32.exe")
    # === Screen 1
    print "1"
    if waitVerifyActivate(aut, "ODBC Data Source Administrator", "", 60) == FAIL:
        return FAIL
    aut.Send("^{TAB}")
    aut.Send("!D")
    aut.Send("m")
    aut.Send("{ENTER}")
    if waitVerifyActivate(aut, "ODBC Microsoft Access Setup", "", 30) == FAIL:
        return FAIL
    aut.Send("%s{TAB}" % dsnName)
    aut.Send("!S")
    if waitVerifyActivate(aut, "Select Database","", 30) == FAIL:
        return FAIL
    aut.Send("%s{TAB}" % path)
    aut.Send("{ENTER}")
    if waitVerifyActivate(aut, "ODBC Data Source Administrator", "", 30) == FAIL:
        return FAIL
    aut.Send("{ENTER}")
    if waitVerifyActivate(aut, "ODBC Data Source Administrator", '', 30) == FAIL:
        return FAIL
    aut.Send("{ENTER}")
    if waitVerifyActivate(aut, "ODBC Microsoft Access Setup", '', 30) == FAIL:
        return FAIL
    aut.Send("{ENTER}")
    if waitVerifyActivate(aut, "ODBC Data Source Administrator", '', 30) == FAIL:
        return FAIL
    aut.Send("{ENTER}")
    del aut
    return OK

def checkHotfix(issue=''):
    if issue == '':
        issue = getIssueNumber()
    cmd = "%s /l:." % os.path.join(os.environ['windir'], "system32", QFECHECK)
    os.system(cmd)
    files = os.listdir('.')
    fullyInstalled = True
    if os.path.isfile("newHotFix.txt"):
        fullyInstalled = False
    for fileName in files:
        if fileName.endswith(".log"):
            output = open(fileName, 'r').readlines()
            for line in output:
                if line.startswith(issue):
                    if len(line.split(':')) == 2:
                        if line.split(':')[1].strip() == "Current on system.":
                            print "HotFix %s verified." % issue
                            sys.exit(0)
                        if not fullyInstalled:
                            if line.split(':')[1].strip() == "This hotfix should be reinstalled.":
                                os.unlink("newHotFix.txt")
                                print "Hotfix %s partially verified" % issue
                                sys.exit(0)
    print "ERROR: HotFix %s FAILED verification." % issue
    sys.exit(1)

def findHotfixFile(issue):
    issue = getIssueNumber()
    fileNames = os.listdir('.')
    for fileName in fileNames:
        if fileName.rfind(issue) != -1:
            return fileName
    return ''

def determineReboot(arguments):
    if not arguments:
        print "Hotfix package run without arguments"
        return True
    pkgList = arguments[-1]
    otherPackages = pkgList.split(',')
    if len(otherPackages) < 2:
        print "Hotfix package last in package series (%s)" % arguments
        return True
    # don't reboot if the next package is a Hotfix.
    if otherPackages[1].upper().rfind("HOTFIX") != -1:
        print "Hotfix package follows this one: %s" % otherPackages[1]
        return False
    print "Non-Hotfix package follows this one: %s" % otherPackages[1]
    return True
    
def installHotfix(arguments=[]):
    issue = getIssueNumber()
    reboot = determineReboot(arguments)
    fileName = findHotfixFile(issue)
    if fileName:
        cmd = "start /wait %s -q -z" % fileName
        os.system(cmd)
        try:
            open("newHotFix.txt", 'w').write('x')
        except:
            print "Cannot flag this as a new hotfix"
        if not reboot:
            sys.exit(0)
        else:
            sys.exit(2)
    else:
        erstr = "Unable to find an appropriate HotFix file "\
                "for issue %s" % issue
        print erstr
    sys.exit(1)

def uninstallHotfix():
    issue = getIssueNumber()
    fileName = findHotfixFile(issue)
    if fileName:
        cmd = "start /wait %s -y -u -q -z" % fileName
        os.system(cmd)
        sys.exit(0)
    else:
        erstr = "Unable to find an appropriate HotFix "\
                "file for issue %s" % issue
        print erstr
    sys.exit(1)

def getIssueNumber():
    cwd = os.getcwd().split("\\")
    if cwd[-1] != "injector":
        print "ERROR: HOTFIX installer: incorrect directory. Aborting."
        sys.exit(1)
    kbIssue = cwd[-2]
    kbData = kbIssue.split('-')
    if kbData[0].upper() != "HOTFIX":
        erstr = "ERROR: package should be named "\
                "'Hotfix-[KBNUMBER]-[version]'"
        print erstr
        sys.exit(1)
    kbNumber = kbData[1]
    issue = "KB"+kbNumber
    return issue

def uninstallMsi(msiFile):
    logFile = 'c:\\spkg\\log\\'+msiFile+'-uninstall.log'
    try:
        os.unlink(logFile)
    except:
        pass
    print "Starting uninstallation..."
    cmd = 'start /wait %s /X%s.msi /QN /L* "%s"' % (MSIEXEC, msiFile, logFile)
    status = os.system(cmd)
    if status == FAIL:
        print "Can't uninstall. MSI file failed to run"
        sys.exit(1)
    checkLog(logFile)

def installMsi(msiFile):
    logFile = 'c:\\spkg\\log\\%s-install.log' % msiFile
    try:
        os.unlink(logFile)
    except:
        pass
    print "Starting installation..."
    cmd = 'start /wait %s /I%s.msi /QN /L* "%s"' % (MSIEXEC, msiFile, logFile)
    status = os.system(cmd)
    if status == FAIL:
        print "Can't install. MSI file failed to run"
        return FAIL
    return checkLog(logFile)

def installIss(issFile, setupFile="setup"):
    cwd = os.getcwd()
    logfile = 'c:\\spkg\\log\\%s-install.log' % issFile
    if os.path.isfile(logfile):
        os.unlink(logfile)
    cmd = 'start /wait %s /s /f1"%s" /f2"%s" /v"/qn /r"'\
          % (setupFile, os.path.join(cwd, issFile), logfile)
    status = os.system(cmd)
    if status != OK:
        print "installing: setup returned an error"
        return FAIL
    return checkIss(issFile)

def uninstallIss(issFile, setupFile="setup"):
    cwd = os.getcwd()
    logfile = 'c:\\spkg\\log\\%s-install.log' % issFile
    cmd = 'start /wait %s /x /f1"%s" /f2"%s" /v"/qn /r"'\
          % (setupFile, os.path.join(cwd, issFile), logfile)
    status = os.system(cmd)
    if status != OK:
        print "uninstalling: setup returned an error"
        sys.exit(1)

def checkIss(filename):
    logfile = 'c:\\spkg\\log\\%s-install.log' % filename
    if not os.path.isfile(logfile):
        print "no logfile, continuing..."
        return OK
    logReader = ConfigParser.ConfigParser()
    try:
        fname = open(logfile,"r")
        logReader.readfp(fname)
        code = logReader.get("ResponseResult", "ResultCode")
        if code != "0":
            print "Non-critical Error installing: %s" % code
            return OK
    except:
        print "Error checking log"
    return OK

def checkLog(filename):
    c = re.compile(".*ailed*")
    f = open(filename, 'r')
    lines = f.readlines()
    if len(lines) < 4:
        print "failed install: MSI problem"
        return FAIL
    for line in lines:
        if c.findall(line):
            print "ERROR: FAILED install: %s" % line
            return FAIL
    return OK

def doubleEscape(oldString):
    outString = ''
    for i in oldString:
        if i == '\\':
            outString += "\\\\"
        else:
            outString += i
    return outString

def convertIfAble(s):
    output = s
    try:
        output = int(s)
    except ValueError:
        pass
    return output

def verifyRegistry(config, section, keypath):
    status = OK
    try:
        r = RegistryDict.RegistryDict(keypath=keypath, flags=win32con.KEY_ALL_ACCESS)
    except pywintypes.error, e:
        if e[0] == 2:
            print "keypath: %s does not exist" % keypath
            return FAIL
    for option in config.options(section):
        if r[option] != convertIfAble(config.get(section, option)):
            print "registry option for %s is %s and should be "\
                  "%s" % (option, r[option], config.get(section, option))
            status = FAIL
    return status

def iniToRegistry(config, section, keypath):
    try:
        r = RegistryDict.RegistryDict(keypath=keypath, flags=win32con.KEY_ALL_ACCESS)
    except pywintypes.error, e:
        if e[0] == 2:
            print "ERROR: keypath: %s does not exist" % keypath
            return FAIL
    for option in config.options(section):
        value = convertIfAble(config.get(section, option))
        r[option] = value
    return OK

def replaceDelimiter(oldField, newField, fileName):
    dataLines = open(fileName, 'r').readlines()
    newData = []
    status = FAIL
    for line in dataLines:
        data = line.strip().split('*')
        newLine = line
        if len(data) < 3:
            newData.append(newLine)
            continue
        for index in range(0,len(data)):
            if oldField == data[index]:
                newLine = string.join(data[0:index], '*')+newField+string.join(data[index+1:])+'\n'
                status = OK
        newData.append(newLine)
    newText = string.join(newData, '')
    open(fileName, 'w').write(newText)
    return status
                
def replaceAut(oldField, newField, fileName=''):
    if fileName == '':
        fileName = "../scripts/config.aut"
    status = replaceInFile(".*\*"+oldField+"\*.*",
                           "Send, "+doubleEscape(newField)+"{TAB}",
                           fileName)
    if status == FAIL:
        print "Error configuring AutoIT"
        sys.exit(1)

def replaceAutV3(oldField, newField, fileName):
    status = replaceDelimiter(oldField, newField, fileName)
    if status == FAIL:
        print "Error configuring AutoItv3 File"
        sys.exit(1)
        
def autoIt(configList, fileName='', wait=0, version=2):
    # replace all components in autoit file:
    if fileName == '':
        shutil.copy("../scripts/config-orig.aut", "../scripts/config.aut")
        fileName = r"..\scripts\config.aut"
    if version == 2:
        ai = os.path.join(miniUtility.getSpkgPath(),AUTOIT_V2)
        for item in configList:
            replaceAut(item[0], item[1], fileName)
    else:
        ai = os.path.join(miniUtility.getSpkgPath(),AUTOIT_V3)
        for item in configList:
            replaceAutV3(item[0], item[1], fileName)
    print "Starting AutoIT..."
    if wait:
        status = os.system('"start /wait '+ai+r' '+fileName)
    else:
        status = os.system('"start '+ai+r' '+fileName)
    if status == FAIL:
        print "Unable to start AutoIT"
        sys.exit(1)

def checksumList(md5list):
    for chunk in md5list:
        fullFilename = chunk[0]
        md5sum = chunk[1]
        lastslash = fullFilename.rfind('\\')+1
        base = fullFilename[0:lastslash]
        filename = fullFilename[lastslash:]
        if not os.path.isdir(base):
            print "ERROR: missing directory: %s" % base
            sys.exit(1)
        if not os.path.isfile(fullFilename):
            print "ERROR: missing file: %s" % filename
            sys.exit(1)
        computed = md5.new(open(fullFilename, 'rb').read()).hexdigest()
        if md5sum == computed:
            continue
        print "ERROR: invalid md5sum for: %s" % filename
        print "ERROR: md5 -- expected: %s got: %s" % (md5sum,computed)
        sys.exit(1)
    print "checksums verified"

def checksum(md5sums, baseDir = ''):
    if type(md5sums) == type(['list']):
        checksumList(md5sums)
    else:
        md5sumList = []
        c1 = md5sums.split('\n')
        for checksumLine in c1:
            data = checksumLine.split(' ')
            if len(data) != 2:
                continue
            csum = data[0]
            filename = data[1]
            if filename.startswith('*'):
                filename = filename[1:]
            path = os.path.join(baseDir, filename)
            md5sumList.append([path, csum])
        checksumList(md5sumList)

def checkRev(versions):
    for checkFile in versions:
        filename = checkFile[0]
        ver = checkFile[1]
        try:
            if version(filename) != ver:
                erstr = "ERROR: Version mismatch. Installed: %s "\
                        "expected: %s" % (version(filename), ver)
                print erstr
                return FAIL
        except:
            erstr = "ERROR: Version check failed- file not "\
                    "found: %s" % filename
            print erstr
            return FAIL
    print "version check succesful"
    return OK

# ===========================
# VBScript Wrappers
# ===========================

def changeCabPath(path):
    # this doesn't seem to work.
    try:
        keyName = "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Setup"
        key = RegistryDict.RegistryDict(keyName)
        key["SourcePath"] = path
        key.close()
        return OK
    except:
        return FAIL

def setenv(variable, value):
    keyName = "SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment"
    portKey = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                         keyName, 0, winreg.KEY_SET_VALUE)
    winreg.SetValueEx(portKey, variable, 0, winreg.REG_SZ, value)

def modifyWebTimeout(index, timeout):
    adsi=win32com.client.Dispatch('ADsNameSpaces')
    adspath='IIS://localhost/w3svc/%s' % index
    try:
        adsobj=adsi.getobject("",adspath)
    except pywintypes.com_error:
        print "Unknown web site"
        return FAIL
    adsobj.ASPSessionTimeout = timeout
    adsobj.SetInfo()
    del adsobj
    return OK
    
def modifyWebPermissions(index, permissions):
    adsi=win32com.client.Dispatch('ADsNameSpaces')
    adspath='IIS://localhost/w3svc/%s/ROOT' % index
    try:
        adsobj=adsi.getobject("",adspath)
    except pywintypes.com_error:
        print "Unknown web site"
        return FAIL
    adsobj.AccessFlags = permissions
    adsobj.SetInfo()
    del adsobj
    return OK

def configCom(targetApp, username, password):
    catalog = win32com.client.Dispatch("COMAdmin.COMAdminCatalog")
    applications = catalog.GetCollection("Applications")
    applications.Populate()
    application = None
    validNames = []
    for obj in applications:
        if obj.Name == None:
            continue
        name = obj.Name
        validNames.append(name)
        if name.lower() == targetApp.lower():
            application = obj
            break
    if application == None:
        print "Invalid application name %s" % targetApp
        print "valid names: %s" % string.join(validNames, ',')
        return FAIL
    application.SetValue("Identity", username)
    application.SetValue("Password", password)
    application.SetValue("RunForever", True)
    applications.SaveChanges()
    del application
    del applications
    del catalog
    return OK

def setPath(path):
    shell = win32com.client.Dispatch("WScript.Shell")
    shell.Environment["Path"] = path
    os.environ["PATH"] = path
    del shell


def normalizer(s):
    for j in range(len(s)):
        if len(s[j]) > 3:
            k = s[j][2:]
        else:
            k = '0' + s[j][2:]
        s[j] = k
    return s
        
def calcversioninfo(fn):
    VOS_DOS             = 0x00010000L
    VOS_NT              = 0x00040000L
    VOS__WINDOWS32      = 0x00000004L
    VOS_DOS_WINDOWS16   = 0x00010001L
    VOS_DOS_WINDOWS32   = 0x00010004L
    VOS_NT_WINDOWS32    = 0x00040004L
    ostypes = [VOS_DOS, VOS_NT, VOS__WINDOWS32, VOS_DOS_WINDOWS16,
               VOS_DOS_WINDOWS32, VOS_NT_WINDOWS32]
    verstrings = []                                        
    sigstrings = findsignatures(fn)
    if sigstrings[0] == '':
        return
    for i in sigstrings:
        FV = normalizer(i.split(',')[8:16])
        FOS = normalizer(i.split(',')[32:36])
        hexver = FV[3]+FV[2]+FV[1]+FV[0]+':'+FV[7]+FV[6]+FV[5]+FV[4]
        OStag = long('0x' + FOS[3]+FOS[2]+FOS[1]+FOS[0] + 'L',16)
        if OStag not in ostypes:
           continue
        if hexver not in verstrings:
           verstrings.append(hexver)                   
    myver = max(verstrings)
    return parsver(myver)

def createparsestruct(b):
    s= ''
    for i in range(len(b)):
        s += hex(ord(b[i]))+','
    return s[:-1]                                         
 
def findsignatures(file):
    f = open(file, 'rb')
    sz = f.read()
    f.close()
    res = []
    indx=sz.find('\xbd\x04\xef\xfe')
    cnt = sz.count('\xbd\x04\xef\xfe')
    while cnt > 1:
        s = createparsestruct(sz[indx:indx+52])
        sz = sz[indx+1:]
        cnt = sz.count('\xbd\x04\xef\xfe')
        indx=sz.find('\xbd\x04\xef\xfe')
        res.append(s)
    res.append(createparsestruct(sz[indx:indx+52]))
    return res

def parsver(v):
    a,b,c,d = v[:4], v[4:8], v[9:13], v[13:]
    return str(int(a,16)) + '.'+ str(int(b,16)) +'.' + str(int(c,16)) + '.' + str(int(d,16))

def flip(byteOne, byteTwo):
    byte1 = int(byteOne, 16)
    byte2 = int(byteTwo, 16)
    return (byte2<<8) + byte1

def version(filename):
    versionInfo = findsignatures(filename)
    vbytes = versionInfo[0].split(',')
    majorRev = flip(vbytes[18], '0x0') 
    minorRev = flip(vbytes[16], vbytes[17]) 
    subrev   = flip(vbytes[14], vbytes[15]) 
    build    = flip(vbytes[12], vbytes[13])
    return `majorRev`+'.'+`minorRev`+'.'+`subrev`+'.'+`build`

