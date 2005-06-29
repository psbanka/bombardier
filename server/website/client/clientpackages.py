#!/cygdrive/c/Python23/python.exe
import template, string, os, ConfigParser, shelve, webUtil

# REFACTOR: This is nearly identical to the server code
from static import *

def getInstallablePkgGroups(installedPkgGroups):
    possiblePkgGroups = []
    fileNames = webUtil.listConfigDir()
    for fileName in fileNames:
        if fileName.upper().endswith(".BOM"):
            index = fileName.upper().rfind(".BOM")
            pkgGroupName = fileName[:index]
            if pkgGroupName in installedPkgGroups:
                continue
            possiblePkgGroups.append(pkgGroupName)
    return possiblePkgGroups

def addPkgGroup(newPkgGroup, pkgGroups, config):
    if not config.has_section("packageGroups"):
        config.add_section("packageGroups")
    if config.has_section("type"):
        config.remove_option("packageGroups", "type")
    index = 0
    for pkgGroup in pkgGroups:
        config.set("packageGroups", "group"+`index`, pkgGroup)
        index += 1
    if newPkgGroup in pkgGroups:
        return
    config.set("packageGroups", "group"+`index`, newPkgGroup)

def rmAllPkgGroups(config):
    index = 0
    keepChecking = True
    while keepChecking:
        if config.has_option("packageGroups", "group"+`index`):
            config.remove_option("packageGroups", "group"+`index`)
            index += 1
        else:
            keepChecking = False
    return OK

def removePkgGroup(oldPkgGroup, pkgGroups, config):
    if not config.has_section("packageGroups"):
        config.add_section("packageGroups")
    if config.has_section("type"):
        config.remove_option("packageGroups", "type")
    if oldPkgGroup not in pkgGroups:
        return
    pkgGroups.remove(oldPkgGroup)
    status = rmAllPkgGroups(config)
    index = 0
    for pkgGroup in pkgGroups:
        config.set("packageGroups", "group"+`index`, pkgGroup)
        index += 1

def configurePkg(packageName, packageData):
    status = OK
    if not packageData.has_key(packageName):
        return FAIL, []
    packageInfo = packageData[packageName]
    configItems = []
    if packageInfo.has_key("config"):
        configRawData  = packageInfo["config"]
        for keyName in configRawData.keys():
            tupleString = keyName.split(',')
            if len(tupleString) == 2:
                section, option = keyName.split(',')
                default = configRawData[keyName]
                configItems.append([section,option,default])
    return OK, configItems

def configurePkgGroup(pkgGroup, config, clientName):
    output = []
    # THis code is broke -pbanka
    #packageData = shelve.open(os.path.join("deploy", "packages.dat"))
    pkgFile = os.path.join("deploy", "config", "%s.BOM" % pkgGroup)
    output.append("<h1>Configure Package Group: <strong>%s</strong></h1>" % pkgGroup)
    output.append("<table>")
    packageNames = open(pkgFile).readlines()
    for packageName in packageNames:
        packageName = packageName.strip()
        status, configItems = configurePkg(packageName, packageData)
        if status == FAIL:
            output.append("<tr><td colspan=4><strong>ERROR:</strong> Package %s "\
                          "is not in the repository</td></tr>" % (packageName))
            continue
        output.append('<form action="./clientconfigpkg?client=%s" method=POST>' % (clientName))
        if configItems:
            output.append("<tr><td colspan=4><h2>%s</h2></td></tr>" % packageName)
            output.append("<tr><th>section</th><th>option</th><th>default</th><th>value</th></tr>")
            index = 0
            for configItem in configItems:
                index += 1
                if index % 2 == 0:
                    bgcolor = "white"
                else:
                    bgcolor = "#FFFFCC"
                section, option, default = configItem
                output.append('<tr bgcolor="%s"><td>%s</td><td>%s</td><td>%s</td>'\
                              '<td><INPUT TYPE="text" value="%s" NAME="%s:%s" maxlength="50" size="20">'\
                              '</td></tr>' % (bgcolor, section, option, default, default, section, option))
        else:
            output.append("<tr><td colspan=4>No configuration necessary "\
                          "for package %s</td></tr>" % packageName)
    output.append('<tr><td colspan=4><hr><input type="submit" '\
                  'value="SUBMIT CHANGES"></td></tr> </form>')
    output.append("</table>")
    return output
                    
def post(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    clientName = request.args.get("client")
    if not clientName:
        return webUtil.err301(request, "./login")
    clientName = clientName[0]
    iniFile    = os.path.join("deploy", "config", "%s.ini" % clientName)
    dataFile = open(os.path.join(iniFile))
    config = ConfigParser.ConfigParser()
    config.readfp(dataFile)
    dataFile.close()
    output = []
    pkgGroups = webUtil.getPackageGroups(config)
    for key in request.args.keys():
        value = request.args[key][0]
        if key == "client":
            continue
        if key == "newpkggroup":
            if value == "NONE":
                continue
            addPkgGroup(value, pkgGroups, config)
            output.append("<p>Added Package group <strong>%s</strong></p>" % value)
            output += configurePkgGroup(value, config, clientName)
        else:
            if key not in pkgGroups:
                output.append("<p><strong>ERROR:</strong> %s not in package list" % key)
                continue
            if value == 'REMOVE':
                removePkgGroup(key, pkgGroups, config)
                output.append("<p>Removed Package group <strong>%s</strong></p>" % key)
            else:
                output.append("<p>KEY: %s, PkgGroups: %s" % (key, pkgGroups))
    dataFile = open(os.path.join(iniFile), 'w')
    config.write(dataFile)
    dataFile.close()
    output.append("<hr><p><a href=./clientpackages?client=%s>Return to packages list "\
                  "page</a></p>" % clientName)
    return template.generateHtml(mainMenuList=website.client.list,
                                 subMenuList=subMenuList,
                                 body=string.join(output, '\n'))

def get(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    clientName = request.args.get("client")
    if not clientName:
        return webUtil.err301(request, "./login")
    clientName = clientName[0]
    output = []
    iniFile    = os.path.join("deploy", "config", "%s.ini" % clientName)
    config = ConfigParser.ConfigParser()
    if not os.path.isfile(iniFile):
        output.append("<h1>New system: <strong>%s</strong></h1>" % clientName)
        config.add_section("system")
        config.add_section("packageGroups")
        dataFile = open(os.path.join(iniFile), 'w')
        config.write(dataFile)
        dataFile.close()
    else:
        dataFile = open(os.path.join(iniFile))
        config.readfp(dataFile)
        dataFile.close()
        output.append("<h1>System: <strong>%s</strong></h1>" % clientName)
        if not config.has_section("packageGroups"):
            config.add_section("packageGroups")
        if not config.has_section("system"):
            config.add_section("system")
    pkgGroups = webUtil.getPackageGroups(config)
    pkgGroups.sort()
    output.append('<table align="left" >')
    output.append('<form action="./clientpackages?client=%s" method=POST>' % (clientName))
    output.append('<tr><td colspan=2><hr><h2>Add Package Groups:</h2></td></tr>')
    output.append('<tr><td>New Package Group</td>')
    possiblePkgGroups = getInstallablePkgGroups(pkgGroups)
    output.append('<td><SELECT NAME="newpkggroup"> <OPTION SELECTED>NONE')
    for pkgGroup in possiblePkgGroups:
        output.append('<OPTION>%s' % pkgGroup)
    output.append('</SELECT></td></tr>')        

    if pkgGroups:
        output.append('<tr><td colspan=2><hr><h2>Remove Package Groups:</h2></td></tr>')
        output.append("<TH>Package Group</th><th>Remove</th></tr>")
        index = 0
        for pkgGroup in pkgGroups:
            index += 1
            if index % 2 == 0:
                bgcolor = "white"
            else:
                bgcolor = "#FFFFCC"
            output.append('<tr bgcolor="%s"><td>%s</td><td><INPUT type="checkbox" name="%s" '\
                          'value="REMOVE"></td></tr>' % (bgcolor, pkgGroup, pkgGroup))
    output.append('<tr><td colspan=3><hr><input type="submit" '\
                  'value="SUBMIT CHANGES"></td></tr> </form>')
    output.append("</table>")
    return template.generateHtml(mainMenuList=website.client.list,
                                 subMenuList=subMenuList,
                                 body=string.join(output, '\n'))
