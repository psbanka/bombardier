#!/cygdrive/c/Python23/python.exe
import template, string, os, ConfigParser, shelve, webUtil
from twisted.web import server
from static import *

"""It allows the user to add packages, and when they add them, they
can configure the options on the packages."""

# RESTful

def getInstallablePkgGroups(installedPkgGroups):
    possiblePkgGroups = []
    pkgGroups = webUtil.serviceRequest("pkggroups")
    for pkgGroup in pkgGroups:
        if not pkgGroup in installedPkgGroups:
            possiblePkgGroups.append(pkgGroup)
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

def removePkgGroup(oldPkgGroup, pkgGroups, config, errlog):
    if not config.has_section("packageGroups"):
        config.add_section("packageGroups")
    if config.has_section("type"):
        config.remove_option("packageGroups", "type")
    if oldPkgGroup not in pkgGroups:
        errlog.error( "clientpackageGroups: old package group %s is not in the current "\
                      "packageGroups %s" % (oldPkgGroup, pkgGroups))
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
                section, option = tupleString
                required = True
                if option.endswith('*'): # FIXME: Kludgey -- need to switch to YAML or XML
                    option = option[:-1]
                    required = False
                default = configRawData[keyName]
                configItems.append([section,option,default, required])
    return OK, configItems

def configurePkgGroup(pkgGroup, config, clientName, errlog):
    output = []
    packageData = webUtil.serviceYamlRequest("package", errlog, {"type":"yaml"})
    packageNames = webUtil.serviceRequest("pkggroups", {"group":pkgGroup})
    output.append("<h1>Configure Package Group: <strong>%s</strong></h1>" % pkgGroup)
    output.append("<table>")
    for packageName in packageNames:
        status, configItems = configurePkg(packageName, packageData)
        if status == FAIL:
            output.append("<tr><td colspan=4><strong>ERROR:</strong> The "\
                          "package named %s in group %s is not in the "\
                          "repository</td></tr>" % (packageName, pkgGroup))
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
                section, option, default, required = configItem
                if required:
                    output.append('<tr bgcolor="%s"><td>%s</td><td>%s</td><td>%s</td>'\
                                  '<td><INPUT TYPE="text" NAME="%s:%s" maxlength="50" size="20" value="%s">'\
                                  '</td></tr>' % (bgcolor, section, option, default, section, option, default))
                else:
                    output.append('<tr bgcolor="%s" textcolor="#8B000"><td>%s</td><td>%s</td><td>%s</td>'\
                                  '<td><INPUT TYPE="text" NAME="%s:%s*" maxlength="50" size="20" value="%s">'\
                                  '</td></tr>' % (bgcolor, section, option, default, section, option, default))
                    
        else:
            output.append("<tr><td colspan=4>No configuration necessary "\
                          "for package %s</td></tr>" % packageName)
    output.append('<tr><td colspan=4><hr><input type="submit" '\
                  'value="SUBMIT CHANGES"></td></tr> </form>')
    output.append("</table>")
    return output

def changePackageGroups(args, errlog):
    clientName, groupsToAdd, groupsToRemove = args
    output = []
    config  = webUtil.serviceConfigRequest("clientconfig", {"client":clientName})
    pkgGroups = webUtil.getPackageGroups(config)
    for newPackageGroup in groupsToAdd:
        addPkgGroup(newPackageGroup, pkgGroups, config)
        output.append("<p>Added Package group <strong>%s</strong></p>" % newPackageGroup)
    for deadPackageGroup in groupsToRemove:
        removePkgGroup(deadPackageGroup, pkgGroups, config, errlog)
        output.append("<p>Removed Package group <strong>%s</strong></p>" % deadPackageGroup)
    for newPackageGroup in groupsToAdd:
        output += configurePkgGroup(newPackageGroup, config, clientName, errlog)
    webUtil.serviceConfigPut("clientconfig", config, {"client":clientName})
    output.append("<hr><p><a href=./clientpackages?client=%s>Return to packages list "\
                  "page</a></p>" % clientName)
    return output

def addPackageGroups(clientName, errlog):
    output = []
    config  = webUtil.serviceConfigRequest("clientconfig", {"client":clientName})
    if not config.has_section("packageGroups"):
        config.add_section("packageGroups")
    if not config.has_section("system"):
        config.add_section("system")
    pkgGroups = webUtil.getPackageGroups(config)
    possiblePkgGroups = getInstallablePkgGroups(pkgGroups)

    output.append("<h1>Modify package groups for <strong>%s</strong></h1>" % clientName)
    output.append('<form action="./clientpackages?client=%s" method=POST>' % (clientName))
    output.append('<table align="left" >')
    output.append('<tr><td colspan=2><hr><h2>Add package groups</h2></td></tr>')
    output.append('<tr><td>New Package Group</td>')
    output.append('<td><SELECT NAME="newpkggroup"> <OPTION SELECTED>NONE')
    for pkgGroup in possiblePkgGroups:
        output.append('<OPTION>%s' % pkgGroup)
    output.append('</SELECT></td></tr>')        
    if pkgGroups:
        output.append('<tr><td colspan=2><hr><h2>Remove package groups</h2></td></tr>')
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
                  'value="SUBMIT CHANGES"></td></tr> </table>')
    output.append("</form>")
    return output

def get(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    clientName = request.args.get("client")
    if not clientName:
        return webUtil.err301(request, "./login")
    clientName = clientName[0]
    pThread = webUtil.Process(request=request,
                              function=addPackageGroups,
                              mainMenuList = website.server.list,
                              subMenuList = subMenuList,
                              args=clientName)
    pThread.start()
    return server.NOT_DONE_YET

def post(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    clientName = request.args.get("client")
    if not clientName:
        return webUtil.err301(request, "./login")
    clientName = clientName[0]
    groupsToAdd    = []
    groupsToRemove = []
    for key in request.args.keys():
        if key == "client":
            continue
        value = request.args[key][0]
        if key == "newpkggroup":
            if value == "NONE":
                continue
            groupsToAdd.append(value)
        else:
            if value == 'REMOVE':
                groupsToRemove.append(key)
            else:
                errlog.error( "clientpackages/POST: Unknown KEY: %s" % (key) )

    pThread = webUtil.Process(request=request,
                              function=changePackageGroups,
                              mainMenuList = website.server.list,
                              subMenuList = subMenuList,
                              args=[clientName, groupsToAdd, groupsToRemove])
    pThread.start()
    return server.NOT_DONE_YET
