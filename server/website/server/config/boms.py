import template, os, string, website.server, shelve, webUtil
from twisted.web import server

from static import *

# RESTful.

def mainMenu(dummy, errlog):
    output = []
    output.append('<h1>Modify package groups</h1>')
    bomFiles = webUtil.serviceRequest("pkggroups")
    output.append('<form action="./boms" method=GET>')
    output.append('<table cellpadding=1 cellspacing=20>')
    output.append('<tr><td>Select a package group to modify:</td></tr>')
    output.append('<tr><td><SELECT NAME="bom">')
    output.append('<OPTION>')
    for bomFile in bomFiles:
        output.append('<OPTION>%s' % bomFile)
    output.append('</td></tr>')
    output.append('<tr><td><input type="SUBMIT" value="Select Group">\n </form>')
    output.append('</td></tr></table><hr>')
    return output

def getNewPackages(currentPackages, errlog):
    packageNames = webUtil.serviceRequest("package")
    for packageName in currentPackages:
        if not packageName in packageNames:
            errlog.error( "config/BOMS server: %s isn't a real package" % packageName.strip() )
            continue
        packageNames.remove(packageName)
    return packageNames

def bomInfo(bomFile, errlog):
    output = []
    bomFiles = webUtil.serviceRequest("pkggroups")
    if bomFile in bomFiles:
        output.append('<h1>Packages of group <strong>%s</strong></h1>' % bomFile.lower())
        output.append('<form action="./boms?bom=%s" method=POST>' % bomFile)
        packageNames = webUtil.serviceRequest("pkggroups", {"group":bomFile})
        output.append("<table>")
        output.append('<tr><td colspan=2><h2>Remove Packages</h2></td></tr>')
        index = 0
        for packageName in packageNames:
            if packageName.strip() == "":
                continue
            index += 1
            if index % 2 == 0:
                bgcolor = "white"
            else:
                bgcolor = "#FFFFCC"
            output.append('<tr bgcolor="%s"><td>%s</td><td>' % (bgcolor, packageName))
            output.append('<INPUT TYPE="checkbox" NAME="%s" value="REMOVE"></td></tr>' %\
                          (packageName))

        output.append('<tr><td colspan=2><h2>Add a Package</h2></td></tr>')
        newPackageNames = getNewPackages(packageNames, errlog)
        output.append('<tr><td colspan=2><SELECT NAME="packageAdder">')
        output.append('<OPTION>')
        for packageName in newPackageNames:
            output.append('<OPTION>%s' % packageName)
        output.append('</SELECT></td></tr>')        
        output.append('</table>')
        output.append('<input type="SUBMIT" value="Modify Package Group">\n </form></body>')
    else:
        output.append("<h1>Unknown package group %s" % bomFile)
    return output


def modifyGroup(args, errlog):
    group, packageToAdd, packagesToRemove = args
    packageNames = webUtil.serviceRequest("pkggroups", {"group":group})
    newPackageNames = getNewPackages(packageNames, errlog)
    output = []
    output.append("<h1>Modifying package %s</h1>" % group)
    if packageToAdd != '':
        if not packageToAdd in newPackageNames:
            output.append("<p>Package %s cannot be added to package group %s.</p>" \
                          % (packageToAdd, packageGroupName))
        else:
            packageNames.append(packageToAdd)
            output.append("<p>Added package %s to package group %s.</p>" % \
                          (packageToAdd, group))
    for packageToRemove in packagesToRemove:
        if not packageToRemove in packageNames:
            output.append("<p>Package %s cannot be removed from package group.</p>" % key)
            continue
        packageNames.remove(packageToRemove)
        output.append("<p>Removed package %s from package "\
                      "group %s</p>" % (packageToRemove, group))
    status = webUtil.servicePut("pkggroups",
                                {"group":group},
                                string.join(packageNames, '\n'))
    output.append("<p>Return to package listing for "\
                  "<a href=./boms?bom=%s>%s</a></p>" % (group, group))
    return output


def post(request, logger, errlog):
    if request.args.get("bom") == None:
        return webUtil.err301(request, "./boms")
    group = request.args.get("bom")[0]
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    packageToAdd = request.args["packageAdder"][0]
    packagesToRemove = []
    for key in request.args.keys():
        value = request.args[key][0]
        if value == "REMOVE":
            packagesToRemove.append(key)

    pThread = webUtil.Process(request, 
                              modifyGroup,
                              errlog,
                              mainMenuList = website.server.list,
                              subMenuList = subMenuList,
                              args = [group, packageToAdd, packagesToRemove])

    pThread.start()
    return server.NOT_DONE_YET

def get(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    fullPath = request.args.get("bom")
    if not fullPath:
        pThread = webUtil.Process(request,
                                  mainMenu,
                                  errlog,
                                  mainMenuList = website.server.list,
                                  subMenuList = subMenuList)
        pThread.start()
        return server.NOT_DONE_YET
    else:
        fullPath = fullPath[0]
        pThread = webUtil.Process(request,
                                  bomInfo,
                                  errlog,
                                  mainMenuList = website.server.list,
                                  subMenuList = subMenuList,
                                  args = fullPath)
        pThread.start()
        return server.NOT_DONE_YET

