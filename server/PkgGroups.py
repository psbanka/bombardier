import cherrypy, Root

import os, string, webUtil

from static import *

def getBomFiles(pkgGroup):
    if pkgGroup == None:
        return webUtil.getBomNames()
    return webUtil.readBom(pkgGroup)

class PkgGroups(Root.Root):

    known_methods = ["GET", "PUT"]

    def PUT(self, group):
        cherrypy.response.headerMap["Content-type"] = "text/plain"
        data = cherrypy.request.body.read()
        bomFile = os.path.join(webUtil.getBomPath(), group+".BOM")
        fd = open(bomFile, 'w')
        fd.write(data)
        return "OK"

    def GET(self, group=None):
        cherrypy.response.headerMap["Content-type"] = "text/plain"
        pkgGroups = getBomFiles(group)
        output = []
        for pkgGroup in pkgGroups:
            if pkgGroup.strip() != '':
                output.append(pkgGroup)
        output.sort()
        return "\n".join(output)

