import template, os, string, website.server, shelve, webUtil

from static import *

def getBomFiles(pkgGroup):
    if pkgGroup == None:
        files = os.listdir(os.path.join(ROOT_DIR, 'deploy', 'config'))
        bomFiles = []
        for file in files:
            if file.upper().endswith(".BOM"):
                bomFiles.append(file[:file.upper().rfind(".BOM")])
        return bomFiles
    else:
        pkgFilePath = os.path.join(ROOT_DIR, "deploy", "config", pkgGroup[0]+".BOM")
        if os.path.isfile(pkgFilePath):
            packageNames = open(pkgFilePath, 'r').readlines()
            return map(string.strip, packageNames)
    return []

def put(request, logger, errlog):
    path = __name__.split('.')
    if request.args.get("group") == None:
        return webUtil.err400(request, errlog, "Must provide group name",
                              "provide group=[groupname] in querystring")
    else:
        group = request.args.get("group")[0]
        data = request.content.read()
        bomFile = os.path.join(ROOT_DIR, "deploy", "config", group+".BOM")
        fd = open(bomFile, 'w')
        fd.write(data)
        output = "OK"
    return output
 

def get(request, logger, errlog):
    path = __name__.split('.')
    pkgGroups = getBomFiles(request.args.get("group"))
    output = []
    for pkgGroup in pkgGroups:
        if pkgGroup.strip() != '':
            output.append(pkgGroup)
    output.sort()
    return string.join(output, '\n')

