import template, string, os

# REFACTOR: This is nearly identical to the server code

from static import *

def getSystemNames():
    baseDir = os.path.join('deploy','config')
    inodes = os.listdir(baseDir)
    systemNames = ['']
    for inode in inodes:
        if os.path.isfile(os.path.join(baseDir,inode)) and inode.endswith('.ini'):
            systemName = inode[:inode.rfind('.ini')]
            if systemName != '':
                systemNames.append(systemName)
    systemNames.sort()
    return systemNames

def post(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    clientName = request.args.get("clientname")[0]
    if not clientName:
        clientName = request.args.get("clientNameSelecter")[0]
    if not clientName:
        return '<h1>Invalid input</h1>'
    output = []
    clientPath = os.path.join("client", clientName)
    iniFile    = os.path.join("deploy", "config", "%s.ini" % clientName)
    if not os.path.isdir(clientPath) and not os.path.isfile(iniFile):
        output.append("<h1>Welcome new system %s</h1>" % clientName)
    else:
        output.append("<h1>System %s</h1>" % clientName)
    output.append('<ul><li> <a href="./clientconfig?client=%s">Modify configuration</a>' % clientName)
    output.append('<li> <a href="./clientpackages?client=%s">Modify packages</a></ol>' % clientName)
    return template.generateHtml(mainMenuList=website.client.list,
                                 subMenuList=subMenuList,
                                 body=string.join(output, '\n'))

def get(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    systemNames = getSystemNames()
    output = []
    output.append('<h1>Provide the name of the system to manage</h1><br>')
    output.append('<form action="./login" method=POST>')
    output.append('<table>')
    output.append('<tr><td>Enter your system name:</td>')
    output.append('<td><INPUT TYPE="text" NAME="clientname" maxlength="50" size="20">')
    output.append('</td></tr><tr><td colspan=2><hr>')
    output.append('''<input type="submit" value="SUBMIT"></td></tr></table>''')
    output.append('</form>')
    return template.generateHtml(mainMenuList=website.client.list,
                                 subMenuList=subMenuList,
                                 body=string.join(output, '\n'))
