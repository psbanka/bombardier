import template, string, os
import webUtil
from twisted.web import server

from static import *

# Fully RESTful

def mainMenu(dummy, errlog):
    systemNames = webUtil.serviceRequest("clientconfig")
    output = []
    output.append('<h1>Name of the system to configure</h1><br>')
    output.append('<form action="./login" method=GET>')
    output.append('<table>')
    output.append('<tr><td>Select from a list:</td>')
    output.append('<td><SELECT NAME="clientNameSelecter">')
    for systemName in systemNames:
        output.append('<OPTION>%s' % systemName)
    output.append('</SELECT></td></tr>')        
    output.append('<tr><td>Or type it in</td>')
    output.append('<td><INPUT TYPE="text" NAME="clientname" maxlength="50" size="20">')
    output.append('</td></tr><tr><td colspan=2><hr>')
    output.append('''<input type="submit" value="SUBMIT"></td></tr></table>''')
    output.append('</form>')
    return output

def actionMenu(clientName, errlog):
    output = []
    systemNames = webUtil.serviceRequest("clientconfig")
    if clientName not in systemNames:
        output.append("<h1>Welcome new system %s</h1>" % clientName)
    else:
        output.append("<h1>System %s</h1>" % clientName)
    output.append('<ol><li> <a href="./clientconfig?client=%s">Modify configuration</a>' % clientName)
    output.append('<li> <a href="./clientpackages?client=%s">Modify package groups</a></ol>' % clientName)
    return output

def get(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))

    clientName = request.args.get("clientname")
    if not clientName or clientName[0] == '':
        clientName = request.args.get("clientNameSelecter")
    if not clientName:
        pThread = webUtil.Process(request=request,
                                  function=mainMenu,
                                  mainMenuList = website.server.list,
                                  subMenuList = subMenuList)
        pThread.start()
        return server.NOT_DONE_YET
    else:
        clientName = clientName[0]
        pThread = webUtil.Process(request=request,
                                  function=actionMenu,
                                  mainMenuList = website.server.list,
                                  subMenuList = subMenuList,
                                  args=clientName)
        pThread.start()
        return server.NOT_DONE_YET
        
