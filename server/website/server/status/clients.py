import template, string, os, webUtil, ConfigParser, yaml
from twisted.web import server

from static import *

def clientDetail(clientName, errlog):
    output = []
    clientStatus = webUtil.serviceYamlRequest("clientstatus", errlog, {"client":clientName})
    output.append('<h1>%s</h1>' % clientName)
    output.append('<p>')
    output.append("<h2>General status</h2><hr>")
    output.append('<table align = "left">')
    if clientStatus["status"]["elapsed"] > (5*60):
        alive = "<strong>NO</strong>"
        bgcolor = "red"
    else:
        alive = "YES"
        bgcolor = "green"
    output.append('<tr><td>Alive</td><td bgcolor=%s>%s</td></tr>' % (bgcolor, alive))
    output.append("<tr><td>Last status</td><td>%s</td></tr>" % clientStatus["status"]["lastStatus"])
    output.append("<tr><td>Last message</td><td>%s</td></tr>" % clientStatus["status"]["lastMessage"])
    output.append("</table>")
    output.append('</p>')
    output.append("<p>.</p>")
    output.append('<p><hr></p>')
    output.append('<p><h2>Installation status</h2><hr></p>')
    output.append('<table align="left" >')
    output.append('<th></th><th></th><th>Status</th>')
    for packageGroupName in clientStatus["installation"].keys():
        packageGroupStatus = clientStatus["installation"][packageGroupName]
        if packageGroupStatus["status"] == "FAIL":
            ermsg = "Group not found"
        else:
            ermsg = ''
        if packageGroupStatus["installedStatus"] == "OK":
            bgcolor = "GREEN"
        else:
            bgcolor = "RED"
        output.append('<tr><td bgcolor=%s colspan=3><strong>%s</strong></td></tr>' % \
                      (bgcolor, packageGroupName))
        packageData = packageGroupStatus["packages"]
        for packageName in packageData.keys():
            packageStatus = packageData[packageName]
            if packageStatus == "OK":
                bgcolor = "green"
            else:
                bgcolor = "red"
            output.append('<tr><td></td><td>%s</td><td bgcolor="%s"> </td></tr>' % (packageName, bgcolor))
    output.append("</table>")
    output.append('</p>')
    return output


def mainMenu(args, errlog):
    systemNames = webUtil.serviceRequest("clientstatus")
    output = []
    output.append('<h1>Provide the name of the system to examine</h1><br>')
    output.append('<form action="./clients" method=POST>')
    output.append('<table>')
    output.append('<tr><td>Select from the list:</td>')
    output.append('<td><SELECT NAME="client">')
    systemNames.sort()
    for systemName in systemNames:
        output.append('<OPTION>%s' % systemName)
    output.append('</SELECT></td></tr>')        
    output.append('<tr><td colspan=2><hr>')
    output.append('''<input type="submit" value="SUBMIT"></td></tr></table>''')
    output.append('</form>')
    return output

def post(request, logger, errlog):
    clientName = request.args.get("client")
    if clientName:
        return webUtil.err301(request, errlog, "./clients?client=%s" % clientName[0])
    else:
        return webUtil.err400(request, errlog, "No client name provided")

def get(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    clientName = request.args.get("client")
    if not clientName:
        #meta = ''
        pThread = webUtil.Process(request,
                                  mainMenu,
                                  errlog,
                                  mainMenuList = website.server.list,
                                  subMenuList = subMenuList)
        pThread.start()
        return server.NOT_DONE_YET
    else:
        clientName = clientName[0]
        #meta = '<META HTTP-EQUIV="REFRESH" CONTENT="50" />'
        pThread = webUtil.Process(request,
                                  clientDetail,
                                  errlog,
                                  mainMenuList = website.server.list,
                                  subMenuList = subMenuList,
                                  args = clientName)
        pThread.start()
        return server.NOT_DONE_YET
