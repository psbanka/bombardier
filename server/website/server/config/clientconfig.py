import template, string, os, ConfigParser
import webUtil
from twisted.web import server
from static import *

# now totally RESTful

def modifyConfig(args, errlog):
    clientName, modifyData, newData = args
    config  = webUtil.serviceConfigRequest("clientconfig", {"client":clientName})
    output  = []
    for modification in modifyData:
        section, option, value = modification
        output.append("<p>Changing [%s]%s to %s</p>" % (section, option, value))
        config.set(section, option, value)
    if newData["section"] and newData["value"] and newData["option"]:
        output.append("<p>Setting configuration option %s:%s:%s " % \
                      (newData["section"], newData["option"], newData["value"]))
        if not config.has_section(newData["section"]):
            config.add_section(newData["section"])
        config.set(newData["section"], newData["option"], newData["value"])
    output.append("<hr><p><a href=./clientconfig?client=%s>Return to configuration "\
                  "page</a></p>" % clientName)
    webUtil.serviceConfigPut("clientconfig", config, {"client":clientName})
    return output

def post(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    clientName = request.args.get("client")
    if not clientName:
        webUtil.err301(request, "./login")
    clientName = clientName[0]
    modifyData = []
    newData = {"section":"", "option":"", "value":""}
    for key in request.args.keys():
        value = request.args[key][0]
        if value != '':
            if key == "NEWVAL":
                newData["value"] = value
                continue
            elif key == "NEWSECT":
                newData["section"] = value
                continue
            elif key == "NEWOPT":
                newData["option"] = value
                continue
            else:
                if key.rfind(':') == -1:
                    continue
                section,option = key.split(':')
                modifyData.append([section, option, value])
    pThread = webUtil.Process(request=request,
                              function=modifyConfig,
                              mainMenuList = website.server.list,
                              subMenuList = subMenuList,
                              args=[clientName, modifyData, newData])
    pThread.start()
    return server.NOT_DONE_YET

def showConfig(clientName, errlog):
    output = []
    config = webUtil.serviceConfigRequest("clientconfig", {"client":clientName})
    if len(config.sections()) == 0:
        output.append("<h1>Welcome new system %s</h1>" % clientName)
        config.add_section("system")
        config.set("system", "maintenanceWindow", "")
    else:
        output.append("<h1>Modify configuration for %s</h1>" % clientName)
    output.append('<form action="./clientconfig?client=%s" method=POST>' % (clientName))
    output.append('<table align="left" >')
    for section in config.sections():
        output.append("<tr><td colspan=3><h2>%s</h2></td></tr>" % section)
        output.append("<tr><TH>Option</th><th>Value</th><th>New Value</th></tr>")
        index = 0
        for option in config.options(section):
            index += 1
            if index % 2 == 0:
                bgcolor = "white"
            else:
                bgcolor = "#FFFFCC"
            output.append('<tr bgcolor="%s"><td>%s</td><td>%s</td><td>' % \
                          (bgcolor, option, config.get(section, option)))
            output.append('<INPUT TYPE="text" NAME="%s:%s" maxlength="50" value="%s" size="20"></td>'\
                          % (section, option, config.get(section, option)))
            output.append("</tr>")
    output.append('<tr><td colspan=3><hr><h2>Add Option</h2></td></tr>')
    output.append("<tr><TH>Section</th><th>Option</th><th>Value</th></tr>")
    output.append('<tr><td><INPUT TYPE="text" NAME="NEWSECT" maxlength="50" size="20"></td>')
    output.append(' <td><INPUT TYPE="text" NAME="NEWOPT" maxlength="50" size="20"></td>')
    output.append(' <td><INPUT TYPE="text" NAME="NEWVAL" maxlength="50" size="20"></td></tr>')
    output.append('<tr><td colspan=3><hr><input type="submit" value="SUBMIT CHANGES"></td></tr> </table>')
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
                              function=showConfig,
                              mainMenuList = website.server.list,
                              subMenuList = subMenuList,
                              args=clientName)
    pThread.start()
    return server.NOT_DONE_YET
