import template, string, os, ConfigParser

# REFACTOR: This is nearly identical to the server code

from static import *

def post(request, logger, errlog):
    path = __name__.split('.')
    clientName = request.args.get("client")
    if not clientName:
        webUtil.err301(request, "./login")
    clientName = clientName[0]
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    iniFile    = os.path.join("deploy", "config", "%s.ini" % clientName)
    dataFile = open(os.path.join(iniFile))
    config = ConfigParser.ConfigParser()
    config.readfp(dataFile)
    dataFile.close()
    output = []
    new = {"section":"", "option":"", "value":""}
    for key in request.args.keys():
        value = request.args[key][0]
        if value != '':
            if key == "NEWVAL":
                new["value"] = value
                continue
            elif key == "NEWSECT":
                new["section"] = value
                continue
            elif key == "NEWOPT":
                new["option"] = value
                continue
            else:
                if key.rfind(':') == -1:
                    continue
                section,option = key.split(':')
                output.append("<p>Changing [%s]%s to %s</p>" % (section, option, value))
                config.set(section, option, value)
    if new["section"] and new["value"] and new["option"]:
        output.append("<p>Setting configuration option %s:%s:%s " % \
                      (new["section"], new["option"], new["value"]))
        if not config.has_section(new["section"]):
            config.add_section(new["section"])
        config.set(new["section"], new["option"], new["value"])
    dataFile = open(os.path.join(iniFile), 'w')
    config.write(dataFile)
    dataFile.close()
    output.append("<hr><p><a href=./clientconfig?client=%s>Return to configuration "\
                  "page</a></p>" % clientName)
    return template.generateHtml(mainMenuList=website.client.list,
                                 subMenuList=subMenuList,
                                 body=string.join(output, '\n'))

def get(request, logger, errlog):
    path = __name__.split('.')
    clientName = request.args.get("client")
    if not clientName:
        return webUtil.err301(request, "./login")
    clientName = clientName[0]
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    output = []
    iniFile    = os.path.join("deploy", "config", "%s.ini" % clientName)
    config = ConfigParser.ConfigParser()
    if not os.path.isfile(iniFile):
        output.append("<h1>Welcome new system %s<h1>" % clientName)
        config.add_section("system")
        config.set("system", "maintenanceWindow", "")
        dataFile = open(os.path.join(iniFile), 'w')
        config.write(dataFile)
        dataFile.close()
    else:
        dataFile = open(os.path.join(iniFile))
        config.readfp(dataFile)
        dataFile.close()
        output.append("<h1>System %s</h1>" % clientName)

    output.append('<table align="left" >')
    output.append('<form action="./clientconfig?client=%s" method=POST>' % (clientName))
    for section in config.sections():
        output.append("<tr><td colspan=3><h2>%s</h2></td></tr>" % section)
        output.append("<TH>Option</th><th>Value</th><th>New Value</th></tr>")
        index = 0
        for option in config.options(section):
            index += 1
            if index % 2 == 0:
                bgcolor = "white"
            else:
                bgcolor = "#FFFFCC"
            output.append('<tr bgcolor="%s"><td>%s</td><td>%s</td><td>' % \
                          (bgcolor, option, config.get(section, option)))
            output.append('<INPUT TYPE="text" NAME="%s:%s" maxlength="50" size="20"></td>'\
                          % (section, option))
            output.append("</tr>")

    output.append('<tr><td colspan=3><hr><h2>Add Option</h2></td></tr>')
    output.append("<TH>Section</th><th>Option</th><th>Value</th></tr>")
    output.append('<tr><td><INPUT TYPE="text" NAME="NEWSECT" maxlength="50" size="20"></td>')
    output.append('<td><INPUT TYPE="text" NAME="NEWOPT" maxlength="50" size="20"></td>')
    output.append('<td><INPUT TYPE="text" NAME="NEWVAL" maxlength="50" size="20"></td></tr>')
    output.append('<tr><td colspan=3><hr><input type="submit" value="SUBMIT CHANGES"></td></tr> </form>')
    output.append("</table>")
    return template.generateHtml(mainMenuList=website.client.list,
                                 subMenuList=subMenuList,
                                 body=string.join(output, '\n'))
