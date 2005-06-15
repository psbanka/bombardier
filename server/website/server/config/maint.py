import template, string, ConfigParser

from static import *

def post(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    output = [ "This method is not working" ]
##     output = []
##     day       = request.args.get("DAY")[0]
##     startHour = request.args.get("startHour")[0]
##     startMin  = request.args.get("startMin")[0]
##     duration  = request.args.get("duration")[0]
##     output.append('<h1>Configure Maintenance Window Successful</h1>')
##     output.append('<p>New maintenance window is %s %s:%s for %s minutes</p>' \
##                   % (day, startHour, startMin, duration))
##     output.append("<br>")
##     config = ConfigParser.ConfigParser()
##     f = open('default.ini', 'w')
##     config.add_section("system")
##     config.set("system", "maintenanceWindow", "%s %s:%s %s" % \
##                (day, startHour, startMin, duration))
##     config.write(f)
##     f.close()
    return template.generateHtml(mainMenuList=website.server.list,
                                 subMenuList=subMenuList,
                                 body=string.join(output, '\n'))

def get(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    output = [ "THIS Method is not working" ]
##     config = ConfigParser.ConfigParser()
##     currentWindow = 'NOT SET'
##     try:
##         config.readfp(open("default.ini"))
##         currentWindow = config.get("system", "maintenanceWindow")
##     except IOError:
##         pass
##     output = []
##     output.append('<h1 id="alt-layout">Configure System Maintenance Window</h1><br>')
##     output.append('<form action="./maint" method=POST>')
##     output.append('<p>Current Maintenance window: %s</p><br>' % currentWindow)
##     output.append('<p>Start time: should be on a 24-hour clock')
##     output.append('<SELECT NAME="DAY">')
##     output.append('<OPTION>SUN<OPTION>MON<OPTION>TUE<OPTION>WED<OPTION>THU<OPTION>FRI<OPTION>SAT<OPTION>SUN')
##     output.append('</SELECT>')
##     output.append('<INPUT TYPE="text" NAME="startHour" maxlength="2" size="2">')
##     output.append(':')
##     output.append('<INPUT TYPE="text" NAME="startMin" maxlength="2" size="2">')
##     output.append('</p><br>')
##     output.append('<p>Duration: (minutes - max 1440)')
##     output.append('<INPUT TYPE="text" NAME="duration" maxlength="4" size="4">')
##     output.append('</p><br>')
##     output.append('''<input type="submit" value="SUBMIT">\n </form></body>''')
    return template.generateHtml(mainMenuList=website.server.list, subMenuList=subMenuList, body=string.join(output, '\n'))
