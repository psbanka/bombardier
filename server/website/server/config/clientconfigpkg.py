import template, string, os, ConfigParser, webUtil
from static import *
from twisted.web import server

"""CALLED FROM CLIENTPACKAGES. This interface page provides for
package configuration for a client. """

#RESTful

def configChanges(args, errlog):
    clientName, configChangeList = args
    output     = []
    config = webUtil.serviceConfigRequest("clientconfig", {"client":clientName})
    for configChange in configChangeList:
        section, option, value = configChange
        required = True
        if option.endswith("*"):
            option = option[:-1] # FIXME: Kludgey-- need to transition to YAML or XML
            required = False
        if value or required:
            output.append("<p>Setting %s / %s to %s</p>" % (section, option, value))
            if not config.has_section(section):
                config.add_section(section)
            config.set(section, option, value)
    status = webUtil.serviceConfigPut("clientconfig", config, {"client":clientName})
    if status == OK:
        output.append("<p><strong>wrote configuration file</strong></p>")
    else:
        output.append("<p><strong>ERROR</strong> writing configuration file. Not saved.</p>")
    return output

def post(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    clientName = request.args.get("client")
    if not clientName:
        return webUtil.err301(request, "./login")
    clientName = clientName[0]
    configChangeList = []
    for arg in request.args.keys():
        if len(arg.split(':')) != 2:
               continue
        section, option = arg.split(':')
        value = request.args.get(arg)[0]
        configChangeList.append([section, option, value])
    pThread = webUtil.Process(request=request,
                              function=configChanges,
                              mainMenuList = website.server.list,
                              subMenuList = subMenuList,
                              args=[clientName, configChangeList])
    pThread.start()
    return server.NOT_DONE_YET

def get(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    return webUtil.err301(request, "./login")

