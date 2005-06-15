import template, string, os, ConfigParser, webUtil


# REFACTOR: This is nearly identical to the server code

from static import *

def post(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    output     = []
    clientName = request.args.get("client")
    if not clientName:
        return webUtil.err301(request, "./login")
    clientName = clientName[0]
    iniFile    = os.path.join("deploy", "config", "%s.ini" % clientName)
    dataFile   = open(os.path.join(iniFile))
    config     = ConfigParser.ConfigParser()
    config.readfp(dataFile)
    dataFile.close()
    for arg in request.args.keys():
        if len(arg.split(':')) != 2:
               continue
        section, option = arg.split(':')
        value = request.args.get(arg)[0]
        output.append("<p>Setting %s / %s to %s</p>" % (section, option, value))
        if not config.has_section(section):
            config.add_section(section)
        config.set(section, option, value)
    dataFile   = open(os.path.join(iniFile), 'w')
    config.write(dataFile)
    dataFile.close()
    output.append("<h1>Prepare for reboot</h1>")
    output.append("<p>or choose start -> programs -> Bombardier -> Bombardier</p>")
    output.append("<p><a href=./clientpackages?client=%s>Return to "\
                  "package selection</a></p>" % clientName)
    return template.generateHtml(mainMenuList=website.client.list,
                                 subMenuList=subMenuList,
                                 body=string.join(output, '\n'))

def get(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    return webUtil.err301(request, "./login")

