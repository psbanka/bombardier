import template, os, string, website

from static import *

def post(request, logger, errlog):
    path = request.path
    parts = path.split('/')
    if parts[-1] != "packages":
        if parts[-1].lower() in SUB_MENUS:
            pageName = parts[-1].lower()
            exec('import website.server.%s' % pageName)
            logger.info( 'about returning  website.server.%s.post(request)' % pageName )
            exec('returnData = website.server.%s.post(request)' % pageName)
            return returnData            

def get(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    output = """<h1>System Configuration</h1>

    <p>This section of the website provides the functionality to
    configure the systems and users that access the Bombardier
    server. At the moment, the only feature available is the
    configuration of the system maintenance window.</p>"""
    
    return template.generateHtml(mainMenuList=website.server.list,
                                 subMenuList=subMenuList,
                                 body=output)

