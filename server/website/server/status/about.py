import template, string, website.server

from static import *

body = """
<h1>System Configuration</h1>

<p> This section of the system will allow you to monitor the progress
    of installed packages, examine the progress of individual servers,
    and know what version of Bombardier each server is running. </p>

"""


def get(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    return template.generateHtml(mainMenuList=website.server.list,
                                 subMenuList=subMenuList,
                                 body=body)
