import template, string, website.server, os

from static import *

def get(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    body     = open("news.html", 'r').read()
    return template.generateHtml(mainMenuList=website.server.list,
                                 subMenuList=subMenuList,
                                 body=body)
