#!/cygdrive/c/Python24/python.exe
import cherrypy
import re, os
from static import *

TEMPLATE_PATH = os.path.join(ROOT_DIR, TEMPLATE)
FOOTER_PATH   = os.path.join(ROOT_DIR, FOOTER)

def substituteFile(fileName, inputData):
    varMatch = re.compile("\%\((.*?)\)s")
    configData = open(fileName, 'r').read()
    variables = varMatch.findall(configData)
    output = []
    for line in configData.split('\n'):
        variables = varMatch.findall(line)
        configDict = {}
        for variable in variables:
            configValue = inputData[variable]
            configDict[variable] = configValue
        if configDict == {}:
            output.append(line)
        else:
            output.append(line % configDict)
    outputData = "\n".join(output)
    return outputData

class Root:

    def __init__(self):
        self.title="Bombardier Server"
        self.heading="Bombardier"
        self.subtitle="Open-source Windows Package Management"
        self.menuList = {}
        self.subMenuList = []
        self.body=''
        self.footer=''
        self.meta = '<META HTTP-EQUIV="REFRESH" CONTENT="500" />'

    known_methods = ["GET", "POST"]

    def default(self, *vpath, **params):
        # Dispatch based on HTTP method (GET, POST, PUT, etc.)
        http_meth = cherrypy.request.method
        if http_meth in self.known_methods:
            meth = getattr(self, http_meth)
            if meth and callable(meth):
                result = meth(*vpath, **params)
                if result is None:
                    cherrypy.response.status = 204 # No response
                return result

        allowed = [x for x in self.known_methods if getattr(self, x,None)]
        cherrypy.response.headerMap["Allow"] = ", ".join(allowed)
        cherrypy.response.status = 405

    default.exposed = True
    index = default   # Makes "default" handle URL with no virtual path

    def GET(self, *args):
        return "GET not implemented for class %s (%s)" % \
               (self.__class__.__name__, cherrypy.request.path)
    def PUT(self, *args):
        return "PUT not implemented for class %s (%s)" % \
               (self.__class__.__name__, cherrypy.request.path)
    def POST(self, *args):
        return "POST not implemented for class %s (%s)" % \
               (self.__class__.__name__, cherrypy.request.path)
    def HEAD(self, *args):
        return "HEAD not implemented for class %s (%s)" % \
               (self.__class__.__name__, cherrypy.request.path)
    def OPTIONS(self, *args):
        return "OPTIONS not implemented for class %s (%s)" % \
               (self.__class__.__name__, cherrypy.request.path)
    def DELETE(self, *args):
        return "DELETE not implemented for class %s (%s)" % \
               (self.__class__.__name__, cherrypy.request.path)
    def TRACE(self, *args):
        return "TRACE not implemented for class %s (%s)" % \
               (self.__class__.__name__, cherrypy.request.path)
    def CONNECT(self, *args):
        return "CONNECT not implemented for class %s (%s)" % \
               (self.__class__.__name__, cherrypy.request.path)

    def _mainMenu(self):
        output = ""
        menuHtml = '        <a href="%(link)s" title="%(explanation)s">%(name)s</a> |'
        for menuItem in self.menuList:
            explanation = menuItem.get('explanation')
            name = menuItem['name']
            link = name
            if menuItem.has_key("link"):
                link = menuItem["link"]
            output += menuHtml % ({'name': name, 'link':link, 'explanation':explanation})
        return output

    def _subMenu(self):
        output = ""
        menuHtml = '      <li><a href="%(link)s" title="%(explanation)s">%(name)s</a></li>'
        for menuItem in self.subMenuList:
            explanation = menuItem.get('explanation')
            name = menuItem['name']
            link = name
            if menuItem.has_key("link"):
                link = menuItem["link"]
            output += menuHtml % ({'name': name, 'link':link, 'explanation':explanation})
        return output

    def generateHtml(self):
        inputData = {"meta": self.meta,
                     "title": self.title,
                     "heading": self.heading,
                     "subtitle": self.subtitle,
                     "menu": self._mainMenu(),
                     "submenu": self._subMenu(),
                     "body": self.body}
        outputData  = substituteFile(TEMPLATE, inputData)
        outputData += substituteFile(FOOTER, {"footer":self.footer})
        return outputData

    def generateNoFooter(self):
        inputData = {"meta": self.meta,
                     "title": self.title,
                     "heading":self.heading,
                     "subtitle":self.subtitle,
                     "menu":self._mainMenu(),
                     "submenu":self._subMenu(),
                     "body":self.body}
        return substituteFile(TEMPLATE, inputData)

