#!/cygdrive/c/Python24/python.exe
import cherrypy

import Server
import Service
import Root

class WebSite(Root.Root):
    def __init__(self):
        Root.Root.__init__(self)
        self.server  = Server.Server()
        self.service = Service.Service()

        self.server.exposed  = True
        self.service.exposed = True
        self.default = self.server

    def GET(self):
        return "hello"

