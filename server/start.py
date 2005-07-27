#!/cygdrive/c/Python24/python.exe
import cherrypy
import Root
import WebSite
from static import *

cherrypy.root = Root.Root()
cherrypy.root.website = WebSite.WebSite()

# Start the CherryPy server using the configuration file tutorial.conf.
cpconf = { "/deploy" : {"staticFilter.on" : True,
                        "staticFilter.dir" : DEPLOY_DIR,
                        "staticFilter.listing": True},
           "global" : {"server.socketPort": 8080,
                       "server.threadPool": 10,
                       "server.environment": "development" },
                       "sessionFilter.default.storageType": "ram"}

cherrypy.config.update(cpconf)
cherrypy.server.start()