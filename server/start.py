#!c:\Python24\python.exe
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
           "global" : {"server.socketPort": TCP_PORT,
                       "server.threadPool": 10,
                       "server.environment": ENVIRONMENT },
           "sessionFilter.default.storageType": "ram",
           "server.logToScreen": LOG_TO_SCREEN}
if LOG_FILE:
    cpconf["server.logFile"]  = LOG_FILE

cherrypy.config.update(cpconf)
cherrypy.server.start()
