import cherrypy
import Root

import webUtil, os, yaml, time

from static import *

"""This thing allows clients to add to their logging entries. This
provides a timeline function for the administrator to know the history
of a given machine."""

def doc():
    return """
get:
    [client] <section> : Returns all log entries for the given
             client name and optionally filtering for section.

put: 
    [client] [severity] <section>: adds an entry to the log for this client,
             indicating severity and message details. Section is optional and only 

    """

class ClientLog(Root.Root):

    known_methods = ["GET", "POST", "PUT"]

    def writeLast(self, logEntry, configData):
        repeat    = False
        logPath   = os.path.join(webUtil.getLogPath(), configData["client"])
        lastPath  = os.path.join(logPath, LAST_STATUS)
        try:
            fh        = open(lastPath, 'r')
            lastData  = yaml.load(fh).next()
            fh.close()
        except:
            lastData = {"severity": "none", "section":"none", "message":"none"}
        if lastData["severity"] == configData["severity"]:
            if lastData["section"] == configData["section"]:
                if lastData["message"] == lastData["message"]:
                    repeat = True
        fh        = open(lastPath, 'w')
        fh.write(logEntry)
        fh.close()
        return repeat

    def writeStatus(self, configData):
        logEntry   = yaml.dump(configData)
        logPath    = os.path.join(webUtil.getLogPath(), configData["client"])
        print ">>>>>>>>>>>>>>>",logPath
        statusPath = os.path.join(logPath, STATUS_FILE)
        if not os.path.isdir(logPath):
            cherrypy.log("Making directory %s" % logPath)
            os.makedirs(logPath)
        if not os.path.isfile(statusPath):
            fh = open(statusPath, 'w')
        else:
            fh = open(statusPath, 'a')
        repeat = self.writeLast(logEntry, configData)
        if not repeat:
            fh.write(logEntry)
        return "OK\n"

    def PUT(self, client, severity, section=None):
        cherrypy.response.headerMap["Content-type"] = "text/plain"
        if not section:
            section = GENERAL
        message = cherrypy.request.body.read()
        configData = {"client": client, "severity":severity, "message":message,
                      "section": section, "time": time.ctime()}
        return self.writeStatus(configData)

    def GET(self, client, section=None):
        cherrypy.response.headerMap["Content-type"] = "text/plain"
        output = []
        statusPath = os.path.join(webUtil.getClientPath(), client, STATUS_FILE)
        if os.path.isfile(statusPath):
            yamldata = yaml.loadFile(statusPath)
            try:
                for entry in yamldata:
                    if section and entry["section"].upper() != section.upper():
                        continue
                    output.append(yaml.dump(entry))
                return "\n".join(output)
            except:
                cherrypy.log("No status data in %s" % statusPath)
        else:
            cherrypy.log("No status file %s" % statusPath)
            return ""

