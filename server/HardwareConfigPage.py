from static import *
import cherrypy, Root, time
import bombardier.Server
import StatusPage
server = bombardier.Server.Server(None, {"address":"http://127.0.0.1:8080"})

class HardwareConfigPage(StatusPage.StatusPage):

    known_methods = ["POST"]

    def POST(self, hardware, location, description, type, rack, client):
        config = {"location":location, "description":description,
                  "type":type, "rack":rack, "clients":clients}
        hardwarePath = "website/service/putfile/hardwares/%s.yml/" % hardware
        serverResponse = server.serviceYamlRequest(hardwarePath, putData = config, debug=True, legacyPathFix=False)
        output = []
        if serverResponse == "OK":
            output.append( "<h1>Hardware %s has been modified</h1>" % hardware )
            self.title = "Hardware %s updated" % hardware
            self.subtitle = "Hardware %s updated" % hardware
        else:
            output.append("<h1>Hardware %s has not been modified</h1>" % hardware)
            output.append("Error details: %s" % serverResponse)
            self.title = "Error in %s hardware data" % hardware
            self.subtitle = "Hardware %s updated" % hardware
        output.append('<a href="/website/server/hardwarestatus/">Return to hardware summary</a>')
        self.body = "\n".join(output)
        return self.generateHtml()
            
        
