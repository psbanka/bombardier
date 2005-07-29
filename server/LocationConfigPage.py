from static import *
import cherrypy, Root, time, yaml
import bombardier.Server
import StatusPage
server = bombardier.Server.Server(None, {"address":"http://127.0.0.1:8080"})
import Location

class LocationConfigPage(StatusPage.StatusPage):

    known_methods = ["POST"]

    def POST(self, name, description, data={}):
        output = []
        status = OK
        location = Location.Location(name)
        
        if data.strip():
            try:
                location.data = yaml.load(data).next()
                if type(location.data) != type(dict()):
                    location.data = {}
            except:
                status = FAIL
                errmsg = "Could not parse YAML configuration data "
        if status == OK:
            location.description = description
            status = location.commit()

            if status == "OK":
                output.append( "<h1>Location %s has been modified</h1>" % name )
                self.title = "Location %s updated" % name
                self.subtitle = "Location %s updated" % name
            else:
                errmsg = "Server returned with message: %s" % status
                status = FAIL
        if status == FAIL:
            output.append("<h1>Location %s has not been modified</h1>" % name)
            output.append("Error details: %s" % errmsg)
            self.title = "Error in %s location data" % name
            self.subtitle = "Location %s updated" % name
        output.append('<a href="/website/server/locationstatus/"><br><p>Return to location summary</a></p>')
        self.body = "\n".join(output)
        return self.generateHtml()
            
        
