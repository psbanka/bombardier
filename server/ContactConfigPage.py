from static import *
import cherrypy, Root, time
import bombardier.Server
import StatusPage
server = bombardier.Server.Server(None, {"address":"http://127.0.0.1:8080"})

class ContactConfigPage(StatusPage.StatusPage):

    known_methods = ["POST"]

    def POST(self, contact, fullname, email, ownedclients=[], managedclients=[]):
        if type(ownedclients) == type("string"):
            ownedclients = [ownedclients]
        if type(managedclients) == type("string"):
            managedclients = [managedclients]
        config = {"fullname":fullname, "email":email,
                  "ownedclients":ownedclients, "managedclients":managedclients}
        contactPath = "website/service/putfile/contacts/%s.yml/" % contact
        serverResponse = server.serviceYamlRequest(contactPath, putData = config, debug=True)
        output = []
        if serverResponse == "OK":
            output.append( "<h1>Contact %s has been modified</h1>" % contact )
            self.title = "Contact %s updated" % contact
            self.subtitle = "Contact %s updated" % contact
        else:
            output.append("<h1>Contact %s has not been modified</h1>" % contact)
            output.append("Error details: %s" % serverResponse)
            self.title = "Error in %s contact data" % contact
            self.subtitle = "Contact %s updated" % contact
        output.append('<a href="/website/server/contactstatus/">Return to contact summary</a>')
        self.body = "\n".join(output)
        return self.generateHtml()
            
        
