from static import *
import Contact
import cherrypy, Root, time
import bombardier.Server
import StatusPage
server = bombardier.Server.Server(None, {"address":"http://127.0.0.1:8080"})

class ContactConfigPage(StatusPage.StatusPage):

    known_methods = ["POST"]

    def POST(self, name, fullname, email, ownedclients=[], managedclients=[]):
        contact = Contact.Contact(name)
        if type(ownedclients) == type("string"):
            contact.ownedclients = [ownedclients]
        if type(managedclients) == type("string"):
            contact.managedclients = [managedclients]
        contact.fullname = fullname
        contact.email    = email
        status = contact.commit()

        output = []
        if status == "OK":
            output.append( "<h1>Contact %s has been modified</h1>" % contact.name )
            self.title = "Contact %s updated" % contact.name
            self.subtitle = "Contact %s updated" % contact.name
        else:
            output.append("<h1>Contact %s has not been modified</h1>" % contact.name)
            output.append("Error details: %s" % status)
            self.title = "Error in %s contact data" % contact.name
            self.subtitle = "Contact %s updated" % contact.name
        output.append('<a href="/website/server/contactstatus/">Return to contact summary</a>')
        self.body = "\n".join(output)
        return self.generateHtml()
            
        
