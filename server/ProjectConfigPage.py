from static import *
import cherrypy, Root, time
import bombardier.Server
import StatusPage
import Project

server = bombardier.Server.Server(None, {"address":"http://127.0.0.1:8080"})

class ProjectConfigPage(StatusPage.StatusPage):

    known_methods = ["POST"]

    def POST(self, name, start, finish, contact, clients):
        project = Project.Project(name)
        if type(clients) == type("string"):
            project.clients = [clients]
        else:
            project.clients = clients
        project.start   = start
        project.finish  = finish
        project.contact = contact
        status = project.commit()

        output = []
        if status == "OK":
            output.append( "<h1>Project %s has been modified</h1>" % project )
            self.title = "Project %s updated" % project
            self.subtitle = "Project %s updated" % project
        else:
            output.append("<h1>Project %s has not been modified</h1>" % project)
            output.append("Error details: %s" % status)
            self.title = "Error in %s project data" % project
            self.subtitle = "Project %s updated" % project
        output.append('<a href="/website/server/projectstatus/">Return to project summary</a>')
        self.body = "\n".join(output)
        return self.generateHtml()
            
        
