from static import *
import cherrypy, Root, time
import bombardier.Server
import StatusPage

server = bombardier.Server.Server(None, {"address":"http://127.0.0.1:8080"})

class ProjectConfigPage(StatusPage.StatusPage):

    known_methods = ["POST"]

    def POST(self, project, start, finish, contact, clients):
        if type(clients) == type("string"):
            clients = [clients]
        config = {"start":start, "finish":finish, "contact":contact, "clients":clients}
        projectPath = "website/service/putfile/projects/%s.yml/" % project
        serverResponse = server.serviceYamlRequest(projectPath, putData = config,
                                                   debug=True, legacyPathFix=False)
        #write index
        output = []
        if serverResponse == "OK":
            output.append( "<h1>Project %s has been modified</h1>" % project )
            self.title = "Project %s updated" % project
            self.subtitle = "Project %s updated" % project
        else:
            output.append("<h1>Project %s has not been modified</h1>" % project)
            output.append("Error details: %s" % serverResponse)
            self.title = "Error in %s project data" % project
            self.subtitle = "Project %s updated" % project
        output.append('<a href="/website/server/projectstatus/">Return to project summary</a>')
        self.body = "\n".join(output)
        return self.generateHtml()
            
        
