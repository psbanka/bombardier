from static import *
import cherrypy, Root, time, webUtil
import StatusPage
import Project

def projectDetail(projectName):
    project = Project.Project(projectName)
    allClients  = webUtil.getClientNames()
    allClients.sort()
    projectClients = project.clients
    if project.new:
        startTime = time.strftime("%Y-%m-%d")
        endTime   = time.strftime("%Y-%m-%d", time.localtime(time.time()+(60*60*24*30))) # 30 days
        contact   = ""
    else:
        startTime = project.start
        endTime   = project.finish
        contact   = project.contact
    output = []
    output.append('<h1>Project %s</h1>' % projectName)
    output.append('<form action="/website/server/projectconfig/%s/" method=POST>' % (projectName))
    output.append('<table>')
    output.append('<tr><td>Project start date (YYYY-MM-DD)</td>')
    output.append('<td><INPUT TYPE="text" NAME="start" maxlength="50" ')
    output.append('value="%s" size="20"></td></tr>' % startTime)
    output.append('<tr><td>Project end date (YYYY-MM-DD)</td>')
    output.append('<td><INPUT TYPE="text" NAME="finish" maxlength="50" ')
    output.append('value="%s" size="20"></td></tr>' % endTime)
    output.append('<tr><td>Project contact</td><td>')
    output += webUtil.selectionBox(webUtil.getContactNames(), project.contactid, "contact", False)
    output.append('</td></tr>')

    output.append('<tr><td><p>Clients<p>(control-click multiple systems)')
    output.append('</td><td><SELECT multiple size="10" name="clients">')
    for client in allClients:
        if client in projectClients:
            output.append('<OPTION selected="selected" value="%s">%s</OPTION>' % (client, client))
        else:
            output.append('<OPTION>%s</OPTION>' % client)
    output.append('</SELECT></td></tr>')
    output.append('<tr><td colspan=3><hr><input type="submit" value="SUBMIT CHANGES"></td></tr> </table>')
    output.append("</form>")
    return '\n'.join(output)

def rowGenerator():
    projectNames = webUtil.getProjectNames()
    for projectName in projectNames:
        project = Project.Project(projectName)
        record = [project.name, len(project.clients), project.start,
                  project.finish, project.contact]
        yield record

def mainMenu():
    output = []
    header = []
    output.append('<h1>Project summary</h1>')
    header.append('<tr><th>Name</th>')
    header.append('<th>clients</th>')
    header.append('<th>start</th>')
    header.append('<th>finish</th>')
    header.append('<th>contact</th></tr>')
    output += webUtil.makeTable('\n'.join(header), rowGenerator)
    output.append('<hr>')
    output.append('<form action="/website/server/projectstatus/" method=GET>')
    output.append('Create New Project: ')
    output.append('<INPUT TYPE="text" NAME="project" maxlength="50" value="" size="20">')
    output.append('<input type="submit" value="Create Project">')
    output.append("</form>")
    return '\n'.join(output)

class ProjectStatusPage(StatusPage.StatusPage):

    known_methods = ["GET"]

    def GET(self, project=None):
        if not project:
            self.title = "Project status summary"
            self.subtitle = "Project Status Summary"
            self.body = mainMenu()
            return self.generateHtml()
        else:
            self.title = "Project %s" % project
            self.subtitle = "Project %s information" % project
            self.body = projectDetail(project)
            return self.generateHtml()

            
        
