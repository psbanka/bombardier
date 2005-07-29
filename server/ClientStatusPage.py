from static import *
import cherrypy, Root, time
import webUtil
import StatusPage
import Client

def clientDetail(clientName):
    output = []
    client = Client.Client(clientName)
    client.getPackageDetail()
    # FIXME: must be defensive
    output.append('<h1>%s</h1>' % client.name)
    output.append('<p>')
    output.append('<table cellpadding="3" cellspacing="0" width=600>')
    output.append('<tr><td><strong>Status</strong></td><td>%s</td></tr>' % client.status)
    output.append('<tr><td>Managers</td><td>%s<td></tr>' % ','.join(client.managers))
    output.append('<tr><td>Owners</td><td>%s<td></tr>' % ','.join(client.owners))
    if client.minSinceUpdate == NEVER:
        output.append('<tr><td>Minutes since contact</td><td>(client has never contacted server)<td></tr>')
    else:
        output.append('<tr><td>Minutes since contact</td><td>%4.2f<td></tr>' % client.minSinceUpdate)
    output.append('<tr><td>Projects</td><td>%s<td></tr>' % ','.join(client.projects))
    output.append('<tr><td>Hardware used</td><td>%s</tr>' % client.hardware)
    output.append('<tr><td>System will be available in</td>')
    output.append('<td> %3.1f days</td</tr>' % client.endDays)
    output.append('</table>')
    output.append('<h2>Last Message</h2>')
    output.append('<p>%s' % client.lastMessage)
    output.append('<br>')
    output.append('</p>')
    output.append('<p><hr></p>')
    
    output.append('<p><h2>Package Installation Status</h2><hr></p>')
    packageStatus = client.packageDetail

    for packageGroupName in packageStatus.keys():
        output.append('<table align="left" >')
        output.append('<th></th><th></th><th>Status</th>')
        packageGroupStatus = packageStatus[packageGroupName]
        if packageGroupStatus["installedStatus"] == "OK":
            bgcolor = "GREEN"
        else:
            bgcolor = "RED"
        output.append('<tr><td bgcolor=%s colspan=3><strong>%s</strong></td></tr>' % \
                      (bgcolor, packageGroupName))
        packageData = packageGroupStatus["packages"]
        for packageName in packageData.keys():
            status = packageData[packageName]
            if status == "OK":
                bgcolor = "green"
            else:
                bgcolor = "red"
            output.append('<tr><td></td><td>%s</td><td bgcolor="%s"> </td></tr>' % (packageName, bgcolor))
        output.append("</table>")
    output.append('</p>')
    return '\n'.join(output)

class ClientStatusPage(StatusPage.StatusPage):

    known_methods = ["GET"]

    def rowGenerator(self):
        clientNames = webUtil.getClientNames()
        clientNames.sort(lambda x,y: cmp(x.lower(), y.lower()))
        
        self.totalClients = len(clientNames)
        self.fullyInstalled = 0
        self.aliveClients   = 0
        for clientName in clientNames:
            client = Client.Client(clientName)
            if client.alive:
                self.aliveClients += 1
                alive = "<st><font color=#669900>yes</em></font>"
            else:
                alive = "<st><font color=#CC0000>no</em></font>"
            if client.percentage == 100:
                self.fullyInstalled += 1
            record = [client.name, client.status, client.hardware, "%3.1f" % client.endDays, 
                      ','.join(client.projects), alive, "%3.1f" % client.percentage]
            yield record
        
    def mainMenu(self):
        output = []
        output.append('<h1>Client Status</h1>')
        header = []
        header.append('<tr><th>Name</th>')
        header.append('<th>status</th>')
        header.append('<th>hardware</th>')
        header.append('<th>Availability</th><th>Project</th>')
        header.append('<th>Alive</th><th>% complete</th></tr>')
        table = webUtil.makeTable('\n'.join(header), self.rowGenerator)
        output.append('<h2>Summary</h2>')
        output.append('<table width=400>')
        output.append('<tr><td>Total clients:</td><td>%4d</td></tr>' % self.totalClients)
        output.append('<tr><td>Clients alive:</td><td>%4d</td></tr>' % self.aliveClients)
        output.append('<tr><td>Clients fully installed:</td><td>%4d</td></tr>' % self.fullyInstalled)
        output.append('</table><hr>')
        output += table
        return '\n'.join(output)

    def GET(self, client=None):
        if not client:
            self.title = "Bombardier Client Status"
            self.subtitle = "Client Client Status"
            self.body = self.mainMenu()
            return self.generateHtml()
        else:
            self.meta = '<META HTTP-EQUIV="REFRESH" CONTENT="50" />'
            self.title = "Client %s" % client
            self.subtitle = "Status for client: %s" % client
            self.body = clientDetail(client)
            return self.generateHtml()

