from static import *
import cherrypy, Root, time
import webUtil
import StatusPage
import Client

def clientDetail(clientName):
    clientData = webUtil.readAllClientData()
    projectData = webUtil.readAllProjectData()
    hardwareData = webUtil.readAllHardwareData()
    bomData = webUtil.readAllBomData()
    progressData = webUtil.readAllProgressData()
    contactData = webUtil.readAllContactData()

    output = []
    client = Client.Client(clientName, clientData, contactData,
                           projectData, hardwareData, bomData, progressData)
    client.getInfo()
    client.getPackageDetail()
    # FIXME: must be defensive
    output.append('<h1>%s</h1>' % client.name)
    output.append('<p>')
    output.append('<table cellpadding="3" cellspacing="0" width=400>')
    output.append('<tr><td><strong>Status</strong></td><td>%s</td></tr>' % client.status)
    output.append('<tr><td>Managers</td><td>%s<td></tr>' % ','.join(client.managers))
    output.append('<tr><td>Owners</td><td>%s<td></tr>' % ','.join(client.owners))
    output.append('<tr><td>Minutes since contact</td><td>%4.2f<td></tr>' % client.minSinceUpdate)
    output.append('<tr><td>Projects</td><td>%s<td></tr>' % ','.join(client.projects))
    output.append('<tr><td>Hardware used</td><td>%s</tr>' % client.hardware)
    output.append('<tr><td>System will be available in</td>')
    output.append('<td> %3.1f days</td</tr>' % client.endDays)
    output.append('<tr><td>Last Message</td><td>%s<td></tr>' % client.lastMessage)
    output.append('</table>')
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

def rowGenerator():
    clientData = webUtil.readAllClientData()
    projectData = webUtil.readAllProjectData()
    hardwareData = webUtil.readAllHardwareData()
    bomData = webUtil.readAllBomData()
    progressData = webUtil.readAllProgressData()
    contactData = webUtil.readAllContactData()
    
    clientNames = clientData.keys()
    clientNames.sort()
    for clientName in clientNames:
        client = Client.Client(clientName, clientData, contactData,
                               projectData, hardwareData, bomData, progressData)
        client.getInfo()
        record = [client.name, client.status, client.hardware, "%3.1f" % client.endDays, 
                  ','.join(client.projects), client.alive, "%3.1f" % client.percentage]
        yield record

def mainMenu():
    output = []
    output.append('<h1>Client summary</h1>')
    header = []
    header.append('<tr><th>Name</th>')
    header.append('<th>status</th>')
    header.append('<th>hardware</th>')
    header.append('<th>Availability</th><th>Project</th>')
    header.append('<th>Alive</th><th>% complete</th></tr>')
    output += webUtil.makeTable('\n'.join(header), rowGenerator)
    return '\n'.join(output)

class ClientStatusPage(StatusPage.StatusPage):

    known_methods = ["GET"]

    def GET(self, client=None):
        if not client:
            self.title = "Client status"
            self.subtitle = "Client Status Summary"
            self.body = mainMenu()
            return self.generateHtml()
        else:
            self.meta = '<META HTTP-EQUIV="REFRESH" CONTENT="50" />'
            self.title = "Client %s" % client
            self.subtitle = "Status for client: %s" % client
            self.body = clientDetail(client)
            return self.generateHtml()

