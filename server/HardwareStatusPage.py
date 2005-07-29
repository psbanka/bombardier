from static import *
import cherrypy, Root, time, webUtil
import bombardier.Server
import StatusPage
import Hardware

def hardwareDetail(hardwareName):
    hardware = Hardware.Hardware(hardwareName)
    output = []
    output.append('<h1>Hardware %s</h1>' % hardware.name)
    output.append('<form action="/website/server/hardwareconfig/%s/" '% hardwareName)
    output.append('method=POST>')
    output.append('<table>')
    output.append('<tr><td>Location </td><td>')

    output += webUtil.selectionBox(webUtil.getLocationNames(), hardware.location,
                                   name="location", multi=False)
    output.append('</td></tr>')

    output.append('<tr><td>Description</td>')
    output.append('<td><INPUT TYPE="text" NAME="description" maxlength="50" ')
    output.append('value="%s" size="20"></td></tr>' % hardware.description)

    output.append('<tr><td>Type</td>')
    output.append('<td><INPUT TYPE="text" NAME="type" maxlength="50" ')
    output.append('value="%s" size="20"></td></tr>' % hardware.type)

    output.append('<tr><td>Rack</td>')
    output.append('<td><INPUT TYPE="text" NAME="rack" maxlength="50" ')
    output.append('value="%s" size="20"></td></tr>' % hardware.rack)

    output.append('<tr><td>Client</td><td>')
    output += webUtil.clientSelectionBox(hardware.client, name="client", multi=False)
    output.append('</td></tr>')

    output.append('<tr><td colspan=3><hr><input type="submit" ')
    output.append('value="SUBMIT CHANGES"></td></tr> </table>')
    output.append("</form>")
    return '\n'.join(output)

def rowGenerator():
    for hardwareName in webUtil.getHardwareNames():
        hardware = Hardware.Hardware(hardwareName)
        record = [hardware.name, hardware.location, hardware.description,
                  hardware.type, hardware.rack, hardware.client]
        yield record

def mainMenu():
    output = []
    output.append('<h1>Hardware summary</h1>')
    header = []
    header.append('<tr><th>name</th>')
    header.append('<th>location</th>')
    header.append('<th>description</th>')
    header.append('<th>type</th>')
    header.append('<th>rack</th>')
    header.append('<th>client</th></tr>')
    table = webUtil.makeTable('\n'.join(header), rowGenerator)
    output += table
    output.append('<hr>')
    output.append('<form action="/website/server/hardwarestatus/" method=GET>')
    output.append('Create New Hardware: ')
    output.append('<INPUT TYPE="text" NAME="hardware" maxlength="50" value="" size="20">')
    output.append('<input type="submit" value="Create Hardware">')
    output.append("</form>")
    return '\n'.join(output)

class HardwareStatusPage(StatusPage.StatusPage):

    def GET(self, hardware=None):
        if not hardware:
            self.title = "Hardware status summary"
            self.subtitle = "Hardware Status Summary"
            self.body = mainMenu()
            return self.generateHtml()
        else:
            self.title = "Hardware %s" % hardware
            self.subtitle = "Hardware %s information" % hardware
            self.body = hardwareDetail(hardware)
            return self.generateHtml()

            
        
