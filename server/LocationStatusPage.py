from static import *
import cherrypy, Root, time, webUtil
import bombardier.Server
import StatusPage
import Location

def locationDetail(locationName):
    location = Location.Location(locationName)
    output = []
    output.append('<h1>Location %s</h1>' % location.name)
    output.append('<form action="/website/server/locationconfig/%s/" '% locationName)
    output.append('method=POST>')
    output.append('<table>')

    output.append('<tr><td>Description</td>')
    output.append('<td><INPUT TYPE="text" NAME="description" maxlength="50" ')
    output.append('value="%s" size="65"></td></tr>' % location.description)

    output.append('<tr><td><p>YAML configuration data</p><p>(OPTIONAL)</p></td>')
    output.append('<td><TEXTAREA ROWS=20 COLS=50 NAME="data"> ')
    if location.data != None:
        output.append(str(location.stringData))
    output.append('</TEXTAREA></td></tr>')

    output.append('<tr><td colspan=3><hr><input type="submit" ')
    output.append('value="SUBMIT CHANGES"></td></tr> </table>')
    output.append("</form>")
    return '\n'.join(output)

def rowGenerator():
    for locationName in webUtil.getLocationNames():
        location = Location.Location(locationName)
        record = [locationName, location.description, location.configKeys]
        yield record

def mainMenu():
    output = []
    output.append('<h1>Locations</h1>')
    header = []
    header.append('<tr><th>name</th>')
    header.append('<th>description</th>')
    header.append('<th>configuration keys</th></tr>')
    table = webUtil.makeTable('\n'.join(header), rowGenerator)
    output += table
    output.append('<hr>')
    output.append('<form action="/website/server/locationstatus/" method=GET>')
    output.append('Create New Location: ')
    output.append('<INPUT TYPE="text" NAME="location" maxlength="50" value="" size="20">')
    output.append('<input type="submit" value="Create Location">')
    output.append("</form>")
    return '\n'.join(output)

class LocationStatusPage(StatusPage.StatusPage):

    def GET(self, location=None):
        if not location:
            self.title = "Bombardier location"
            self.subtitle = "Locations"
            self.body = mainMenu()
            return self.generateHtml()
        else:
            self.title = "Location %s" % location
            self.subtitle = "Location %s information" % location
            self.body = locationDetail(location)
            return self.generateHtml()

            
        
