from static import *
import cherrypy, Root, time, webUtil
import bombardier.Server
import StatusPage
import Contact

def contactDetail(contactName):
    contact = Contact.Contact(contactName)
    output = []
    output.append('<h1>Contact %s</h1>' % contact.name)
    output.append('<form action="/website/server/contactconfig/%s/" '% contactName)
    output.append('method=POST>')
    output.append('<table>')
    output.append('<tr><td>Full name </td>')
    output.append('<td><INPUT TYPE="text" NAME="fullname" maxlength="50" ')
    output.append('value="%s" size="20"></td></tr>' % contact.fullname)

    output.append('<tr><td>Email address (name@domain)</td>')
    output.append('<td><INPUT TYPE="text" NAME="email" maxlength="50" ')
    output.append('value="%s" size="20"></td></tr>' % contact.email)

    output.append('<tr><td><p>Owner of clients<p>(control-click multiple clients)</td><td>')
    output += webUtil.clientSelectionBox(contact.ownedclients, "ownedclients")
    output.append('</td></tr>')

    output.append('<tr><td><p>Manager of clients<p>(control-click multiple clients)</td><td>')
    output += webUtil.clientSelectionBox(contact.managedclients, "managedclients")
    output.append('</td></tr>')

    output.append('<tr><td colspan=3><hr><input type="submit" ')
    output.append('value="SUBMIT CHANGES"></td></tr> </table>')
    output.append("</form>")
    return '\n'.join(output)

def rowGenerator():
    for contactName in webUtil.getContactNames():
        contact = Contact.Contact(contactName)
        projectsNum       = 0
        ownedClientsNum   = 0
        managedClientsNum = 0
        if type(contact.projects) == type([]):
            projectsNum = len( contact.projects )
        if type(contact.ownedclients) == type([]):
            ownedClientsNum = len( contact.ownedclients )
        if type(contact.managedclients) == type([]):
            managedClientsNum = len( contact.managedclients )
        record = [contact.name, contact.fullname, contact.email, projectsNum,
                  ownedClientsNum, managedClientsNum]
        yield record

def mainMenu():
    output = []
    output.append('<h1>Contact summary</h1>')
    header = []
    header.append('<tr><th>id</th>')
    header.append('<th>full name</th>')
    header.append('<th>email</th>')
    header.append('<th>projects</th>')
    header.append('<th>owned clients</th>')
    header.append('<th>managed clients</th></tr>')
    table = webUtil.makeTable('\n'.join(header), rowGenerator)
    output += table
    output.append('<hr>')
    output.append('<form action="/website/server/contactstatus/" method=GET>')
    output.append('Create New Contact: ')
    output.append('<INPUT TYPE="text" NAME="contact" maxlength="50" value="" size="20">')
    output.append('<input type="submit" value="Create Contact">')
    output.append("</form>")
    return '\n'.join(output)

class ContactStatusPage(StatusPage.StatusPage):

    def GET(self, contact=None):
        if not contact:
            self.title = "Contact status summary"
            self.subtitle = "Contact Status Summary"
            self.body = mainMenu()
            return self.generateHtml()
        else:
            self.title = "Contact %s" % contact
            self.subtitle = "Contact %s information" % contact
            self.body = contactDetail(contact)
            return self.generateHtml()

            
        
