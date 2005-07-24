import Root
import ClientStatusPage
import ProjectStatusPage
import ProjectConfigPage
import ContactStatusPage
import ContactConfigPage
import HardwareStatusPage
import HardwareConfigPage

class Server(Root.Root):

    def __init__(self):
        Root.Root.__init__(self)
        self.clientstatus  = ClientStatusPage.ClientStatusPage()
        self.projectstatus = ProjectStatusPage.ProjectStatusPage()
        self.projectconfig = ProjectConfigPage.ProjectConfigPage()
        self.contactstatus = ContactStatusPage.ContactStatusPage()
        self.contactconfig = ContactConfigPage.ContactConfigPage()
        self.hardwarestatus = HardwareStatusPage.HardwareStatusPage()
        self.hardwareconfig = HardwareConfigPage.HardwareConfigPage()
        
        self.clientstatus.exposed = True
        self.projectstatus.exposed = True
        self.projectconfig.exposed = True
        self.contactstatus.exposed = True
        self.contactconfig.exposed = True
        self.hardwarestatus.exposed = True
        self.hardwareconfig.exposed = True
        self.default = self.clientstatus

    def GET(self):
        return "Server"
