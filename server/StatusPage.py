from static import *
import Root
import bombardier.Server

class StatusPage(Root.Root):

    known_methods = ["GET"]

    def __init__(self):
        Root.Root.__init__(self)
        self.subMenuList = [{"name":"Clients", "link":"/website/server/clientstatus/"},
                            {"name":"Projects", "link":"/website/server/projectstatus/",
                             "explanation":"Describe the contacts servers are used for"},
                            {"name":"Contacts", "link":"/website/server/contactstatus/",
                             "explanation":"Describe the people responsible for systems"},
                            {"name":"Hardware", "link":"/website/server/hardwarestatus/",
                             "explanation":"Describe the hardware that you have available"}]
        self.menuList= [{"name":"Status", "link":"/website/server/clientstatus/"}]

        
        
