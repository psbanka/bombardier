import Root
import ClientConfig
import ClientStatus
import ClientLog
import Package
import PkgGroups
import Vlan
import PutFile

class Service(Root.Root):

    def __init__(self):
        Root.Root.__init__(self)
        self.clientconfig  = ClientConfig.ClientConfig()
        self.clientstatus  = ClientStatus.ClientStatus()
        self.clientlog     = ClientLog.ClientLog()
        self.package       = Package.Package()
        self.pkggroups     = PkgGroups.PkgGroups()
        self.vlan          = Vlan.Vlan()
        self.putfile       = PutFile.PutFile()
        
        self.clientconfig.exposed  = True
        self.clientstatus.exposed  = True
        self.clientlog.exposed     = True
        self.package.exposed       = True
        self.pkggroups.exposed     = True
        self.vlan.exposed          = True
        self.putfile.exposed       = True
