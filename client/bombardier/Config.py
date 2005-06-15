#!/cygdrive/c/Python23/python.exe

import os, ConfigParser, re
from staticData import *
import miniUtility, yaml
import sets
import Exceptions
import Filesystem

# TESTED
def quadToInt(quadString):
    quadIndex = 3
    integer = 0L
    for octet in quadString.split('.'):
        integer += (long(octet) << (quadIndex * 8))
        quadIndex -= 1
    return integer

# TESTED
def computeNet(address, snm):
    if type(address) == type("string"):
        address = quadToInt(address)
    if type(snm) == type("string"):
        snm = quadToInt(snm)
    network = address & snm
    return network

# TESTED
def convertToSlashNotation(quad):
    bits = 0
    quads = quad.split('.')
    if len(quads) != 4: return 0
    for i in quad.split('.'):
        if int(i) < 0 or int(i) > 255:
            return 0
        for j in range(0,8):
            if int(i) == 256-(2**j):
                bits += (8-j)
                break
    return bits

# TESTED
def convertFromSlashNotation(bits):
    if bits == 0: return 0L
    output = 0L
    for i in range(0,31):
        if bits:
            output += 1
            bits -= 1
        output = output << 1
    return output

# TESTED
def convertSlashToNetwork(network):
    try:
        address  = network.split('/')[0]
        maskBits = network.split('/')[1]
    except IndexError:
        print "BAD DATA: %s" % network
        print network.split('/')
        return convertFromSlashNotation(32)
    snm = convertFromSlashNotation(int(maskBits))
    networkInt = computeNet(address, snm)
    return networkInt

# TESTED
def getNetworkList(networkDict):
    addressSet = networkDict["address"]
    networks = []
    for network in addressSet:
        networkInt = convertSlashToNetwork(network)
        networks.append(networkInt)
    return networks

def getMatchStringList( filesystem, patternStr, fileName ):
    pat    = re.compile( patternStr )
    lines  = filesystem.getAllFromFile(patternStr, fileName)
    retSet = sets.Set([])
    if not lines:
        return retSet
    for line in lines:
        m = re.match( pat, line )
        if m:
            retSet.union_update( m.groups() )
    return( retSet ) 

def ipConfig(filesystem):
    cmd="%s > output.txt" % os.path.join(filesystem.environ["WINDIR"], "system32", "ipconfig.exe")
    filesystem.execute(cmd)
    pattern1 = ".*IP Address[\.\s]+\:\s(\S+)"
    pattern2 = ".*Subnet Mask[\.\s]+\:\s(\S+)"
    addresses = []
    snms      = []
    fh = filesystem.open("output.txt", 'r')
    for line in fh.readlines():
        m1 = re.match(pattern1, line)
        m2 = re.match(pattern2, line)
        if m1:
            addresses.append(m1.groups()[0])
        elif m2:
            snms.append(m2.groups()[0])
    addressSet = sets.Set([])
    if len(snms) != len(addresses):
        return addressSet
    for i in range(0, len(addresses)):
        bits = convertToSlashNotation(snms[i])
        addressSet.union_update(["%s/%d" % (addresses[i], bits)])
    return addressSet

# TESTED
def getIpAddress(filesystem=Filesystem.Filesystem()): # needed for backwards compatibility.
    retVal = {'dhcp': sets.Set(), 'address': sets.Set(), 'snm': sets.Set(),
              'defgw': sets.Set(), 'dns': sets.Set(), 'wins': sets.Set()  }
    ipdataList = [ "ipdata1.txt", "ipdata2.txt", "ipdata3.txt"]
    netsh = os.path.join(filesystem.environ["WINDIR"], "system32", "netsh.exe")
    cmd = '%s interface ip show address '\
          '"Local Area Connection" > %s' % (netsh, ipdataList[0])
    filesystem.execute(cmd)
    retVal['snm']     = getMatchStringList( filesystem, ".*SubnetMask\:\s*(\S+)", ipdataList[0] )    
    retVal['dhcp']    = getMatchStringList( filesystem, ".*DHCP enabled\:\s*(\S+)", ipdataList[0] )
    retVal['defgw']   = getMatchStringList( filesystem, ".*Default Gateway\:\s*(\S+)", ipdataList[0] )
    retVal['address'] = ipConfig(filesystem)

    cmd = '%s interface ip show dns '\
          '"Local Area Connection" > %s' % (netsh, ipdataList[1])
    filesystem.execute(cmd)

    retVal['dns'] = getMatchStringList( filesystem, ".*DNS Servers\:\s*(\S+)", ipdataList[1] )

    cmd = '%s interface ip show wins '\
          '"Local Area Connection" > %s' % (netsh, ipdataList[2])
    filesystem.execute(cmd)
    retVal['wins'] = getMatchStringList( filesystem, ".*WINS Servers\:\s*(\S+)", ipdataList[2] )
    return( retVal )

def findParentList(data):
    parentList = []
    for key in data.keys():
        if key.upper() == "PARENTS":
            parents = data[key]
            for index, value in parents.iteritems():
                parentList.append(value)
    return parentList

class Config(dict):

    """This object retains the complete configuration for a system. It
    provides that information either as a dictionary or a
    ConfigParser. It will download its own configuration from the
    repository as well as following any 'parent' chains to create a
    single unified dictionary."""
    
    ### TESTED
    def __init__(self, logger, filesystem, server, windows):
        if logger == None:
            self.logger = miniUtility.Logger()
        else:
            self.logger = logger
        self.filesystem = filesystem
        self.server     = server
        self.windows    = windows
        self.repository = None
        self.config     = ConfigParser.ConfigParser()
        self.parents    = []
        self.automated  = False
        self.data       = {}
        self.username   = None
        self.password   = None
        self.domain     = None

    def __getitem__(self, key):
        return self.data[key]

    def __setitem__(self, key, value):
        self.data[key] = value

    def getPackageGroups(self):
        if self.data.has_key("packageGroups"):
            if type(self.data["packageGroups"]) == type({}):
                packageGroups = []
                for groupId,packageGroup in self.data["packageGroups"].items():
                    packageGroups.append(packageGroup)
                return packageGroups
        if self.data.has_key("system"):
            if self.data["system"].has_key("type"):
                packageGroup = self.data["system"]["type"]
                if type(packageGroup) == type("string"):
                    return ["base", packageGroup]
        return []

    ### TESTED
    def downloadConfig(self, configName):
        self.logger.debug("Downloading configuration data...")
        newData = self.server.serviceYamlRequest("clientconfig", 
                                                 {"client": configName, "type":"YAML"})
        self.data = miniUtility.addDictionaries(self.data, newData)
        self.makeConfigObject()
        newParents = findParentList(newData)
        self.loadParents(newParents)
        dumpFile = self.filesystem.open("config.yml", 'w')
        yaml.dumpToFile(dumpFile, self.data)
        dumpFile.flush()
        dumpFile.close()
        return OK

    def loadParents(self, newParents):
        for parentName in newParents:
            if parentName not in self.parents:
                self.parents.append(parentName)
                self.downloadConfig(parentName)

    def freshen(self):
        savedData = self.data
        self.data = {}
        status = self.downloadConfig(self.filesystem.environ["COMPUTERNAME"])
        if status == FAIL:
            self.data = savedData
            return FAIL
        if self.data.has_key("system"):
            self.username = self.data["system"].get("serviceuser")
            if self.username == None:
                errmsg = "Configuration file has no 'system/username' "\
                         "value: will not be able to get console access"
                self.logger.error(errmsg)
                self.password = None
                self.domain   = None
            else:
                self.password = self.data["system"].get("servicepasswd")
                self.domain   = self.data["system"].get("servicedomain")
        else:
            errmsg = "Configuration file has no 'system' value: "\
                     "will not be able to get console access"
            self.logger.error(errmsg)
            self.username = None
            self.password = None
            self.domain   = None
        return OK

    def makeConfigObject(self):
        self.config = ConfigParser.ConfigParser()
        for section in self.data.keys():
            if not self.config.has_section(section):
                self.config.add_section(section)
            datum = self.data[section]
            if type(datum) == type(dict()):
                for option in datum.keys():
                    value = datum[option]
                    if type(value) != type(dict()):
                        self.config.set(section, option, value)
                    else:
                        ermsg =  "incompatible types (ini/yaml) for (%s:%s)" % (section, option)
                        self.logger.warning(ermsg)
            else:
                pass
                self.logger.warning("incompatible types (ini/yaml) for (%s)" % (section))

    ### TESTED
    def set(self, section, option, value):
        if type(self.data.get(section)) != type(dict()):
            if self.data.get(section) != None:
                ermsg = "Clobbering data in configuration due to yaml/ini incompatibilities"
                self.logger.warning(ermsg)
            self.data[section] = {}
        self.data[section][option] = value
        self.makeConfigObject()
        return OK

    def has_section(self, sectionQuery):
        for section in self.data.keys():
            if section.lower() == sectionQuery.lower():
                if type(self.data[section]) == type({}):
                    return True
        return False

    def has_option(self, sectionQuery, optionQuery):
        for section in self.data.keys():
            if section.lower() == sectionQuery.lower():
                sectData = self.data[section]
                if type(sectData) == type({}):
                    for option in sectData.keys():
                        if option.lower() == optionQuery.lower():
                            return True
        return False

    def options(self, section):
        output = []
        if self.config.has_section(section):
            output = self.config.options(section)
        for parentName in self.parents.keys():
            output += self.parents[parentName].options(section)
        return output

    ### TESTED
    def get(self, section, option, default=''):
        if self.data.has_key(section):
            if self.data[section].has_key(option):
                return self.data[section][option]
        for key in self.data.keys():
            if key.lower() == section.lower():
                if type(self.data[key] == type({})):
                    for subkey in self.data[key].keys():
                        if subkey.lower() == option.lower():
                            return self.data[key][subkey]
        if default:
            if not self.data.has_key(section):
                self.data[section] = {}
            self.data[section][option] = default
            return default
        else:
            if self.data.has_key(section):
                raise ConfigParser.NoOptionError(option, section)
            else:
                raise ConfigParser.NoSectionError(section)

    def addressSet(self):
        if not self.data.has_key("system"):
            return sets.Set([])
        addresses = self.data["system"].get("ipaddress").split(',')
        netmasks  = self.data["system"].get("subnetmask").split(',')
        addressSet = sets.Set([])
        if len(addresses) != len(netmasks):
            if len(netmasks) != 1:
                return sets.Set([])
            netmask = convertToSlashNotation(netmasks[0])
            for address in addresses:
                addressSet = addressSet.union(sets.Set([address+"/"+`netmask`]))
            return addressSet
        else:
            for index in range(0, len(addresses)):
                address = addresses[index]
                netmask = convertToSlashNotation(netmasks[index])
                addressSet = addressSet.union(sets.Set([address+'/'+`netmask`]))
            return addressSet

