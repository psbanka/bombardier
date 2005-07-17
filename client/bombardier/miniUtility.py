#!/cygdrive/c/Python24/python.exe

# miniUtility.py: common stuff that many Bombardier modules need.

# Copyright (C) 2005 Peter Banka

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.
import os, Filesystem, re
import _winreg as winreg
from staticData import *

def standAloneMode(filesystem):
    configPath = os.path.join(getSpkgPath(), CONFIG_FILE)
    if os.path.isfile(configPath):
        return True
    return False

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


def ipConfig():
    cmd="%s > output.txt" % os.path.join(os.environ["WINDIR"], "system32", "ipconfig.exe")
    os.system(cmd)
    pattern1 = ".*IP Address[\.\s]+\:\s(\S+)"
    pattern2 = ".*Subnet Mask[\.\s]+\:\s(\S+)"
    addresses = []
    snms      = []
    fh = open("output.txt", 'r')
    for line in fh.readlines():
        m1 = re.match(pattern1, line)
        m2 = re.match(pattern2, line)
        if m1:
            addresses.append(m1.groups()[0])
        elif m2:
            snms.append(m2.groups()[0])
    addressSet = set([])
    if len(snms) != len(addresses):
        return addressSet
    for i in range(0, len(addresses)):
        bits = convertToSlashNotation(snms[i])
        addressSet.union_update(["%s/%d" % (addresses[i], bits)])
    return addressSet

def getMatchStringList( patternStr, fileName ):
    filesystem = Filesystem.Filesystem()
    pat    = re.compile( patternStr )
    lines  = filesystem.getAllFromFile(patternStr, fileName)
    retSet = set([])
    if not lines:
        return retSet
    for line in lines:
        m = re.match( pat, line )
        if m:
            retSet.union_update( m.groups() )
    return( retSet ) 

# TESTED
def getIpAddress():
    retVal = {'dhcp': set(), 'address': set(), 'snm': set(),
              'defgw': set(), 'dns': set(), 'wins': set()  }
    ipdataList = [ "ipdata1.txt", "ipdata2.txt", "ipdata3.txt"]
    netsh = os.path.join(os.environ["WINDIR"], "system32", "netsh.exe")
    cmd = '%s interface ip show address '\
          '"Local Area Connection" > %s' % (netsh, ipdataList[0])
    os.system(cmd)
    retVal['snm']     = getMatchStringList( ".*SubnetMask\:\s*(\S+)",
                                            ipdataList[0] )    
    retVal['dhcp']    = getMatchStringList( ".*DHCP enabled\:\s*(\S+)",
                                            ipdataList[0] )
    retVal['defgw']   = getMatchStringList( ".*Default Gateway\:\s*(\S+)",
                                            ipdataList[0] )
    retVal['address'] = ipConfig()

    cmd = '%s interface ip show dns '\
          '"Local Area Connection" > %s' % (netsh, ipdataList[1])
    os.system(cmd)

    retVal['dns'] = getMatchStringList( ".*DNS Servers\:\s*(\S+)",
                                        ipdataList[1] )

    cmd = '%s interface ip show wins '\
          '"Local Area Connection" > %s' % (netsh, ipdataList[2])
    os.system(cmd)
    retVal['wins'] = getMatchStringList( ".*WINS Servers\:\s*(\S+)",
                                         ipdataList[2] )
    return( retVal )

### TESTED
def addDictionaries(dict1, dict2):
    """dict1 gets stuff from dict2, only if it doesn't have it"""
    #! must be deeper
    for key,value in dict2.iteritems():
        if not dict1.has_key(key):
            dict1[key] = value
        else:
            if type(value) == type(dict1[key]):
                if type(value) == type(dict()):
                    dict1[key] = addDictionaries(dict1[key], value)
    return dict1

class Logger:
    def info(self, string):
        print "info:",string
    def debug(self, string):
        print "debug:",string
    def warning(self, string):
        print "warning:",string
    def error(self, string):
        print "error:",string
    def critical(self, string):
        print "critical:",string
    def rmFileLogging(self):
        pass

def getSpkgPath():
    count = 0
    keyName = r"Software\GE-IT\Bombardier"
    while count < 4:
        key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                            keyName, 0, winreg.KEY_QUERY_VALUE)
        spkgPath, objtype = winreg.QueryValueEx(key, "InstallPath")
        try:
            return str(spkgPath)
        except UnicodeEncodeError:
            count += 1
    return str(spkgPath)

# NOT WORTH TESTING
def getBomPath():
    return os.path.join(getSpkgPath(), BOM_FILE)

def getPythonPath():
    return os.path.join(sys.prefix, "python.exe")

# NOT WORTH TESTING
def getPackagePath():
    return os.path.join(getSpkgPath(), PACKAGES)

# NOT WORTH TESTING
def getBombardierPath():
    return os.path.join(getSpkgPath(), BOMBARDIER)

# TESTED
def getProgressPath():
    newPath = os.path.join(getSpkgPath(), PROGRESS_FILE)
    return newPath

# TESTED
def getProgressPath2():
    newPath = os.path.join(getSpkgPath(), PROGRESS_FILE2)
    return newPath

#### TESTED
def evalBoolean(data):
    if not data:
        return False
    if type(data) != type('string'):
        if type(data) == type(1):
            if data == 1:
                return True
            else:
                return False
        else:
            return False
    data = data.strip().upper()
    if data in ["TRUE", "YES", "1", "OK"]:
        return True
    return False

def connectString(server, instance, port):
    dataSource = server.strip()
    instance = instance.strip()
    port=port.strip()
    if instance:
        dataSource += "\\"+instance
    if port:
        dataSource += ","+port
    return dataSource


def getConnectString(config):
    server = config.get('sql', 'server')
    instance = config.get('sql', 'instance')
    port = config.get('sql', 'port')
    return connectString(server, instance, port)
    
def consoleSync(status):
    consoleFile = os.path.join(getSpkgPath(),CONSOLE_MONITOR)
    f = open(consoleFile, 'w')
    if type(status) == type('a'):
        f.write(status)
    elif type(status) == type(1):
        f.write(`status`)
    else: # assume a zero exit code
        f.write('0')
    f.close()

