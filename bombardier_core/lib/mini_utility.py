#!/cygdrive/c/Python24/python.exe

# mini_utility.py: common stuff that many Bombardier modules need.

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

import os, re, time, random, yaml, md5
import sys
from static_data import OK, FAIL
from static_data import CONSOLE_MONITOR, PACKAGES, STATUS_FILE, CONFIG_FILE

BROKEN_INSTALL   = 0
INSTALLED        = 1
BROKEN_UNINSTALL = 2
UNINSTALLED      = 3

def cygpath(dosPath):
    prefix = ''
    if not dosPath:
        return ''
    if ':' in dosPath:
        drive = dosPath.split(':')[0]
        prefix = '/cygdrive/%s' % drive
        dosPath = dosPath.split(':')[-1]
    return prefix + dosPath.replace('\\', '/')

def cyghome():
    import RegistryDict
    a = RegistryDict.RegistryDict(r"SOFTWARE\Cygnus Solutions\Cygwin\mounts v2\/")
    cygwinRoot = a["native"].replace('\\', '/')
    return cygwinRoot

def dospath(cygpath):
    cygwinRoot = cyghome()
    if cygpath.startswith("/cygdrive/"):
        elements = cygpath.split('/')
        drive    = elements[2]
        dosPath  = drive+':/'+'/'.join(elements[3:])
    else:
        dosPath  = cygwinRoot+cygpath
    return dosPath

def hashList(l):
    r = []
    for value in l:
        if type(value) == type({}):
            r.append(hashDictionary(value))
        elif type(value) == type([]):
            r.append(hashList(value))
        elif type(value) == type('string'):
            r.append(md5.new(value).hexdigest())
        elif  type(value) == type(1):
            r.append(md5.new(`value`).hexdigest())
    return r

def hashDictionary(d):
    r = {}
    for key in d.keys():
        value = d[key]
        if type(value) == type({}):
            r[key] = hashDictionary(value)
        elif type(value) == type([]):
            r[key] = hashList(value)
        elif type(value) == type('string'):
            r[key] = md5.new(value).hexdigest()
        elif type(value) == type(1):
            r[key] = md5.new(`value`).hexdigest()
    return r


def diffLists(sub, super, checkValues=False):
    differences = []
    #print "compareLists: (%s/%s)" % (sub, super)
    if len(sub) == 0:
        return []
    if type(sub[0]) == type(super[0]):
        if type(sub[0]) == type(1) or type(sub[0]) == type('string'):
            # comparing a list of literals, length doesn't matter.
            return []
    if len(sub) != len(super):
        differences = list(set(sub) - set(super))
    for index in range(0, len(sub)):
        subValue = sub[index]
        superValue = super[index]
        if type(subValue) != type(superValue):
            differences.append(subValue)
            continue
        if type(subValue) == type({}):
            differences.append(diffDicts(subValue, superValue, checkValues))
            continue
        elif type(subValue) == type([]):
            differences.append(diffLists(subValue, superValue, checkValues))
            continue
        elif type(subValue) == type('string') or type(subValue) == type(1):
            if checkValues and subValue != superValue:
                differences.append(subValue)
                continue
        else:
            differences.append(subValue)
            continue
    return differences

def diffDicts(sub, super, checkValues=False):
    #print "diffDicts: (\nsub\n%s\nsuper\n%s)" % (sub, super)
    differences = {}
    for subKey in sub.keys():
        #print "SUBKEY:", subKey
        if subKey not in super.keys():
            differences[subKey] = sub[subKey]
            #print "Found 0"
            continue
        subValue = sub[subKey]
        superValue = super[subKey]
        if type(subValue) == type('string') or type(subValue) == type(1):
            if type(superValue) != type('string') and type(superValue) != type(1):
                differences[subKey] = subValue
                #print "Found 1"
                continue
            if not checkValues:
                continue
            if subValue != superValue:
                differences[subKey] = subValue
                #print "Found 2"
                continue
            continue
        elif type(subValue) != type(superValue):
            differences[subKey] = subValue
            #print "Found 3"
            continue
        elif type(subValue) == type({}):
            diff = diffDicts(subValue, superValue, checkValues)
            if diff != {}:
                differences[subKey] = diff
                #print "Found 4", checkValues, diff
            continue
        elif type(subValue) == type([]):
            diff = diffLists(subValue, superValue, checkValues)
            if diff != []:
                differences[subKey] = diff
                #print "Found 5"
            continue
        else:
            differences[subKey] = subValue
            #print "Found 6"
            continue
    return differences

def compareLists(sub, super, checkValues=False):
    if diffLists(sub, super, checkValues) == []:
        return True
    return False

def compareDicts(sub, super, checkValues=False):
    if diffDicts(sub, super, checkValues) == {}:
        return True
    return False

def datesort(x, y):
    if type(x) == type(["list"]):
        if type(y) == type(["list"]):
            if x[1]:
                if y[1]:
                    return x[1] - y[1]
    return False

def getTimeStruct(s):
    if s == "NA":
        return 0
    try:
        timestruct = int(time.mktime(time.strptime(s)))
    except ValueError:
        timestruct = int(time.time())
    return timestruct

def strip_version(packageFile):
    if packageFile.rfind('-') == -1:
        return packageFile
    ending = packageFile[packageFile.rfind('-')+1:]
    validator = re.compile("([0-9]+)")
    if validator.search(ending):
        if validator.search(ending).groups()[0] == ending:
            packageFile = packageFile[:packageFile.rfind('-')]
    return packageFile

def strip_version_from_keys(progress_data):
    output = {}
    for key in progress_data.keys():
        output[strip_version(key)] = progress_data[key]
    return output

def determine_install_status(item, progress_data):
    # 1. Broken installation
    # 2. Installed, not uninstalled.
    # 3. Broken uninstallation
    # 4. ok uninstallation
    if progress_data[item].get("INSTALLED") == None:
        progress_data[item]["INSTALLED"] = ''
    if progress_data[item].get("UNINSTALLED") == None:
        progress_data[item]["UNINSTALLED"] = ''
    iTxt = progress_data[item]["INSTALLED"]
    uTxt = progress_data[item]["UNINSTALLED"]
    if iTxt == "BROKEN":
        return BROKEN_INSTALL, None
    if uTxt == "BROKEN":
        return BROKEN_UNINSTALL, None
    if iTxt == "NA":
        return UNINSTALLED, None
    iInt = getTimeStruct(iTxt)
    if uTxt != "NA":
        uInt = getTimeStruct(uTxt)
        if uInt > iInt:
            return UNINSTALLED, uInt
    return INSTALLED, iInt

def get_installed_uninstalled_times(progress_data):
    output = {"installed":[], "uninstalled":[], 
              "broken_installed":[], "broken_uninstalled":[]}
    for item in progress_data.keys():
        status, last_action = determine_install_status(item, progress_data)
        if status == INSTALLED:
            output["installed"].append([item, last_action])
        elif status == UNINSTALLED:
            output["uninstalled"].append([item, last_action])
        elif status == BROKEN_INSTALL:
            output["broken_installed"].append([item, last_action])
        elif status == BROKEN_UNINSTALL:
            output["broken_uninstalled"].append([item, last_action])
    output["installed"].sort(datesort)
    output["uninstalled"].sort(datesort)
    return output

def rpartition(str, chr):
    chunks = str.split(chr)
    if len(chunks) == 1:
        return ('', '', str)
    first = chr.join(chunks[:-1])
    last = chunks[-1]
    return (first, chr, last)

def checkToRemove(basePackageName, actionTime, comparisonList):
    remove = False
    for fullPackageName2, actionTime2 in comparisonList:
        basePackageName2 = rpartition(fullPackageName2, '-')[0]
        if basePackageName == basePackageName2:
            if actionTime2 > actionTime:
                remove = True
    return remove

def strip_version_info(pkgInfo):
    output = {"installed":[], "uninstalled":[], 
              "broken_installed":[], "broken_uninstalled":[]}
    packageListNames = output.keys()
    for packageListName in packageListNames: # look at packageListName for duplicates in other listTypes
        for fullPackageName, actionTime in pkgInfo[packageListName]: # do other package lists have this basename?
            basePackageName = rpartition(fullPackageName, '-')[0]
            otherTypes = output.keys()
            otherTypes.remove(packageListName)
            for compareList in otherTypes:
                remove = checkToRemove(basePackageName, actionTime, pkgInfo[compareList])
                if remove == True:
                    break
            if not remove:
                output[packageListName].append([basePackageName, actionTime])
    return output


def getInstalled(progress_data):
    pkgInfo = get_installed_uninstalled_times(progress_data)
    pkgInfo = strip_version_info(pkgInfo)
    installedPackageNames = [packageName[0] for packageName in pkgInfo["installed"]]
    brokenPackageNames    = [packageName[0] for packageName in pkgInfo["broken_installed"]]
    brokenPackageNames   += [packageName[0] for packageName in pkgInfo["broken_uninstalled"]]
    return installedPackageNames, brokenPackageNames

def integrate(data, dictionary, overwrite):
    data["timestamp"] = time.time()
    if overwrite:
        for key, value in dictionary.iteritems():
            data[key] = value
    else:
        data = updateDict(dictionary, data)
    return data

def updateDict(newdict, olddict):
    for key, value in newdict.iteritems():
        if type(value) == type({}) and olddict.has_key(key):
            olddict[key] = updateDict(value, olddict[key])
        elif type(value) == type(["list"]) and olddict.has_key(key):
            olddict[key] = olddict[key] + value
        else:
            olddict[key] = value
    return olddict

def getTmpPath():
    alphabet = map(chr, range(97, 123))
    tmpFn    = "tmp"
    tmpFn   += random.choice(alphabet)
    tmpFn   += random.choice(alphabet)
    tmpFn   += random.choice(alphabet)
    tmpFn   += random.choice(alphabet)
    tmpFn   += random.choice(alphabet)
    tmpPath  = os.path.join(getSpkgPath(), tmpFn)
    return tmpPath

def standAloneMode(filesystem):
    configPath = os.path.join(getSpkgPath(), CONFIG_FILE)
    if filesystem.isfile(configPath):
        return True
    return False


# TESTED
def netStringToNetLong(netString):
    quadIndex = 3
    netLong = 0L
    for octet in netString.split('.'):
        netLong += (long(octet) << (quadIndex * 8))
        quadIndex -= 1
    return netLong

def netLongToNetString(netLong):
    netStrings = []
    while netLong:
        netString = str( netLong & 255 )
        netStrings.append( netString )
        netLong = netLong >> 8
    netStrings.reverse()
    return '.'.join(netStrings)
    
# TESTED
def computeNetLongFromStrings(netStringAddress, netStringMask):
    if type(netStringAddress) == type("string"):
        netLongAddress = netStringToNetLong(netStringAddress)
    else:
        netLongAddress = netStringAddress
    if type(netStringMask) == type("string"):
        netLongMask = netStringToNetLong(netStringMask)
    else:
        netLongMask = netStringMask
    netLong = netLongAddress & netLongMask
    return netLong

# TESTED
def findBitMaskFromNetString(netString):
    bits = 0
    octets = netString.split('.')
    if len(octets) != 4: return 0
    for octet in octets:
        if int(octet) < 0 or int(octet) > 255:
            return 0
        for j in range(0,8):
            if int(octet) == 256-(2**j):
                bits += (8-j)
                break
    return bits

# TESTED
def netLongFromBitmask(bits):
    if bits == 0: return 0L
    netLong = 0L
    for i in range(0,31):
        if bits:
            netLong += 1
            bits -= 1
        netLong = netLong << 1
    return netLong

# TESTED
def convertCidrToNetLongs(netStringCidr):
    try:
        netString  = netStringCidr.split('/')[0]
        maskBits   = netStringCidr.split('/')[1]
    except IndexError:
        print "BAD DATA: %s" % netStringCidr
        print netStringCidr.split('/')
        return netLongFromBitmask(32)
    netLongMask = netLongFromBitmask(int(maskBits))
    netLong = computeNetLongFromStrings(netString, netLongMask)
    return netLong, netLongMask


# TESTED
def getNetworkList(networkDict):
    addressSet = networkDict["address"]
    netLongs = []
    for networkString in addressSet:
        netLong, netLongMask = convertCidrToNetLongs(networkString)
        netLongs.append(netLong)
    return netLongs

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
        bits = findBitMaskFromNetString(snms[i])
        addressSet.update(["%s/%d" % (addresses[i], bits)])
    return addressSet

def getMatchStringList( patternStr, fileName ):
    import Filesystem
    filesystem = Filesystem.Filesystem()
    pat    = re.compile( patternStr )
    lines  = filesystem.getAllFromFile(patternStr, fileName)
    retSet = set([])
    if not lines:
        return retSet
    for line in lines:
        m = re.match( pat, line )
        if m:
            retSet.update( m.groups() )
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

def getLinuxConfig():
    data = open("/etc/bombardier.yml", 'r').read()
    config = yaml.load(data)
    return config

def putLinuxConfig(config):
    data = open("/etc/bombardier.yml", 'w')
    data.write(yaml.dump(config))

def getSpkgPath():
    spkgPath = ''
    if sys.platform == "linux2":
        config = getLinuxConfig()
        spkgPath = config.get("spkgPath")
    else:
        import _winreg as winreg
        keyName = r"Software\GE-IT\Bombardier"
        try:
            key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                                 keyName, 0, winreg.KEY_QUERY_VALUE)
            spkgPath, dummy = winreg.QueryValueEx(key, "InstallPath")
        except:
            spkgPath = r"C:\spkg"
    return spkgPath

def getPythonPath():
    return os.path.join(sys.prefix, "python.exe")

# NOT WORTH TESTING
def getPackagePath(instanceName):
    return os.path.join(getSpkgPath(), instanceName, PACKAGES)

# TESTED
def getProgressPath(instanceName):
    newPath = os.path.join(getSpkgPath(), instanceName, STATUS_FILE)
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

def consoleFail( errorString="Failed, error unknown" ):
    print errorString
    consoleSync( FAIL )
    sys.exit(FAIL)

