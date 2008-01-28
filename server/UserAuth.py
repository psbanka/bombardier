#!/opt/python2.5/bin/python
from commonUtil import *
import yaml, random, time
from commands import getstatusoutput
from bcs import BombardierRemoteClient, EXECUTE
import pexpect

def showAllRights():
    userData = yaml.load(open("deploy/include/systemInfo.yml").read())
    print yaml.dump(userData["system"]["rights"], default_flow_style=False)

def getPwd():
    lowercase = [ chr(x) for x in range(ord('a'),ord('z')) ]
    uppercase = [ chr(x) for x in range(ord('A'),ord('Z')) ]
    symbols   = [ chr(x) for x in range(ord('!'),ord('/')) ]
    symbols.remove('"')
    symbols.remove("'")
    numbers   = [ chr(x) for x in range(ord('0'),ord('9')) ]
    password  = random.sample(lowercase, 5) + random.sample(uppercase, 2) + \
                random.sample(symbols, 1) + random.sample(numbers, 2)
    random.shuffle(password)
    return ''.join(password)

class BadRightException(Exception):
    def __init__(self, right):
        e = Exception()
        Exception.__init__(e)
        self.right = right
    def __repr__(self):
        return "Could not grant right %s, because it does not exist on the server" % self.right

class UserAuth:

    def __init__(self, name, rightsList, comment, systemInfoFile):
        self.systemInfoFile = systemInfoFile
        self.name = name
        self.rightsList = rightsList
        self.comment = comment
        self.hostPass = getPwd()
        self.vpnPass  = getPwd()
        self.safeCombo = getPwd()
        self.hostAccess = {}

        config = yaml.load(open("serverConfig.yml").read())
        self.environment = config["environment"]
        self.webDir = config["webDir"]
        self.webUser = config["webUser"]
        self.certDir = config["certDir"]
        self.password = libUi.pwdInput('Input root password: ')

    def buildHostAccess(self, rightsDict):
        for host in rightsDict:
            if not host in self.hostAccess:
                self.hostAccess[host] = set()
            self.hostAccess[host] = self.hostAccess[host].union(set(rightsDict[host]))

    def modifySystemInfo(self):
        systemInfo = yaml.load(open(self.systemInfoFile).read())
        systemRights = systemInfo["system"]["rights"]

        if self.name.lower() in [ name.lower() for name in systemInfo["system"]["users"]]:
            #print "modifying rights..."
            return FAIL
            
        userDict = {"password": '', "rights": [], "comment": ''}
        for right in self.rightsList:
            if right not in systemRights:
                return FAIL
            userDict["rights"].append(right)
            self.buildHostAccess(systemRights[right])
        userDict["password"] = self.hostPass
        userDict["comment"]  = self.comment
        systemInfo["system"]["users"][self.name] = userDict
        open(self.systemInfoFile, 'w').write(yaml.dump(systemInfo))
        return OK

    def createVpnCert(self):
        start = os.getcwd()
        os.chdir(self.certDir)
        s = pexpect.spawn("bash mkpkg.sh %s" % self.name)
        s.expect("Enter PEM pass phrase:")
        s.sendline(self.vpnPass)
        s.expect("Verifying - Enter PEM pass phrase:")
        s.sendline(self.vpnPass)
        time.sleep(1)
        s.close()
        os.chdir(start)
        if os.path.isfile("%s/%s-%s-outside.zip" % (self.certDir, self.name, self.environment)):
            if os.path.isfile("%s/%s-%s-inside.zip" % (self.certDir, self.name, self.environment)):
                return OK
        return FAIL

    def createEntry(self, entry, password, notes):
        return "%s %s %s %s %s %s %s" % (self.name, self.safeCombo, entry, self.name, self.environment, password, notes)

    def createPwsafe(self):
        os.system('rm -f %s.dat' % self.name)
        inputFile = open("%s.pass" % self.name, 'w')
        inputFile.write(self.createEntry("VPN_certificate", self.vpnPass, "This will unlock your VPN certificate")+'\n')
        for host in self.hostAccess:
            notes = "rights: %s" % str(list(self.hostAccess[host]))
            ipAddress = yaml.load(open("deploy/client/%s.yml" % host).read())["ipAddress"]
            inputFile.write(self.createEntry("%s-%s" % (host, ipAddress), self.hostPass, notes)+'\n')
        inputFile.close()
        os.system("bash -c 'cat %s.pass | ./pw.sh' 2&> /dev/null" % self.name)
        #os.system("rm %s.pass %s.dat~" % (self.name, self.name))
        if os.path.isfile("%s.dat" % self.name):
            return OK
        return FAIL

    def prepareWebData(self):
        status1 = os.system('mv -f %s.dat %s' % (self.name, self.webDir))
        status2 = os.system('bash -c "mv -f %s/%s*.zip %s"' % (self.certDir, self.name, self.webDir))
        open("%s/%s.passwd" % (self.webDir, self.name), 'w').write(self.safeCombo)
        status3 = os.system('sudo bash -c "chown %s.%s %s/%s*"' % (self.webUser, self.webUser, self.webDir, self.name))
        status4 = os.system('sudo bash -c "chmod 660 %s/%s*"' % (self.webDir, self.name))
        if [status1, status2, status3, status4] == [OK, OK, OK, OK]:
            return OK

        return FAIL
    
    def bombardierUpdate(self):
        overallStatus = OK
        print "bombardierUpdate"
        for host in self.hostAccess:
            print "HOST:", host
            serverObject = BombardierRemoteClient(host, self.password)
            status1, output = serverObject.process(EXECUTE, ["HostAuthorization"], "addUsers", True)
            if "DbAuthorization" in serverObject.info["packages"]:
                status2, output = serverObject.process(EXECUTE, ["DbAuthorization"], "cleanUsers", True)
                status3, output = serverObject.process(EXECUTE, ["DbAuthorization"], "addUsers", True)
            else:
                status2, status3 = OK, OK
            if FAIL in [status1, status2, status3]:
                overallStatus = FAIL
        return overallStatus

if __name__ == "__main__":
    from libTest import *
    os.system("cp testSystemInfo.yml.bak testSystemInfo.yml")
    os.system('bash -c "rm -f CA/cgvpn/joe*"')
    os.system("rm -f joe.pass")
    os.system('bash -c "rm -f /D/www/pwsafe/joe*"')
    status = OK
    userAuth = UserAuth("joe", ["DB_READER"], "joe is a nice guy", "testSystemInfo.yml")
    startTest()
    assert not "joe" in open("testSystemInfo.yml").read()
    assert not os.path.isfile("CA/cgvpn/joe-salem-outside.zip")
    assert not os.path.isfile("/D/www/pwsafe/joe-salem-outside.zip")
    status = runTest(userAuth.modifySystemInfo, [], OK, status)
    status = runTest(userAuth.createVpnCert, [], OK, status)
    assert os.path.isfile("CA/cgvpn/joe-salem-outside.zip")
    status = runTest(userAuth.createPwsafe, [], OK, status)
    status = runTest(userAuth.prepareWebData, [], OK, status)
    assert os.path.isfile("/D/www/pwsafe/joe-salem-outside.zip")
    assert userAuth.hostAccess['lildb'] == set(['casDB', 'uhrDB', 'rmsDB'])
    assert "joe" in open("testSystemInfo.yml").read()
    userAuth = UserAuth("joe", ["DB-READER"], "joe is a nice guy", "testSystemInfo.yml")
    status = runTest(userAuth.modifySystemInfo, [], FAIL, status)
    endTest(status)
