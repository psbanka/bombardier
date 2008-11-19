#!/opt/python2.5/bin/python
from commonUtil import *
import yaml, random, time, os
from commands import getstatusoutput
from bcs import BombardierRemoteClient, EXECUTE
import libCipher
import pexpect


def getPwd():
    lowercase = [ chr(c) for c in range(ord('a'),ord('z')) ]
    uppercase = [ chr(c) for c in range(ord('A'),ord('Z')) ]
    symbols   = [ chr(c) for c in range(ord('!'),ord('/')) ]
    symbols.remove('"')
    symbols.remove("'")
    numbers   = [ chr(c) for c in range(ord('0'),ord('9')) ]
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

    def __init__(self, name, rightsList, comment, systemInfoFile, serverInfoFile, autoConfirm, password=''):
        self.systemInfoFile  = systemInfoFile
        self.autoConfirm     = autoConfirm
        self.name            = name
        self.userName        = self.name.replace('.', ' ')
        self.rightsList      = [ right.upper() for right in rightsList ]
        self.comment         = comment
        self.hostPass        = getPwd()
        self.vpnPass         = getPwd()
        self.safeCombo       = getPwd()
        self.hostAccess      = {}
        self.modifiedSystems = set()

        config = yaml.load(open(serverInfoFile).read())
        self.environment = config["environment"]
        self.webDir = config["webDir"]
        self.webUser = config["webUser"]
        self.password = password
        if not self.password:
            self.password = config.get("password")
            if not self.password:
                self.password = libUi.pwdInput('Input root password: ')

    def buildHostAccess(self, rightsDict):
        for host in rightsDict:
            if not host in self.hostAccess:
                self.hostAccess[host] = set()
            self.hostAccess[host] = self.hostAccess[host].union(set(rightsDict[host]))

    def noteChangedSystems(self, rightsDict):
        for host in rightsDict:
            self.modifiedSystems = self.modifiedSystems.union([host])

    def modifySystemInfo(self):
        systemInfo      = yaml.load(open(mode.serverHome+'/'+self.systemInfoFile).read())
        systemRights    = systemInfo["system"]["rights"]
        foundUser       = False

        for userName in systemInfo["system"]["users"]:
            if self.userName.lower() == userName.lower():
                foundUser     = True
                userDict      = libCipher.decrypt(systemInfo["system"]["users"][userName], self.password)
                self.hostPass = userDict["password"]
                del userDict["password"]
                oldRightsList = userDict["rights"]
                delRightsList = list(set(oldRightsList) - set(self.rightsList))
                addRightsList = list(set(self.rightsList) - set(oldRightsList))
                userDict["rights"] = self.rightsList
                for right in list(set(delRightsList+addRightsList)):
                    if right not in systemRights.keys():
                        print "(%s) not in (%s)" % (right, systemRights.keys())
                        raise BadRightException(right)
                if not self.comment:
                    self.comment = userDict.get("comment")
                else:
                    userDict["comment"] = self.comment
                break
            else:
                pass
                #print "%s != %s" % (userName.lower(), self.userName.lower())
        if not foundUser:
            print "==> User not found in current configuration"
            addRightsList   = self.rightsList
            delRightsList   = []
            userDict        = {"rights": self.rightsList, "comment": self.comment}
            
        if delRightsList:
            print "Removing rights: %s" % delRightsList
            if self.autoConfirm or libUi.askYesNo("Are you sure"):
                for right in delRightsList:
                    self.noteChangedSystems(systemRights[right])
            else:
                print "Not removing any rights..."

        if addRightsList:
            print "Adding rights: %s" % addRightsList
            for right in addRightsList:
                self.buildHostAccess(systemRights[right])
                self.noteChangedSystems(systemRights[right])

        if addRightsList or delRightsList:
            userDict["enc_password"] = libCipher.encrypt(self.hostPass, self.password)
            systemInfo["system"]["users"][self.userName] = userDict
            open(self.systemInfoFile, 'w').write(yaml.dump(systemInfo))
            status2 = os.system("chgrp %s %s" % (mode.defaultGroup, self.systemInfoFile))
            status3 = os.system("chmod 660 %s" % (self.systemInfoFile))
        return OK

    def verifyVpnCert(self):
        if not os.path.isdir("archive/tmp"):
            os.makedirs("archive/tmp")
        os.chdir('archive/tmp')
        status, output = getstatusoutput("unzip ../%s*-inside.zip" % self.name)
        if status != OK:
            print "Error unpacking key file"
            return FAIL
        s = pexpect.spawn("openssl rsa -in %s.key -noout -text" % (self.name))
        s.expect("Enter pass phrase")
        s.sendline(self.vpnPass)
        output = s.read()
        s.close()
        os.chdir("../..")
        os.system("rm -rf archive/tmp")
        if "modulus" in output:
            print "VPN key passes openssl validation"
            return OK
        print "KEY IS NOT OK: %s" % output
        return FAIL

    def createVpnCert(self):
        start = os.getcwd()
        certsPath = os.path.join(mode.serverHome, "include", "OpenVpnCerts.yml")
        caData = yaml.load(open(certsPath).read())
        libCipher.decryptLoop(caData, self.password)
        os.chdir("CA")
        open("%s-CA.crt" % self.environment, "w").write(caData["openvpn"]["CA_CRT"])
        open("%s-CA.key" % self.environment, "w").write(caData["openvpn"]["CA_KEY"])
        os.system("chgrp %s %s" % (mode.defaultGroup, "%s-CA.crt" % self.environment))
        os.system("chmod 660 %s" % ("%s-CA.crt" % self.environment))
        os.system("chgrp %s %s" % (mode.defaultGroup, "%s-CA.key" % self.environment))
        os.system("chmod 660 %s" % ("%s-CA.key" % self.environment))
        s = pexpect.spawn("bash mkpkg.sh %s %s" % (self.name, self.environment))
        s.expect("Enter PEM pass phrase:")
        s.sendline(self.vpnPass)
        s.expect("Verifying - Enter PEM pass phrase:")
        s.sendline(self.vpnPass)
        time.sleep(1)
        s.close()
        os.system("shred -fu %s-CA.*" % self.environment)
        status = self.verifyVpnCert()
        os.chdir(start)
        return status

    def createEntry(self, entry, password, notes):
        return "%s %s %s %s %s %s %s" % (self.name, self.safeCombo, entry, self.name, self.environment, password, notes)

    def createPwsafe(self):
        getstatusoutput('rm -f %s.dat' % self.name)
        inputFile = open("%s.pass" % self.name, 'w')
        status2 = os.system("chgrp %s %s" % (mode.defaultGroup, "%s.pass" % self.name))
        status3 = os.system("chmod 660 %s" % ("%s.pass" % self.name))
        inputFile.write(self.createEntry("VPN_certificate", self.vpnPass, "This will unlock your VPN certificate")+'\n')
        for host in self.hostAccess:
            notes = "rights: %s" % str(list(self.hostAccess[host]))
            hostPath = os.path.join(mode.serverHome, "client", "%s.yml" % host)
            ipAddress = yaml.load(open(hostPath).read())["ipAddress"]
            inputFile.write(self.createEntry("%s-%s" % (host, ipAddress), self.hostPass, notes)+'\n')
        inputFile.close()
        getstatusoutput("bash -c 'cat %s.pass | ./pw.sh' 2&> /dev/null" % self.name)
        getstatusoutput("rm %s.pass %s.dat~" % (self.name, self.name))
        if os.path.isfile("%s.dat" % self.name):
            return OK
        return FAIL

    def removeCredentials(self):
        #^ FIXME: INVALIDATE CERTS
        status, output = getstatusoutput('bash -c "rm -f %s/%s*"' % (self.webDir, self.name))
        return status

    def prepareWebData(self):
        status1, output = getstatusoutput('mv %s.dat %s' % (self.name, self.webDir))
        status2, output = getstatusoutput('bash -c "cp CA/archive/%s*.zip %s"' % (self.name, self.webDir))
        open("%s/%s.passwd" % (self.webDir, self.name), 'w').write(self.safeCombo)
        status2 = os.system("chgrp %s %s" % (mode.defaultGroup, "%s/%s.passwd" % (self.webDir, self.name)))
        status3 = os.system("chmod 660 %s" % ("%s/%s.passwd" % (self.webDir, self.name)))
        status3, output = getstatusoutput('sudo bash -c "chown %s.%s %s/%s*"' % (self.webUser, self.webUser, self.webDir, self.name))
        status4, output = getstatusoutput('sudo bash -c "chmod 600 %s/%s*"' % (self.webDir, self.name))
        if [status1, status2, status3, status4] == [OK, OK, OK, OK]:
            return OK

        return FAIL

    def bombardierUpdate(self):
        overallStatus = OK
        for host in self.modifiedSystems:
            print "HOST:", host
            serverObject = BombardierRemoteClient(host, self.password, mode.serverHome, mode.termwidth)
            status1, output = serverObject.process(EXECUTE, ["HostAuthorization"], "setUsers", True)
            if "DbAuthorization" in serverObject.info["packages"]:
                status2, output = serverObject.process(EXECUTE, ["DbAuthorization"], "setUsers", True)
            else:
                status2 = OK
            if FAIL in [status1, status2]:
                overallStatus = FAIL
        return overallStatus

if __name__ == "__main__":

    def getData():
        return yaml.load(open("testSystemInfo.yml").read())

    from libTest import startTest, runTest, endTest
    getstatusoutput("cp testSystemInfo.yml.bak testSystemInfo.yml")
    getstatusoutput('bash -c "rm -f CA/salem/joe*"')
    getstatusoutput("rm -f joe.pass")
    getstatusoutput('bash -c "rm -f /D/www/pwsafe/joe*"')
    status = OK
    userAuth = UserAuth("joe", ["DB_READER"], "joe is a nice guy", "testSystemInfo.yml", True, 'abc')
    startTest()
    assert not "joe" in open("testSystemInfo.yml").read()
    assert not os.path.isfile("CA/salem/joe-salem-outside.zip")
    assert not os.path.isfile("/D/www/pwsafe/joe-salem-outside.zip")
    status = runTest(userAuth.modifySystemInfo, [], OK, status)
    userAuth = UserAuth("joe", ["SVCNET"], "joe is a nice guy", "testSystemInfo.yml", True, 'abc')
    status = runTest(userAuth.modifySystemInfo, [], OK, status)
    data = getData()
    assert data["system"]["users"]["joe"]["rights"] == ["SVCNET"]
    #status = runTest(userAuth.createVpnCert, [], OK, status)
    #assert os.path.isfile("CA/salem/joe-salem-outside.zip")
    #status = runTest(userAuth.createPwsafe, [], OK, status)
    #status = runTest(userAuth.prepareWebData, [], OK, status)
    #assert os.path.isfile("/D/www/pwsafe/joe-salem-outside.zip")
    #assert userAuth.newHostAccess['lildb'] == set(['casDB', 'uhrDB', 'rmsDB'])
    #assert "joe" in open("testSystemInfo.yml").read()
    #userAuth = UserAuth("joe", ["DB_ADMIN"], "joe is a nice guy", "testSystemInfo.yml", True, 'abc')
    #status = runTest(userAuth.modifySystemInfo, [], OK, status)
    endTest(status)
