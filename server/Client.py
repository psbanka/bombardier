#!/opt/python2.5/bin/python
import bombardier.miniUtility as miniUtility
import os
import yaml
import getpass
import base64
from Crypto.Cipher import AES
from bombardier.staticData import OK, FAIL, CENSORED

VALID_CHARS = [ chr(x) for x in range(ord(' '), ord('~')+1) ]

def pad(str):
    return str + '=' * (16 - len(str) % 16)

def isValidString(str):
    for c in str:
        if c not in VALID_CHARS:
            return False
    return True

class Client:

    def __init__(self, systemName, passwd):
        self.data       = {}
        self.includes   = []
        self.systemName = systemName
        if passwd:
            self.passwd = pad(passwd)
        else:
            self.passwd = ''

    def __getitem__(self, key):
        return self.data[key]

    def findIncludeList(data):
        includeList = []
        for key in data.keys():
            if key.upper() == "INCLUDE":
                includeList += data[key]
        return includeList

    findIncludeList = staticmethod(findIncludeList)

    def get(self):
        if self.downloadClient() == OK:
            return self.convertBoms()
        return FAIL

    def loadIncludes(self, newIncludes):
        for includeName in newIncludes:
            if includeName not in self.includes:
                self.includes.append(includeName)
                self.downloadClient(includeName)

    def downloadClient(self, configName=''):
        if configName == '':
            ymlDirectory = "client"
            configName = self.systemName
        else:
            ymlDirectory = "include"
        filename   = "deploy/%s/%s.yml" % (ymlDirectory, configName)
        newData   = yaml.load( open(filename, 'r').read() )
        self.data = miniUtility.addDictionaries(self.data, newData)
        newIncludes = self.findIncludeList(newData)
        self.loadIncludes(newIncludes)
        return OK

    def convertBoms(self):
        boms = self.data.get("bom", [])
        packages = set(self.data.get("packages", []))
        for bom in boms:
            filename = "deploy/bom/%s.yml" % (bom)
            if not os.path.isfile(filename):
                print "%s does not exist" % filename
                return FAIL
            packages = packages.union(set(yaml.load(open(filename).read())))
        self.data["packages"] = list(packages)
        if self.data.get("bom"):
            del self.data["bom"]
        return OK

    def loadIncludes(self, newIncludes):
        for includeName in newIncludes:
            if includeName not in self.includes:
                self.includes.append(includeName)
                self.downloadClient(includeName)

    def checkEncryption(self):
        return self.checkCryptDict(self.data)

    def checkCryptDict(self, dict):
        encryptedEntries = 0
        for key in dict:
            t = type(dict[key])
            if t == type('') and type(key) == type('') and key.startswith('enc_'):
                encryptedEntries += 1
            elif t == type({}):
                encryptedEntries += self.checkCryptDict(dict[key])
        return encryptedEntries

    def decryptConfig(self):
        self.decryptDict(self.data)

    def decryptDict(self, dict):
        for key in dict:
            t = type(dict[key])
            if t == type('') and type(key) == type('') and key.startswith('enc_'):
                newKey = key.split("enc_")[-1]
                if self.passwd:
                    dict[newKey] = self.decryptString(dict[key])
                else:
                    dict[newKey] = CENSORED
                del dict[key]
            elif t == type({}):
                self.decryptDict(dict[key])
        
    def decryptString(self, b64CipherB64Str):
        #print "b64CipherB64Str: (%s)"%b64CipherB64Str
        cipherB64Str = base64.decodestring(b64CipherB64Str).strip()
        #print "cipherB64Str: (%s)" %cipherB64Str
        decrypter = AES.new(self.passwd, AES.MODE_ECB)
        b64Str = decrypter.decrypt(cipherB64Str)
        #print "b64Str: ", b64Str
        base = b64Str.split('=')[0]
        base += "=" * (len(base) % 4)
        #print "base: ", base
        plainStr = base64.decodestring(b64Str)
        #print "plainStr: ", plainStr
        if not isValidString(plainStr):
            raise DecryptionException(b64CipherB64Str, "Invalid characters in the decrypted text")
        return plainStr

class DecryptionException(Exception):
    def __init__(self, b64Text, reason):
        e = Exception()
        Exception.__init__(e)
        self.key = b64Text
        self.reason = reason
    def __str__(self):
        return "Could not decrypt %s: %s" % (self.b64Text, self.reason)
    
if __name__ == "__main__":
    import sys
    import optparse
    parser = optparse.OptionParser("usage: %prog server-name [options]")
    parser.add_option("-k", "--insecure", dest="insecure",
                      action="store_true", default=False,
                      help="don't ask for a password")
    parser.add_option("-o", "--output", dest="output", metavar="FILENAME",
                      help="designate on output file")

    (options, args) = parser.parse_args()
    if len(args) != 1:
        parser.print_help()
        sys.exit(1)
    client = args[0]
    passwd = ''
    if not options.insecure:
        passwd = getpass.getpass("Enter decryption password: ")
    config = Client(client, passwd)
    status = config.get()
    if status == FAIL:
        print "Bad config file."
        sys.exit(1)
    client.decryptConfig()
    data   = yaml.dump(config.data)
    if options.output:
        print "output to filename: %s" % options.output
        open(options.output, 'w').write(data)
    else:
        print data

