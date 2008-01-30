#!/opt/python2.5/bin/python
import bombardier.miniUtility as miniUtility
import os
import yaml
import getpass
import base64
import libCipher
from Crypto.Cipher import AES
from bombardier.staticData import OK, FAIL, CENSORED

class Client:

    def __init__(self, systemName, passwd):
        self.data       = {}
        self.includes   = []
        self.systemName = systemName
        if passwd:
            self.passwd = libCipher.pad(passwd)
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
        if newData == None:
            newData = {}
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
        return len(self.checkCryptDict(self.data))

    def getEncryptedEntries(self):
        return self.checkCryptDict(self.data, '')

    def checkCryptDict(self, dict, currentPath):
        encryptedEntries = {}
        for key in dict:
            myCurrentPath = "%s/%s" % (currentPath, key)
            t = type(dict[key])
            if t == type('') and type(key) == type('') and key.startswith('enc_'):
                encryptedEntries[myCurrentPath] = key
            elif t == type({}):
                newDict = self.checkCryptDict(dict[key], myCurrentPath)
                encryptedEntries = miniUtility.addDictionaries(encryptedEntries, newDict)
        return encryptedEntries

    def decryptConfig(self):
        libCipher.decryptLoop(self.data, self.passwd)

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
    config.decryptConfig()
    data   = yaml.dump(config.data)
    if options.output:
        print "output to filename: %s" % options.output
        open(options.output, 'w').write(data)
    else:
        print data

