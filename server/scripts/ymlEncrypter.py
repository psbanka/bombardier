import libCipher
import yaml
import sys
import getpass

fileName = sys.argv[1]
libCipher.VALID_CHARS.append('\n')

def encDict(dictionary, passwd):
    for key in dictionary:
        if key.startswith('enc'):
            continue
        value = dictionary[key]
        if type(value) == type({}):
            dictionary[key] = encDict(dictionary[key], passwd)
        elif type(value) == type("string"):
            cipherText = libCipher.encrypt(value, passwd)
            del dictionary[key]
            dictionary['enc_%s' % key] = cipherText
    return dictionary

passwd = getpass.getpass("need the password to use:")
dataDict = yaml.load(open(fileName).read())
dataDict = encDict(dataDict, passwd)
encFilename = "%s.enc" % fileName
open(encFilename, 'w').write(yaml.dump(dataDict))

a = yaml.load(open(encFilename).read())
libCipher.decryptLoop(a, passwd)
print "Round-trip data:", a

