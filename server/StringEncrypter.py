import base64, sys
from Crypto.Cipher import AES
import getpass

def encryptString():
#    VALID_CHARS = [ chr(x) for x in range(ord(' '), ord('~')+1) ]

    def pad(str):
        return str + '=' * (16 - len(str) % 16)

#    def isValidString(str):
#        for c in str:
#            if c not in VALID_CHARS:
#                return False
#        return True


    passwd = pad(getpass.getpass("Enter the encryption password: "))
    passwdCheck = pad(getpass.getpass("Enter the encryption password (again): "))
    if passwd != passwdCheck:
        print "you screwed up."
        sys.exit(1)
    #print "len(passwd):", len(passwd)

    plainStr = getpass.getpass("Enter the data you wish to encrypt: ")
    encrypter  = AES.new(passwd, AES.MODE_ECB)
    b64Str     = pad(base64.encodestring(plainStr).strip())
    #print "b64Str: ", b64Str
    cipherText = encrypter.encrypt(b64Str)
    #print "cipherText: ", cipherText
    b64CipherB64Str = base64.encodestring(cipherText).strip()
    print "Cipher Text: (%s)" % b64CipherB64Str

if __name__ == '__main__':
    encryptString()
