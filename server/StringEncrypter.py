import base64, sys
from Crypto.Cipher import AES
import getpass
import libCipher

passwd = libCipher.pad(getpass.getpass("Enter the encryption password: "))
passwdCheck = libCipher.pad(getpass.getpass("Enter the encryption password (again): "))
if passwd != passwdCheck:
    print "you screwed up."
    sys.exit(1)
#print "len(passwd):", len(passwd)
plainStr = getpass.getpass("Enter the data you wish to encrypt: ")
cipherText = libCipher.encrypt(plainStr, passwd)
print "Cipher Text: (%s)" % cipherText

