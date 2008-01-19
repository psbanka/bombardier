#!/opt/python2.5/bin/python
import base64, binascii
from Crypto.Cipher import AES
from bombardier.staticData import CENSORED

VALID_CHARS = [ chr(x) for x in range(ord(' '), ord('~')+1) ]

def pad(str):
    return str + '=' * (16 - len(str) % 16)

def isValidString(str):
    for c in str:
        if c not in VALID_CHARS:
            return False
    return True

def encrypt(plainStr, passwd):
    passwd = pad(passwd)
    encrypter  = AES.new(passwd, AES.MODE_ECB)
    b64Str     = pad(base64.encodestring(plainStr).strip())
    cipherText = encrypter.encrypt(b64Str)
    b64CipherB64Str = base64.encodestring(cipherText).strip()
    return b64CipherB64Str

def decrypt(dict, passwd):
    if passwd:
        passwd = pad(passwd)
    decryptLoop(dict, passwd)
    return dict

def decryptLoop(dict, passwd):
    if type(dict) != type({}):
        return 
    for key in dict:
        t = type(dict[key])
        if t == type('') and type(key) == type('') and key.startswith('enc_'):
            newKey = key.split("enc_")[-1]
            if passwd:
                dict[newKey] = decryptString(dict[key], passwd)
            else:
                dict[newKey] = CENSORED
            del dict[key]
        elif t == type({}):
            decryptLoop(dict[key], passwd)
    
def decryptString(b64CipherB64Str, passwd):
    #print "b64CipherB64Str: (%s)"%b64CipherB64Str
    try:
        cipherB64Str = base64.decodestring(b64CipherB64Str).strip()
        #print "cipherB64Str: (%s)" %cipherB64Str
        decrypter = AES.new(passwd, AES.MODE_ECB)
        b64Str = decrypter.decrypt(cipherB64Str)
        #print "b64Str: ", b64Str
        base = b64Str.split('=')[0]
        base += "=" * (len(base) % 4)
        #print "base: ", base
        plainStr = base64.decodestring(b64Str)
        #print "plainStr: ", plainStr
    except binascii.Error:
        raise DecryptionException(b64CipherB64Str, "Invalid characters in the decrypted text 0")
    except ValueError:
        raise DecryptionException(b64CipherB64Str, "Invalid characters in the decrypted text 1")
    if not isValidString(plainStr):
        raise DecryptionException(b64CipherB64Str, "Invalid characters in the decrypted text 2")
    return plainStr

class DecryptionException(Exception):
    def __init__(self, b64Text, reason):
        e = Exception()
        Exception.__init__(e)
        self.b64Text = b64Text
        self.reason = reason
    def __str__(self):
        return "Could not decrypt %s: %s" % (self.key, self.reason)
    def __repr__(self):
        return "Could not decrypt %s: %s" % (self.key, self.reason)

if __name__ == "__main__":
    from libTest import *
    data = {"enc_thing1": "Ujlr68WRWt5tkfR/a0sE9g==", "thing2": "hello"}
    passwd = "abcd1234"
    newData = decrypt(data, passwd)
    status = OK
    startTest()
    assert newData["thing1"] == "hello"
    assert newData["thing2"] == "hello" 
    value = encrypt("hello", "abcd1234")
    assert value == "Ujlr68WRWt5tkfR/a0sE9g=="
    endTest(status)

