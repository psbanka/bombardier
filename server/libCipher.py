#!/opt/python2.5/bin/python

import base64, binascii
from Crypto.Cipher import AES
from bombardier.staticData import CENSORED
from staticData import *

VALID_CHARS = [ chr(x) for x in range(ord(' '), ord('~')+1) ]

def pad(str):
    return str + '=' * (16 - len(str) % 16)

def getInvalidChars(str, validChars):
    invalidChars = []
    for c in str:
        if c not in validChars:
            invalidChars.append( c )
    return invalidChars

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
            try:
                decryptLoop(dict[key], passwd)
            except DecryptionException, e:
                pyChucker(e)
                raise InvalidData(key, dict[key], "Unable to decrypt")
    
def decryptString(b64CipherB64Str, passwd, validChars=VALID_CHARS):
    #print "b64CipherB64Str: (%s)"%b64CipherB64Str
    if len(passwd) % 16:
        passwd = pad(passwd)
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
    invalidChars = getInvalidChars( plainStr, validChars )
    if invalidChars:
        print "Invalid characters in the decrypted text: %s" %invalidChars
        raise DecryptionException(b64CipherB64Str, "HOLYCRAP")
    return plainStr

class InvalidData(Exception):
    def __init__(self, key, dictionary, explanation = ''):
        e= Exception()
        Exception.__init__(e)
        self.key = key
        self.dictionary = dictionary
        if not explanation:
            self.explanation = "Invalid data"
        else:
            self.explanation = explanation
    def __str__(self):
        return "%s: [key: (%s), value: (%s)]" % (self.explanation, self.key, self.dictionary)
    def __repr__(self):
        return "%s: [key: (%s), value: (%s)]" % (self.explanation, self.key, self.dictionary)

class DecryptionException(Exception):
    def __init__(self, b64Text, reason):
        e = Exception()
        Exception.__init__(e)
        self.b64Text = b64Text
        self.reason = reason
    def __str__(self):
        return "Could not decrypt %s: %s" % (self.b64Text, self.reason)
    def __repr__(self):
        return "Could not decrypt %s: %s" % (self.b64Text, self.reason)

if __name__ == "__main__":
    from libTest import startTest, endTest
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

