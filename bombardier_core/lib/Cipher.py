#!/usr/bin/env python
"Used to encrypt configuration data"

import base64, binascii
from Crypto.Cipher import AES
from static_data import CENSORED

VALID_CHARS = [ chr(x) for x in range(ord(' '), ord('~')+1) ] + ['\n']

class InvalidData(Exception):
    "Sometimes encrypted data can be corrupt or password is wrong"
    def __init__(self, dictionary, key, explanation = ''):
        """
        dictionary -- dictionary that we're trying to decrypt
        key -- pointer within dictionary
        explanation -- description of problem
        """
        Exception.__init__(self)
        self.key = key
        self.dictionary = dictionary
        if not explanation:
            explanation = "Invalid data"
        self.explanation = explanation
    def __str__(self):
        output = "%s: [key: (%s), value: (%s)]"
        output = output % (self.explanation, self.key, self.dictionary)
        return output
    def __repr__(self):
        return self.__str__()

class DecryptionException(Exception):
    "Lower-level error"
    def __init__(self, base64_text, reason):
        Exception.__init__(self)
        self.base64_text = base64_text
        self.reason = reason
    def __str__(self):
        return "Could not decrypt %s: %s" % (self.base64_text, self.reason)
    def __repr__(self):
        return self.__str__()

def pad(password):
    "Passwords have to be the appropriate length"
    output = password + '=' * (16 - len(password) % 16)
    return output

def get_invalid_characters(suspect_str, valid_chars):
    "determine which characters passed in are invalid"
    invalid_chars = []
    for char in suspect_str:
        if char not in valid_chars:
            invalid_chars.append( char )
    return invalid_chars

def _encrypt_string(plain_str, password):
    "encrypt a string with a password"
    encrypter  = AES.new(password, AES.MODE_ECB)
    base_64_str = pad(base64.encodestring(plain_str).strip())
    cipher_text = encrypter.encrypt(base_64_str)
    base64_cipher_str = base64.encodestring(cipher_text).strip()
    assert cipher_text == base64.decodestring(base64_cipher_str)
    return base64_cipher_str

def _decrypt_string(base64_cipher_str, password, valid_chars=VALID_CHARS):
    """Decrypt some base-64 encoded cypher-text with a password, checking 
    that the characters that come out are valid"""
    #print "base64_cipher_str: (%s)"%base64_cipher_str
    try:
        cipher_b64_str = base64.decodestring(base64_cipher_str)
        #print "cipher_b64_str: (%s)" %cipher_b64_str
        decrypter = AES.new(password, AES.MODE_ECB)
        base_64_str = decrypter.decrypt(cipher_b64_str)
        #print "base_64_str: ", base_64_str
        base = base_64_str.split('=')[0]
        base += "=" * (len(base) % 4)
        #print "base: ", base
        plain_str = base64.decodestring(base_64_str)
        #print "plain_str: ", plain_str
    except binascii.Error:
        msg = "Invalid characters in the decrypted text 0"
        raise DecryptionException(base64_cipher_str, msg)
    except ValueError:
        msg = "Invalid characters in the decrypted text 1"
        raise DecryptionException(base64_cipher_str, msg)
    invalid_chars = get_invalid_characters( plain_str, valid_chars )
    if invalid_chars:
        reason = "Invalid characters in the decrypted text: %s" % invalid_chars
        raise DecryptionException(base64_cipher_str, reason)
    return plain_str

class Cipher:
    "Class to provide simplified encryption services"

    def __init__(self, password):
        if password:
            password = pad(password)
        self.password = password

    def decrypt_string(self, string):
        return _decrypt_string(string, self.password)

    def encrypt_string(self, string):
        return _encrypt_string(string, self.password)

    def decrypt_dict(self, dictionary):
        "pad the password and decrypt the dictionary"
        self.decrypter_loop(dictionary)
        return dictionary

    def decrypter_loop(self, dictionary):
        """Recursive call to decrypt all values in a dictionary if password is
        given. If the password is not given, all encrypted values will be marked
        as censored."""
        if type(dictionary) != type({}):
            return
        for key in dictionary:
            value_type = type(dictionary[key])
            if value_type == type('') and type(key) == type('') \
              and key.startswith('enc_'):
                new_key = key.split("enc_")[-1]
                if self.password:
                    dictionary[new_key] = _decrypt_string(dictionary[key],
                                                         self.password)
                else:
                    dictionary[new_key] = CENSORED
                del dictionary[key]
            elif value_type == type({}):
                try:
                    self.decrypter_loop(dictionary[key])
                except DecryptionException:
                    raise InvalidData(dictionary[key], key, "Unable to decrypt")

