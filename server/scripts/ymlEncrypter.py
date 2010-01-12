#!/usr/bin/python
import bombardier_core.Cipher
import yaml
import sys
import getpass

filename = sys.argv[1]
bombardier_core.Cipher.VALID_CHARS.append('\n')

def enc_dict(dictionary, cipher):
    for key in dictionary:
        if key.startswith('enc'):
            continue
        value = dictionary[key]
        if type(value) == type({}):
            dictionary[key] = enc_dict(dictionary[key], password)
        elif type(value) == type("string"):
            cipher_text = cipher.encrypt(value, password)
            del dictionary[key]
            dictionary['enc_%s' % key] = cipher_text
    return dictionary

password = getpass.getpass("need the password to use:")
cipher = bombardier_core.Cipher.Cipher(password)
data_dict = yaml.load(open(filename).read())
data_dict = enc_dict(data_dict, cipher)
enc_filename = "%s.enc" % filename
open(enc_filename, 'w').write(yaml.dump(data_dict))

a = yaml.load(open(enc_filename).read())
libCipher.decryptLoop(a, password)
print "Round-trip data:", a

