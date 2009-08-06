#!/opt/python2.5/bin/python
from bombardier_core.mini_utility import addDictionaries
import os
import yaml, syck
import getpass
from bombardier_core.libCipher import pad, decryptLoop
from Crypto.Cipher import AES
from bombardier_core.static_data import OK, FAIL, CENSORED
from Exceptions import MachineConfigurationException

class MachineConfig:

    def __init__(self, host_name, passwd, server_home):
        self.data       = {}
        self.includes   = []
        self.host_name = host_name
        self.server_home   = server_home
        if passwd:
            self.passwd = pad(passwd)
        else:
            self.passwd = ''

    def __getitem__(self, key):
        return self.data[key]

    def find_include_list(data):
        include_list = []
        for key in data.keys():
            if key.upper() == "INCLUDE":
                include_list += data[key]
        return include_list

    find_include_list = staticmethod(find_include_list)

    def merge(self):
        self.merge_includes()
        return self.convert_boms()

    def load_includes(self, new_includes):
        for include_name in new_includes:
            if include_name not in self.includes:
                self.includes.append(include_name)
                self.merge_includes(include_name)

    def merge_includes(self, config_name=''):
        if config_name == '':
            yml_directory = "machine"
            config_name = self.host_name
        else:
            yml_directory = "include"
        file_name = os.path.join(self.server_home, yml_directory, "%s.yml" % config_name)
        new_data = syck.load( open(file_name, 'r').read() )
        if new_data == None:
            new_data = {}
        self.data = addDictionaries(self.data, new_data)
        new_includes = self.find_include_list(new_data)
        self.load_includes(new_includes)

    def convert_boms(self):
        boms = self.data.get("bom", [])
        packages = set(self.data.get("packages", []))
        for bom in boms:
            file_name = os.path.join(self.server_home, "bom", "%s.yml" % bom)
            if not os.path.isfile(file_name):
                errmsg = "%s does not exist" % file_name
                raise MachineConfigurationException(self.host_name, errmsg)
            packages = packages.union(set(syck.load(open(file_name).read())))
        self.data["packages"] = list(packages)
        if self.data.get("bom"):
            del self.data["bom"]
        return OK

    def check_encryption(self):
        return len(self.check_crypto_dict(self.data, ''))

    def get_encrypted_entries(self):
        return self.check_crypto_dict(self.data, '')

    def check_crypto_dict(self, dict, current_path):
        encrypted_entries = {}
        for key in dict:
            my_current_path = "%s/%s" % (current_path, key)
            t = type(dict[key])
            if t == type('') and type(key) == type('') and key.startswith('enc_'):
                encrypted_entries[my_current_path] = key
            elif t == type({}):
                new_dict = self.check_crypto_dict(dict[key], my_current_path)
                encrypted_entries = addDictionaries(encrypted_entries, new_dict)
        return encrypted_entries

    def decrypt_config(self):
        decryptLoop(self.data, self.passwd)

if __name__ == "__main__":
    import sys
    import optparse
    parser = optparse.OptionParser("usage: %prog server-name [options]")
    parser.add_option("-k", "--insecure", dest="insecure",
                      action="store_true", default=False,
                      help="don't ask for a password")
    parser.add_option("-o", "--output", dest="output", metavar="file_name",
                      help="designate on output file")

    (options, args) = parser.parse_args()
    if len(args) != 1:
        parser.print_help()
        sys.exit(1)
    machine = args[0]
    passwd = ''
    if not options.insecure:
        passwd = getpass.getpass("Enter decryption password: ")
    config = MachineConfig(machine, passwd, mode.server_home)
    status = config.get()
    if status == FAIL:
        print "Bad config file."
        sys.exit(1)
    config.decrypt_config()
    data   = yaml.dump(config.data)
    if options.output:
        print "output to file_name: %s" % options.output
        open(options.output, 'w').write(data)
    else:
        print data

