"MachineConfig module, does merged cmdb entries and decrption"
#!/opt/python2.5/bin/python
from bombardier_core.mini_utility import add_dictionaries
import os
import yaml, syck
import getpass
from bombardier_core.Cipher import Cipher
from bombardier_core.static_data import OK, FAIL
from Exceptions import MachineConfigurationException

class MachineConfig:
    """Configuration management class for a machine, this class ties together
       multiple include files, merges them together as well as decrypting 
       individual configuration items"""
    def __init__(self, machine_name, password, server_home):
        self.data       = {}
        self.includes   = []
        if not machine_name:
            errmsg = "Machine name not specified"
            raise MachineConfigurationException("NOT_DEFINED", errmsg)
        self.machine_name = machine_name
        self.password     = password
        self.server_home  = server_home

    def __getitem__(self, key):
        return self.data[key]

    @classmethod
    def find_include_list(cls, data_dict):
        "Finds files to include"
        include_list = []
        for key in data_dict:
            if key.upper() == "INCLUDE":
                include_list += data_dict[key]
        return include_list

    def merge(self):
        "Public method to get a machine's merged config"
        self.merge_includes()
        return self.convert_boms()

    def load_includes(self, new_includes):
        "Add new includes and merge them into main config"
        for include_name in new_includes:
            if include_name not in self.includes:
                self.includes.append(include_name)
                self.merge_includes(include_name)

    def merge_includes(self, config_name=''):
        "Apply config items from include files into main config"
        if config_name == '':
            yml_directory = "machine"
            config_name = self.machine_name
        else:
            yml_directory = "include"
        file_name = os.path.join(self.server_home, yml_directory,
                                 "%s.yml" % config_name)
        if not os.path.isfile(file_name):
            errmsg = "Error; %s does not exist." % file_name
            raise MachineConfigurationException(self.machine_name, errmsg)
        new_data = syck.load( open(file_name, 'r').read() )
        if new_data == None:
            new_data = {}
        if type(new_data) != type({}):
            errmsg = "Error parsing %s (yaml did not get a dict)" % file_name
            raise MachineConfigurationException(self.machine_name, errmsg)
        self.data = add_dictionaries(self.data, new_data)
        new_includes = self.find_include_list(new_data)
        self.load_includes(new_includes)

    def convert_boms(self):
        "Pull in list of packages from boms and add them into packages list"
        boms = self.data.get("bom", [])
        packages = set(self.data.get("packages", []))
        for bom in boms:
            file_name = os.path.join(self.server_home, "bom", "%s.yml" % bom)
            if not os.path.isfile(file_name):
                errmsg = "%s does not exist" % file_name
                raise MachineConfigurationException(self.machine_name, errmsg)
            packages = packages.union(set(syck.load(open(file_name).read())))
        self.data["packages"] = list(packages)
        if self.data.get("bom"):
            del self.data["bom"]
        return OK

    def decrypt_config(self):
        "Decrypt entries as necessary with decryptLoop from Cipher"
        if 1 == 1:
        #try:
            cipher = Cipher(self.password)
            cipher.decrypt_dict(self.data)
        else:
        #except:
            errmsg = "Error decrypting configuration"
            raise MachineConfigurationException(self.machine_name, errmsg)
            

def main_func():
    "Utility function for command line usage"
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
    password = ''
    if not options.insecure:
        password = getpass.getpass("Enter decryption password: ")
    config = MachineConfig(machine, password, mode.server_home)
    status = config.get()
    if status == FAIL:
        print "Bad config file."
        sys.exit(1)
    config.decrypt_config()
    data = yaml.dump(config.data)
    if options.output:
        print "output to file_name: %s" % options.output
        open(options.output, 'w').write(data)
    else:
        print data

if __name__ == "__main__":
    main_func()
