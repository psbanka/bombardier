import os, yaml
from bombardier_core.static_data import CLIENT_CONFIG_FILE
from bombardier_core.mini_utility import ensure_bombardier_config_dir

CONFIG_FILE = CLIENT_CONFIG_FILE
INSTANCE_WORK_DIR = os.path.join(os.getcwd(), 'spkg', "TEST_INSTANCE")
PKG_DIR = os.path.join(INSTANCE_WORK_DIR, "packages")
STATUS_FILE = os.path.join(INSTANCE_WORK_DIR, "status.yml")
SPKG_WORK_DIR = os.path.join(os.getcwd(), 'spkg')

def get_current_config():
    ensure_bombardier_config_dir()
    if os.path.isfile(CONFIG_FILE):
        current_config = yaml.load(open(CONFIG_FILE).read())
        if type(current_config) == type({}):
            return current_config
        else:
            return None

def start():
    INSTANCE_WORK_DIR = os.path.join(os.getcwd(), 'spkg', "TEST_INSTANCE")
    status_file = os.path.join(INSTANCE_WORK_DIR, "status.yml")
    spkg_work_dir = os.path.join(os.getcwd(), 'spkg')
    config_data = {"spkg_path": spkg_work_dir}
    os.system("mkdir -p %s" % INSTANCE_WORK_DIR)
    print "Copying repository files..."
    os.system("cp -ax repos/ %s" % spkg_work_dir)
    if not os.path.isdir(PKG_DIR):
        print "Making %s..." % PKG_DIR
        os.system("mkdir -p %s" % PKG_DIR)
        print "Copying package files..."
        os.system("cp repos/type4/*.spkg %s" % PKG_DIR)
    print "Creating %s" % status_file
    open(status_file, 'w').write("{}")
    print "Creating %s" % CONFIG_FILE
    current_config = get_current_config()
    if current_config:
        if "spkg_path" in current_config:
            current_config["old_spkg_path"] = current_config["spkg_path"]
            current_config["spkg_path"] = spkg_work_dir
    else:
        current_config = config_data
    open(CONFIG_FILE, 'w').write(yaml.dump(current_config))

def cleanup():
    current_config = get_current_config()
    if current_config:
        if "old_spkg_path" in current_config:
            print "Reverting %s" % CONFIG_FILE
            current_config["spkg_path"] = current_config["old_spkg_path"]
            del current_config["old_spkg_path"]
            open(CONFIG_FILE, 'w').write(yaml.dump(current_config))
    print "Removing %s..." % SPKG_WORK_DIR
    os.system("rm -rf %s" % SPKG_WORK_DIR)
