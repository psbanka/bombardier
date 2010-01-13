import os, yaml

CONFIG_FILE = "/etc/bombardier.yml"
INSTANCE_WORK_DIR = os.path.join(os.getcwd(), 'spkg', "TEST_INSTANCE")
PKG_DIR = os.path.join(INSTANCE_WORK_DIR, "packages")
STATUS_FILE = os.path.join(INSTANCE_WORK_DIR, "status.yml")
SPKG_WORK_DIR = os.path.join(os.getcwd(), 'spkg')

def start():
    INSTANCE_WORK_DIR = os.path.join(os.getcwd(), 'spkg', "TEST_INSTANCE")
    status_file = os.path.join(INSTANCE_WORK_DIR, "status.yml")
    spkg_work_dir = os.path.join(os.getcwd(), 'spkg')
    config_data = {"spkg_path": spkg_work_dir}
    if not os.path.isdir(PKG_DIR):
        print "Making %s..." % PKG_DIR
        os.system("mkdir -p %s" % PKG_DIR)
    if not os.path.isdir(status_file):
        print "Creating %s" % status_file
        open(status_file, 'w').write("{}")
    if not os.path.isdir(CONFIG_FILE):
        print "Creating %s" % CONFIG_FILE
        open(CONFIG_FILE, 'w').write(yaml.dump(config_data))

def cleanup():
    print "Removing %s..." % SPKG_WORK_DIR
    os.system("rm -rf %s" % SPKG_WORK_DIR)
