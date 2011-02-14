import os
from bombardier_core.static_data import CLIENT_CONFIG_FILE
from bombardier_core.mini_utility import ensure_bombardier_config_dir
from bombardier_core.mini_utility import make_path, get_slash_cwd
from bombardier_core.mini_utility import yaml_load, yaml_dump

cwd = get_slash_cwd()
CONFIG_FILE = CLIENT_CONFIG_FILE.replace('\\', '/')
INSTANCE_WORK_DIR = make_path(cwd, 'spkg', "TEST_INSTANCE")
PKG_DIR = make_path(INSTANCE_WORK_DIR, "packages")
STATUS_FILE = make_path(INSTANCE_WORK_DIR, "status.yml")
SPKG_WORK_DIR = make_path(cwd, 'spkg')

def get_current_config():
    ensure_bombardier_config_dir()
    if os.path.isfile(CONFIG_FILE):
        current_config = yaml_load(open(CONFIG_FILE).read())
        if type(current_config) == type({}):
            return current_config
        else:
            return None

def start():
    cwd = get_slash_cwd()
    INSTANCE_WORK_DIR = make_path(cwd, 'spkg', "TEST_INSTANCE")
    status_file = make_path(INSTANCE_WORK_DIR, "status.yml")
    spkg_work_dir = make_path(cwd, 'spkg')
    config_data = {"spkg_path": spkg_work_dir}
    os.system('bash -c "mkdir -p %s"' % INSTANCE_WORK_DIR)
    os.system('bash -c "mkdir  %s/tmp"' % INSTANCE_WORK_DIR)
    #print "Copying repository files..."
    os.system('bash -c "cp -ax repos/ %s"' % spkg_work_dir)
    if not os.path.isdir(PKG_DIR):
        #print "Making %s..." % PKG_DIR
        os.system('bash -c "mkdir -p %s"' % PKG_DIR)
        #print "Copying package files..."
        os.system('bash -c "cd repos/type4 && tar -czmf TestPackage-7.spkg TestPackage-7"')
        os.system('bash -c "cp repos/type4/*.spkg %s"' % PKG_DIR)
    #print "Creating %s" % status_file
    open(status_file, 'w').write("{}")
    #print "Creating %s" % CONFIG_FILE
    current_config = get_current_config()
    if current_config:
        if "spkg_path" in current_config:
            current_config["old_spkg_path"] = current_config["spkg_path"]
            current_config["spkg_path"] = spkg_work_dir
    else:
        current_config = config_data
    config_fp = open(CONFIG_FILE, 'w')
    config_fp.write(yaml_dump(current_config))
    config_fp.flush()
    config_fp.close()

def cleanup():
    current_config = get_current_config()
    if current_config:
        if "old_spkg_path" in current_config:
            #print "Reverting %s" % CONFIG_FILE
            current_config["spkg_path"] = current_config["old_spkg_path"]
            del current_config["old_spkg_path"]
            open(CONFIG_FILE, 'w').write(yaml_dump(current_config))
    #print "Removing %s..." % SPKG_WORK_DIR
    os.system('bash -c "rm -rf %s"' % SPKG_WORK_DIR)
