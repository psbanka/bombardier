#!/usr/bin/env python

# bc.py: This module is essentially a hacked version of 
# ReconcileThread.py, and is meant to be run on a linux machine.
# It could use some refinement, but seems to work now.
# Copyright (C) 2005 Peter Banka

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

import sys, optparse, StringIO, traceback, yaml, time, re

from bombardier_core.libCipher import decryptString
from bombardier_core.Logger import Logger
from bombardier_core.Filesystem import Filesystem
from bombardier_client.Repository import Repository
from bombardier_client.Config import Config
from bombardier_client.Exceptions import ServerUnavailable
from bombardier_client.PackageV4 import PackageV4
from bombardier_client.PackageV5 import PackageV5
from bombardier_client.BombardierClass import Bombardier
from bombardier_core.mini_utility import getProgressPath, getSpkgPath
from bombardier_core.static_data import FIX, STATUS, CONFIGURE, RECONCILE
from bombardier_core.static_data import VERIFY, INSTALL, UNINSTALL, PURGE
from bombardier_core.static_data import DRY_RUN, INIT, EXECUTE
from bombardier_core.static_data import OK, FAIL
from bombardier_core import CORE_VERSION
from bombardier_client import CLIENT_VERSION
import os
import base64
import zlib

class NoInstanceError(Exception):
    def __init__(self, instance_name):
        Exception.__init__(self)
        self.instance_name = instance_name
    def __repr__(self):
        return "Attempting operation on a non-existant instance: %s" % self.instance_name

def exit_with_return_code(value):
    if type(value) != type(0):
        Logger.error("Invalid exit code, not an integer: %s" % value)
        value = FAIL
    Logger.warning("==EXIT-CODE==:%s" % value)
    sys.exit(value)

def find_likely_pkn(instance_name, pkn):
    status_yml = yaml.load(open(getProgressPath(instance_name)).read())
    pkns = []
    status_packages = status_yml['install-progress']
    for name in status_packages:
        if status_packages[name]['INSTALLED'] in [ 'NA', 'BROKEN' ]:
            continue
        if pkn.lower() in name.lower():
            pkns.append(name)
    if len(pkns) > 1:
        Logger.error( 'Ambiguous package name: %s could be any of %s' %(pkn, str(pkns)))
        exit_with_return_code(FAIL)
    if len(pkns) == 0:
        Logger.error( 'Package not found: %s' %pkn )
        exit_with_return_code(FAIL)
    else:
        pkn = '-'.join(pkns[0].split('-')[:-1])
        Logger.info( 'Using %s' %pkn)
        return pkn

def fix_spkg(instance_name, pkn, action, package_factory):
    status_data = open(getProgressPath(instance_name), 'r').read()
    status = yaml.load(status_data)
    if status.get("install-progress") == None:
        status["install-progress"] = {}
        Logger.warning( "Status file is empty." )
    now = time.asctime()
    if action == FIX:
        fix_name = []
        base_names = re.compile("(\s+)\-\d+").findall(pkn)
        if base_names:
            base_name = base_names[0]
        else:
            base_name = pkn
        for possible_pkn in status["install-progress"]:
            if base_name in possible_pkn:
                fix_name.append(possible_pkn)
        if len(fix_name) > 1:
            Logger.error("Package name %s is ambigious. (possible %s)" % (pkn, ' '.join(fix_name)))
            return FAIL
        elif len(fix_name) == 1:
            pkn = fix_name[0]
        elif len(fix_name) == 0:
            new_package = package_factory.get_me_one(pkn)
            pkn = new_package.full_name
            Logger.info("Selecting previously UNINSTALLED package: %s" % pkn)
        status["install-progress"]["%s" % pkn] = {"INSTALLED": now, "UNINSTALLED": "NA", "VERIFIED": now}
        Logger.info("==OUTPUT==:%s has been set to INSTALLED." % pkn )
    elif action == PURGE:
        if status["install-progress"].get(pkn):
            del status["install-progress"][pkn]
            msg = "==OUTPUT==:%s has been removed from %s status" % (pkn, instance_name)
            Logger.info(msg)
        else:
            Logger.info("==OUTPUT==:%s is not in the status file" % pkn)
            pkns = status["install-progress"]
            possible_names = [x for x in pkns if pkn in x]
            Logger.info("==OUTPUT==:Maybe you want one of these: %s" % str(possible_names))
            return FAIL
    open(getProgressPath(instance_name), 'w').write(yaml.dump(status))
    return OK

class BombardierEnvironment:
    def __init__(self, instance_name):
        self.filesystem      = Filesystem()
        self.repository      = None
        self.config          = None
        self.instance_name    = instance_name

    def data_request(self):
        STREAM_BLOCK_SIZE= 77
        b64Data = []
        while True:
            chunk = sys.stdin.read(STREAM_BLOCK_SIZE)
            if not chunk or chunk[0] == ' ':
                break
            b64Data.append(chunk)
        yaml_data = ''
        yaml_data = zlib.decompress(base64.decodestring(''.join(b64Data)))
        Logger.debug("Received %s lines of yaml" % len(yaml_data.split('\n')))

        try:
            input_data = yaml.load(yaml_data)
        except:
            ermsg = "Received bad YAML: %s" % (repr(yaml_data))
            raise ServerUnavailable, ("input_data", ermsg)
        if type(input_data) == type("string"):
            Logger.error("Invalid Yaml on server: %s" % input_data)
            raise ServerUnavailable, ("input_data", "invalid yaml")
        if type(input_data) != type({}) and type(input_data) != type([]): # backwards comptible yaml
            input_data = input_data.next()
        config_key = input_data.get("config_key", None)
        if config_key:
            enc_yaml_file = os.path.join(getSpkgPath(), instance_name, 'client.yml.enc')
            if not os.path.isfile(enc_yaml_file):
                raise ServerUnavailable, ("input_data", "no %s" % enc_yaml_file)
            enc_data = open(enc_yaml_file).read()
            plain_yaml_str = decryptString(enc_data, config_key)
            try:
                input_data = yaml.load(plain_yaml_str)
            except:
                ermsg = "Received bad YAML file: %s" % enc_yaml_file
                raise ServerUnavailable, ("input_data", ermsg)
                
        config_data  = input_data.get("configData")
        if not config_data:
            Logger.error("No configuration data received")
            raise ServerUnavailable, ("config_data", "invalid yaml")
        package_data = input_data.get("packageData", {})
        self.config = Config(self.filesystem, instance_name, config_data)
        self.repository = Repository(self.filesystem, instance_name, package_data)

    def clear_lock(self):
        self.filesystem.clearLock()

class PackageFactory:
    def __init__(self, env):
        self.env = env

    def get_me_one(self, pkn):
        '''
        create a package object based on pkn
        pkn -- the name of the package to create
        '''
        version = self.env.repository.determine_pkg_version(pkn)
        pkg = None
        if version == 4:
            new_package = PackageV4(pkn, self.env.repository,
                                    self.env.config,
                                    self.env.filesystem,
                                    self.env.instance_name)
        else:
            new_package = PackageV5(pkn, self.env.repository,
                                    self.env.config,
                                    self.env.filesystem,
                                    self.env.instance_name)
        new_package.initialize()
        return new_package

def get_bc(instance_name, env):
    env.clear_lock()
    bc = Bombardier(env.repository, env.config, env.filesystem,
                    instance_name)
    return bc

def instance_setup(instance_name):
    progress_path = getProgressPath(instance_name)
    status_dict = None
    if os.path.isfile(progress_path):
        try:
            status_dict = yaml.load(open(progress_path).read())
        except:
            Logger.warning("Unable to load existing yaml from %s" % progress_path)
    if type(status_dict) != type({}):
        status_dict = {"install-progress": {}}
    status_dict["client_version"] = CLIENT_VERSION
    status_dict["core_version"] = CORE_VERSION
    status_dict["clientVersion"] = CLIENT_VERSION
    status_dict["coreVersion"] = CORE_VERSION
    pkg_dir = os.path.join(getSpkgPath(), instance_name, "packages")
    if not os.path.isdir(pkg_dir):
        os.makedirs(pkg_dir)
    open(progress_path, 'w').write(yaml.dump(status_dict))

def process_action(action, instance_name, pkn, script_name,
                   package_factory, env):
    if action == INIT:
        instance_setup(instance_name)
        return OK

    if action in [ UNINSTALL, VERIFY, CONFIGURE, EXECUTE ]:
        pkn = find_likely_pkn(instance_name, pkn)

    status = FAIL
    try:
        if action in [ FIX, PURGE ]:
            status = fix_spkg(instance_name, pkn, action, package_factory)
            return status
        bc = get_bc(instance_name, env)
        if action == STATUS:
            status_dict = bc.check_system()
            if type(status_dict) == type({}):
                if status_dict["broken"]:
                    Logger.info("BROKEN PACKAGES:")
                    for pkn in status_dict["broken"]:
                        Logger.info("- %s" % pkn)
                status = OK
            else:
                status = FAIL
        elif action in [ RECONCILE, DRY_RUN ]:
            bc.record_errors = True
            status = bc.reconcile_system(action)
        else:
            bc.record_errors = False
            status = bc.use_pkg(pkn, action, script_name)

        bc.filesystem.clearLock()
    except:
        e = StringIO.StringIO()
        traceback.print_exc(file=e)
        e.seek(0)
        data = e.read()
        ermsg = ''
        for line in data.split('\n'):
            ermsg += "\n||>>>%s" % line
        Logger.error(ermsg)
        return FAIL
    return status

if __name__ == "__main__":
    import optparse
    Logger.add_std_err_logging()

    usage  = ["usage: %prog { -s | -r | -n } INSTANCE | "]
    usage += ["       %prog { -c | -v | -i | -u } INSTANCE PACKAGE-NAME [ PACKAGE-NAME2 ... ] | "]
    usage += ["       %prog { -f | -p } INSTANCE FULL-PACKAGE-NAME [FULL-PACKAGE-NAME2 ... ] | "]
    usage += ["       %prog -x INSTANCE PACKAGE-NAME SCRIPT-NAME "]
    usage += ['']
    usage += ["INSTANCE := A complete bombardier client configuration"]
    usage += ["PACKAGE-NAME := An individual software module"]
    usage += ["FULL-PACKAGE-NAME := {PACKAGE-NAME}-{PACKAGE-REVISION}"]
    usage += ["SCRIPT-NAME := The name of a maintenance script that resides in the package"]
    parser = optparse.OptionParser('\n'.join(usage))

    parser.add_option("-s", "--status", dest="action",
                      action="store_const", const=STATUS,
                      help="display the status of the system")
    parser.add_option("-c", "--configure", dest="action",
                      action="store_const", const=CONFIGURE,
                      help="configure a package")
    parser.add_option("-v", "--verify", dest="action",
                      action="store_const", const=VERIFY,
                      help="verify a package")
    parser.add_option("-i", "--install", dest="action",
                      action="store_const", const=INSTALL,
                      help="install a package")
    parser.add_option("-r", "--reconcile", dest="action",
                      action="store_const", const=RECONCILE,
                      help="reconcile the system")
    parser.add_option("-x", "--execute", dest="action",
                      action="store_const", const=EXECUTE,
                      help="Execute a maintenance script")
    parser.add_option("-u", "--uninstall", dest="action",
                      action="store_const", const=UNINSTALL,
                      help="uninstall a package")
    parser.add_option("-f", "--fix", dest="action",
                      action="store_const", const=FIX,
                      help="set a package status to INSTALLED without doing anything")
    parser.add_option("-p", "--purge", dest="action",
                      action="store_const", const=PURGE,
                      help="Remove a package from the status")
    parser.add_option("-d", "--dry-run", dest="action",
                      action="store_const", const=DRY_RUN,
                      help="Do a test reconcile")
    parser.add_option("-n", "--init", dest="action",
                      action="store_const", const=INIT,
                      help="Initialize the client after installation")

    (options, args) = parser.parse_args()
    if len(args) < 1:
        print "CMD: %s" % ' '.join(sys.argv)
        print "This command requires an instance name."
        parser.print_help()
        exit_with_return_code(1)
    instance_name = args[0]

    env = BombardierEnvironment(instance_name)
    env.data_request()
    package_factory = PackageFactory(env)

    if options.action in [ RECONCILE, STATUS, DRY_RUN, INIT ]:
        status = process_action(options.action, instance_name,
                               '', '', package_factory, env)
    else:
        script_name   = ""
        if len(args) < 2:
            print "CMD: %s" % ' '.join(sys.argv)
            print "This command requires a package name as an argument."
            parser.print_help()
            exit_with_return_code( 1 )
        pkns = args[1:]
        if options.action == EXECUTE:
            if len(args) != 3:
                print "CMD: %s" % ' '.join(sys.argv)
                print "This command requires a package name and a script name."
                parser.print_help()
                exit_with_return_code( 1 )
            pkns = [args[1]]
            script_name = args[2]

        for pkn in pkns:
            status = process_action(options.action, instance_name,
                                    pkn, script_name,
                                    package_factory, env)
            if status != OK:
                exit_with_return_code(status)
    exit_with_return_code(status)
