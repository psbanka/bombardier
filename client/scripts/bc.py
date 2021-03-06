#!/usr/bin/python
"""module is meant to be run on a linux machine. This is the method the server
uses to interact with a box: find out what pacakges need to be installed, etc.
Currently supports RHEL5 and Ubuntu Hardy. Other POSIX environments could
be easily added, including cygwin"""

# Copyright (C) 2005-2011 Peter Banka et al

# BSD License
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the name of the GE Security nor the names of its contributors may
#   be used to endorse or promote products derived from this software without
#   specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

import sys, optparse, StringIO, traceback, time, re
import simplejson as json
import tempfile

from bombardier_core.Logger import Logger
from bombardier_core.Config import Config
from bombardier_client.Repository import Repository
from bombardier_core.Exceptions import ConfigurationException
from bombardier_client.PackageV4 import PackageV4
from bombardier_client.PackageV5 import PackageV5
from bombardier_client.BombardierClass import Bombardier
from bombardier_core.mini_utility import get_progress_path, get_spkg_path
from bombardier_core.mini_utility import make_path
from bombardier_core.static_data import FIX, CHECK_STATUS, CONFIGURE, RECONCILE
from bombardier_core.static_data import VERIFY, INSTALL, UNINSTALL, PURGE
from bombardier_core.static_data import DRY_RUN, INIT, EXECUTE, BACKUP, RESTORE
from bombardier_core.static_data import OK, FAIL, ACTION_REVERSE_LOOKUP
from bombardier_core import CORE_VERSION
from bombardier_client import CLIENT_VERSION
import os
import base64

STREAM_BLOCK_SIZE = 77

USAGE  = ["usage: %prog { -s | -r | -n } INSTANCE | "]
USAGE += ["       %prog { -c | -v | -i | -u } INSTANCE PACKAGE-NAME [ PACKAGE-NAME2 ... ] | "]
USAGE += ["       %prog { -f | -p } INSTANCE FULL-PACKAGE-NAME [FULL-PACKAGE-NAME2 ... ] | "]
USAGE += ["       %prog -x INSTANCE PACKAGE-NAME SCRIPT-NAME "]
USAGE += ['']
USAGE += ["INSTANCE := A complete bombardier client configuration"]
USAGE += ["PACKAGE-NAME := An individual software module"]
USAGE += ["FULL-PACKAGE-NAME := {PACKAGE-NAME}-{PACKAGE-REVISION}"]
USAGE += ["SCRIPT-NAME := The name of a maintenance script that resides in the package"]

class NoInstanceError(Exception):
    "Asking to work on an instance that doesn't exist"
    def __init__(self, instance_name):
        Exception.__init__(self)
        self.instance_name = instance_name
    def __repr__(self):
        msg = "Attempting operation on a non-existant instance: %s"
        msg = msg  % self.instance_name
        return msg

def exit_with_return_code(value):
    "Provide proper output to the CNM server"
    if type(value) != type(0):
        Logger.error("Invalid exit code, not an integer: %s" % value)
        value = FAIL
    Logger.warning("==EXIT-CODE==:%s" % value)
    sys.exit(value)

def find_likely_pkn(instance_name, pkn):
    """Sometimes an ambiguous package name is requested.
    This attempts to help a guy out."""
    progress_file = get_progress_path(instance_name)
    try:
        status_yml = json.loads(open(progress_file).read())
    except ValueError:
        Logger.error("Progress data in %s is not parsable." % progress_file)
        raise
    pkns = []
    status_packages = status_yml['install-progress']
    for name in status_packages:
        if status_packages[name]['INSTALLED'] in [ 'NA', 'BROKEN' ]:
            continue
        if pkn.lower() in name.lower():
            pkns.append(name)
    if len(pkns) > 1:
        msg = 'Ambiguous package name: %s could be any of %s' % (pkn, str(pkns))
        Logger.error( msg )
        exit_with_return_code(FAIL)
    if len(pkns) == 0:
        Logger.error( 'Package not found: %s' %pkn )
        exit_with_return_code(FAIL)
    else:
        pkn = '-'.join(pkns[0].split('-')[:-1])
        Logger.info( 'Using %s' %pkn)
        return pkn

def fix_spkg(instance_name, pkn, action, package_factory):
    """A user is requesting to take a package that is currently flagged
    as 'broken' and set it to be 'installed'. Perhaps they have manually
    fixed the package on the system."""
    progress_file = get_progress_path(instance_name)
    status_data = open(progress_file, 'r').read()
    try:
        status = json.loads(status_data)
    except ValueError:
        Logger.error("Data in %s is not parsable by json" % progress_file)
        return FAIL
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
            msg = "Package name %s is ambigious. (possible %s)"
            msg = msg % (pkn, ' '.join(fix_name))
            Logger.error(msg)
            return FAIL
        elif len(fix_name) == 1:
            pkn = fix_name[0]
        elif len(fix_name) == 0:
            new_package = package_factory.get_me_one(pkn)
            pkn = new_package.full_name
            Logger.info("Selecting previously UNINSTALLED package: %s" % pkn)
        status["install-progress"]["%s" % pkn] = {"INSTALLED": now,
                                                  "UNINSTALLED": "NA",
                                                  "VERIFIED": now}
        Logger.info("==OUTPUT==:%s has been set to INSTALLED." % pkn )
    elif action == PURGE:
        if status["install-progress"].get(pkn):
            del status["install-progress"][pkn]
            msg = "==OUTPUT==:%s has been removed from %s status"
            msg = msg % (pkn, instance_name)
            Logger.info(msg)
        else:
            Logger.info("==OUTPUT==:%s is not in the status file" % pkn)
            pkns = status["install-progress"]
            possible_names = [x for x in pkns if pkn in x]
            msg = "==OUTPUT==:Maybe you want one of these: %s"
            msg = msg % str(possible_names)
            Logger.info(msg)
            return FAIL
    open(get_progress_path(instance_name), 'w').write(json.dumps(status))
    return OK

class BombardierEnvironment:
    """Holds information about the system Configuration. Should
    probably be removed"""
    def __init__(self, instance_name):
        """instance_name -- a machine can have several different names
                            that it is referred to to that packages can
                            be installed more than once"""
        self.repository      = None
        self.config          = None
        self.instance_name   = instance_name

    def data_request(self):
        "Obtain configuration data from the server"
        b64_data = []
        while True:
            #Logger.info( "STARTING READ" )
            chunk = sys.stdin.read(STREAM_BLOCK_SIZE).strip()
            #Logger.info( "READ FROM SERVER: [%s]" % chunk)
            if not chunk or chunk[0] == ' ' or chunk.endswith("-"):
                chunk = chunk[:-1]
                b64_data.append(chunk)
                break
            b64_data.append(chunk)
            sys.stdout.write(">\n")
            sys.stdout.flush()
        #Logger.info("FINISHED INPUT LOOP")
        #print "b64_data: ", b64_data
        json_data = ''
        #json_data = zlib.decompress(base64.decodestring(''.join(b64_data)))
      
        json_data = base64.decodestring(''.join(b64_data))
        #print "json_dataa", json_data
        Logger.debug("Received %s lines of json" % len(json_data.split('\n')))

        try:
            input_data = json.loads(json_data)
            #print "input_dataa", input_data
        except:
            ermsg = "Configuration data not YAML-parseable: %s" % (repr(json_data))
            file_number, filename = tempfile.mkstemp(suffix=".yml")
            fh = open(filename, 'w')
            Logger.error("Writing bad data to %s" % filename)
            for line in json_data.split('\n'):
                fh.write("[%s]" % line)
            fh.flush()
            fh.close()
            raise ConfigurationException(ermsg)
        if type(input_data) == type("string"):
            ermsg = "Configuration data not YAML-parseable: %s" % (repr(json_data))
            raise ConfigurationException(ermsg)
        if type(input_data) != type({}) and type(input_data) != type([]):
            input_data = input_data.next()
        config_key = input_data.get("config_key", None)
        if config_key:
            try:
                from bombardier_core.Cipher import Cipher
            except ImportError:
                msg = "This machine cannot accept an encrypted configuration"
                raise ConfigurationException(msg)
            enc_json_file = make_path(get_spkg_path(), self.instance_name,
                                         'client.yml.enc')
            if not os.path.isfile(enc_json_file):
                msg = "%s file doesn't exist" % enc_json_file
                raise ConfigurationException(msg)
            enc_data = open(enc_json_file).read()
            cipher = Cipher(config_key)
            plain_json_str = cipher.decrypt_string(enc_data)
            try:
                input_data = json.loads(plain_json_str)
            except:
                ermsg = "Received bad YAML file: %s" % enc_json_file
                raise ConfigurationException(ermsg)

        config_data  = input_data.get("config_data")
        if not config_data:
            raise ConfigurationException("No configuration data received")
        package_data = input_data.get("package_data", {})
        self.config = Config(self.instance_name, config_data)
        self.repository = Repository(self.instance_name, package_data)

class PackageFactory:
    "Generate type-4 or type-5 packages"
    def __init__(self, env):
        self.env = env

    def get_me_one(self, pkn):
        '''
        create a package object based on pkn
        pkn -- the name of the package to create
        '''
        version = self.env.repository.determine_pkg_version(pkn)
        if version == 4:
            new_package = PackageV4(pkn, self.env.repository,
                                    self.env.config,
                                    self.env.instance_name)
        else:
            new_package = PackageV5(pkn, self.env.repository,
                                    self.env.config,
                                    self.env.instance_name)
        new_package.initialize()
        return new_package

def get_bc(instance_name, env):
    "This class is the primary interface to the rest of the system"
    bc_obj = Bombardier(env.repository, env.config, instance_name)
    return bc_obj

def instance_setup(instance_name):
    "When Bombardier is first getting set up, this method is called."
    progress_path = get_progress_path(instance_name)
    status_dict = None
    if os.path.isfile(progress_path):
        try:
            status_dict = json.loads(open(progress_path).read())
        except:
            msg = "Unable to load existing json from %s" % progress_path
            Logger.warning(msg)
    if type(status_dict) != type({}):
        status_dict = {"install-progress": {}}
    status_dict["client_version"] = CLIENT_VERSION
    status_dict["core_version"] = CORE_VERSION
    status_dict["clientVersion"] = CLIENT_VERSION
    status_dict["coreVersion"] = CORE_VERSION
    pkg_dir = make_path(get_spkg_path(), instance_name, "packages")
    repos_dir = make_path(get_spkg_path(), "repos")
    tmp_dir = make_path(get_spkg_path(), "tmp")
    if not os.path.isdir(pkg_dir):
        os.makedirs(pkg_dir)
    if not os.path.isdir(repos_dir):
        os.makedirs(repos_dir)
    if not os.path.isdir(tmp_dir):
        os.makedirs(tmp_dir)
    open(progress_path, 'w').write(json.dumps(status_dict))

def process_action(action, instance_name, pkn, method_name,
                   arguments, package_factory):
    """
    Performs a Bombardier action on this system
    action -- either INIT, INSTALL, UNINSTALL, VERIFY, CONFIGURE,
              EXECUTE, RECONCILE, CHECK_STATUS, FIX, or PURGE
    instance_name -- the name of the machine
    pkn -- the package name to operate on. Could be none for actions
           that operate on the whole machine, such as RECONCILE,
           CHECK_STATUS, or INIT 
    method_name -- Only applicable for the EXECUTE action, runs a named
                   method within a package.
    package_factory -- provides packages we may need
    """
    if action == INIT:
        instance_setup(instance_name)
        return OK

    if action in [ UNINSTALL, VERIFY, CONFIGURE, EXECUTE, BACKUP ]:
        try:
            pkn = find_likely_pkn(instance_name, pkn)
        except ValueError:
            return FAIL

    status = FAIL
    try:
        if action in [ FIX, PURGE ]:
            status = fix_spkg(instance_name, pkn, action, package_factory)
            return status
        bc_obj = get_bc(instance_name, package_factory.env)
        if action == CHECK_STATUS:
            status_dict = bc_obj.check_system()
            if type(status_dict) == type({}):
                if not status_dict["Packages that are BROKEN"]:
                    status = OK
            else:
                status = FAIL
        elif action in [ RECONCILE, DRY_RUN ]:
            bc_obj.record_errors = True
            status = bc_obj.reconcile_system(action)
        else: # INSTALL, UNINSTALL, VERIFY, BACKUP, CONFIGURE, EXECUTE, RESTORE
            bc_obj.record_errors = False
            status = bc_obj.use_pkg(pkn, action, method_name, arguments)
    except:
        err = StringIO.StringIO()
        traceback.print_exc(file = err)
        err.seek(0)
        data = err.read()
        ermsg = ''
        for line in data.split('\n'):
            ermsg += "\n||>>>%s" % line
        Logger.error(ermsg)
        return FAIL
    return status

def main():
    Logger.add_std_err_logging()

    parser = optparse.OptionParser('\n'.join(USAGE))

    parser.add_option("-s", "--status", dest="action",
                      action="store_const", const=CHECK_STATUS,
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
    parser.add_option("-B", "--backup", dest="action",
                      action="store_const", const=BACKUP,
                      help="Tell a package to back itself up")
    parser.add_option("-R", "--restore", dest="action",
                      action="store_const", const=RESTORE,
                      help="Tell a package to restore itself")

    (options, args) = parser.parse_args()
    Logger.info("options: (%s) / args: (%s)" % (options, args))
    if options.action:
        Logger.info("Action: %s" % (ACTION_REVERSE_LOOKUP[options.action]))
    if len(args) < 1:
        print "CMD: %s" % ' '.join(sys.argv)
        print "This command requires an instance name."
        parser.print_help()
        exit_with_return_code(1)
    instance_name = args[0]
    env = BombardierEnvironment(instance_name)
    env.data_request()
    package_factory = PackageFactory(env)
    arguments = []

    if options.action in [ RECONCILE, CHECK_STATUS, DRY_RUN, INIT ]:
        status = process_action(options.action, instance_name,
                               '', '', '', package_factory)
    else:
        method_name   = ""
        if len(args) < 2:
            print "CMD: %s" % ' '.join(sys.argv)
            print "This command requires a package name as an argument."
            Logger.error("This command requires a package name as an argument.")
            parser.print_help()
            exit_with_return_code( 1 )
        package_name = args[1]
        if options.action == BACKUP:
            method_name = "backup"
        if options.action == RESTORE:
            method_name = "restore"
            arguments = args[2:]

        if options.action == EXECUTE:
            if len(args) < 3:
                print "CMD: %s" % ' '.join(sys.argv)
                print "This command requires a package name and a script name."
                Logger.error("This command requires a package name and a script name.")
                parser.print_help()
                exit_with_return_code( 1 )
            package_name = args[1]
            method_name = args[2]
            if len(args) > 3:
                arguments = args[3:]

        status = process_action(options.action, instance_name,
                                package_name, method_name, arguments,
                                package_factory)
        if status != OK:
            exit_with_return_code(status)
    exit_with_return_code(status)

if __name__ == "__main__":
    main()
