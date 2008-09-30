#!c:\Python25\python.exe

# bc2.py: This module is essentially a hacked version of 
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

from bombardier.staticData import *
from bombardier.Logger import logger, addStdErrLogging
from bombardier.Filesystem import Filesystem
from bombardier.Repository import Repository
from bombardier.Config import Config
import bombardier.Exceptions
import bombardier.Package
import bombardier.BombardierClass
from bombardier.miniUtility import getProgressPath
import base64
import zlib

class NoInstanceError(Exception):
    def __init__(self, instanceName):
        Exception.__init__(self)
        self.instanceName = instanceName
    def __repr__(self):
        return "Attempting operation on a non-existant instance: %s" % self.instanceName

def findLikelyPackageName(instanceName, packageName):
    statusYml = yaml.load(open(getProgressPath(instanceName)).read())
    packageNames = []
    statusPackages = statusYml['install-progress']
    for name in statusPackages:
        if statusPackages[name]['INSTALLED'] in [ 'NA', 'BROKEN' ]:
            continue
        if packageName.lower() in name.lower():
            packageNames.append(name)
    if len(packageNames) > 1:
        logger.error( 'Ambiguous package name: %s could be any of %s' %(packageName, str(packageNames)))
        sys.exit(FAIL)
    if len(packageNames) == 0:
        logger.error( 'Package not found: %s' %packageName )
        sys.exit(FAIL)
    else:
        packageName = '-'.join(packageNames[0].split('-')[:-1])
        logger.info( 'Using %s' %packageName)
        return packageName

def fixSpkg(instanceName, packageName, action, packageFactory):
    statusData = open(getProgressPath(instanceName), 'r').read()
    status = yaml.load(statusData)
    if status.get("install-progress") == None:
        status["install-progress"] = {}
        logger.warning( "Status file is empty." )
    now = time.asctime()
    if action == FIX:
        fixName = []
        baseNames = re.compile("(\s+)\-\d+").findall(packageName)
        if baseNames:
            baseName = baseNames[0]
        else:
            baseName = packageName
        for possiblePackageName in status["install-progress"]:
            if baseName in possiblePackageName:
                fixName.append(possiblePackageName)
        if len(fixName) > 1:
            logger.error("Package name %s is ambigious. (possible %s)" % (packageName, ' '.join(fixName)))
            return FAIL
        elif len(fixName) == 1:
            packageName = fixName[0]
        elif len(fixName) == 0:
            newPackage = packageFactory.getMeOne(packageName)
            packageName = newPackage.fullName
            logger.info("Selecting previously UNINSTALLED package: %s" % packageName)
        status["install-progress"]["%s" % packageName] = {"INSTALLED": now, "UNINSTALLED": "NA", "VERIFIED": now}
        logger.info("%s has been set to INSTALLED." % packageName )
    elif action == PURGE:
        if status["install-progress"].get(packageName):
            del status["install-progress"][packageName]
            logger.info("%s has been removed from status.yml" % packageName)
        else:
            logger.warning("%s is not in the status.yml file" % packageName)
            packageNames = status["install-progress"]
            possibleNames = [x for x in packageNames if packageName in x]
            logger.info("Maybe you want one of these: %s" % str(possibleNames))
            return FAIL
    open(getProgressPath(instanceName), 'w').write(yaml.dump(status))
    return OK

class BombardierEnvironment:
    def __init__(self, instanceName):
        self.filesystem      = Filesystem()
        self.repository      = None
        self.config          = None
        self.operatingSystem = None
        self.instanceName    = instanceName
        if sys.platform == "linux2":
            import bombardier.Linux
            self.operatingSystem = bombardier.Linux.Linux()
        else:
            import bombardier.Windows
            self.operatingSystem = bombardier.Windows.Windows()

    def dataRequest(self):
        STREAM_BLOCK_SIZE= 77
        b64Data = []
        while True:
            chunk = sys.stdin.read(STREAM_BLOCK_SIZE)
            if not chunk or chunk[0] == ' ':
                break
            b64Data.append(chunk)
        yamlData = ''
        yamlData = zlib.decompress(base64.decodestring(''.join(b64Data)))
        logger.debug("Received %s lines of yaml" % len(yamlData.split('\n')))

        try:
            inputData = yaml.load(yamlData)
        except:
            ermsg = "Received bad YAML: %s" % (repr(yamlData))
            raise bombardier.Exceptions.ServerUnavailable, ("inputData", ermsg)
        if type(inputData) == type("string"):
            logger.error("Invalid Yaml on server: %s" % inputData)
            raise bombardier.Exceptions.ServerUnavailable, ("inputData", "invalid yaml")
        if type(inputData) != type({}) and type(inputData) != type([]): # backwards comptible yaml
            inputData = inputData.next()
        configData  = inputData.get("configData")
        if not configData:
            logger.error("No configuration data received")
            raise bombardier.Exceptions.ServerUnavailable, ("configData", "invalid yaml")
        packageData = inputData.get("packageData", {})
        self.config = Config(self.filesystem, instanceName, configData)
        self.repository = Repository(self.filesystem, instanceName, packageData)

    def clearLock(self):
        self.filesystem.clearLock()

class PackageFactory:
    def __init__(self, env):
        self.env = env

    def getMeOne(self, packageName):
        newPackage = bombardier.Package.Package(packageName, self.env.repository, self.env.config,
                                     self.env.filesystem, self.env.operatingSystem, self.env.instanceName)
        newPackage.initialize()
        return newPackage

def getBc(instanceName, env):
    env.clearLock()
    bc = bombardier.BombardierClass.Bombardier(env.repository, env.config, env.filesystem,
                                               env.operatingSystem, instanceName)
    return bc

def instanceSetup(instanceName):
    statusDct = {"status": {"newInstall": "True"}}
    statusDct["clientVersion"] = VERSION
    pkgDir = "%s/packages" % instanceName
    if not os.path.isdir(pkgDir):
        os.makedirs(pkgDir)
    open(getProgressPath(instanceName), 'w').write(yaml.dump(statusDct))

def processAction(action, instanceName, packageName, scriptName, packageFactory, env):
    if action == INIT:
        instanceSetup(instanceName)
        return OK

    if action in [ UNINSTALL, VERIFY, CONFIGURE, EXECUTE ]:
        packageName = findLikelyPackageName(instanceName, packageName)

    status = FAIL
    try:
        if action in [ FIX, PURGE ]:
            status = fixSpkg(instanceName, packageName, action, packageFactory)
            return status
        bc = getBc(instanceName, env)
        if action == STATUS:
            statusDict = bc.checkSystem()
            if type(statusDict) == type({}):
                if statusDict["broken"]:
                    logger.info("BROKEN PACKAGES:")
                    for packageName in statusDict["broken"]:
                        logger.info("- %s" % packageName)
                status = OK
            else:
                status = FAIL
        elif action in [ RECONCILE, DRY_RUN ]:
            bc.recordErrors = True
            status = bc.reconcileSystem(action=action)
        else:
            bc.recordErrors = False
            status = bc.usePackage(packageName, action, scriptName)

        bc.filesystem.clearLock()
    except:
        e = StringIO.StringIO()
        traceback.print_exc(file=e)
        e.seek(0)
        data = e.read()
        ermsg = ''
        for line in data.split('\n'):
            ermsg += "\n||>>>%s" % line
        logger.error(ermsg)
        return FAIL
    return status

if __name__ == "__main__":
    import optparse
    addStdErrLogging()

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
        sys.exit(1)
    instanceName = args[0]

    env = BombardierEnvironment(instanceName)
    env.dataRequest()
    packageFactory = PackageFactory(env)

    if options.action in [ RECONCILE, STATUS, DRY_RUN, INIT ]:
        status = processAction(options.action, instanceName,
                               '', '', packageFactory, env)
    else:
        scriptName   = ""
        if len(args) < 2:
            print "CMD: %s" % ' '.join(sys.argv)
            print "This command requires a package name as an argument."
            parser.print_help()
            sys.exit( 1 )
        packageNames = args[1:]
        if options.action == EXECUTE:
            if len(args) != 3:
                print "CMD: %s" % ' '.join(sys.argv)
                print "This command requires a package name and a script name."
                parser.print_help()
                sys.exit( 1 )
            packageNames = [args[1]]
            scriptName = args[2]

        for packageName in packageNames:
            status = processAction(options.action, instanceName, packageName, scriptName, packageFactory, env)
            if status != OK:
                 sys.exit(status)
    sys.exit(status)
