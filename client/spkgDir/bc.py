#!/usr/local/bin/python2.4

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
import bombardier.Filesystem, bombardier.Exceptions
import bombardier.Config, bombardier.OperatingSystem
import bombardier.Server
import bombardier.Repository, bombardier.BombardierClass
from bombardier.miniUtility import getProgressPath


UNINSTALL = 0
CONFIGURE = 1
INSTALL   = 2
VERIFY    = 3
RECONCILE = 4
STATUS    = 5
EXECUTE   = 6
FIX       = 7
PURGE     = 8

actionDict = { UNINSTALL:'uninstall', CONFIGURE:'configure', 
               INSTALL:'install', VERIFY:'verify', 
               RECONCILE:'reconcile', STATUS:'status', 
               EXECUTE:'execute', FIX:'fix', PURGE:'purge' } 


def findLikelyPackageName(packageName):
    statusYml = yaml.load(open('status.yml').read())
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

def fixSpkg(packageName, action):
    statusData = open(getProgressPath(), 'r').read()
    status = yaml.load(statusData)
    if status.get("install-progress") == None:
        logger.error( "Status file is empty." )
        return FAIL
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
        if len(fixName) != 1:
            logger.error("Could not find package %s. (possible %s)" % (packageName, ' '.join(fixName)))
            return FAIL
        packageName = fixName[0]
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
    open(getProgressPath(), 'w').write(yaml.dump(status))
    return OK

def getBc():
    import bombardier.Filesystem, bombardier.Exceptions
    filesystem = bombardier.Filesystem.Filesystem()
    filesystem.clearLock()
    server = bombardier.Server.Server(filesystem)
    config = bombardier.Config.Config(filesystem, server, bombardier.OperatingSystem.OperatingSystem())
    try:
        config.freshen()
    except bombardier.Exceptions.ServerUnavailable, e:
        logger.error( "Cannot connect to server (%s)" % e )
        sys.exit(1)
    if sys.platform == "linux2":
        import bombardier.Linux
        operatingSystem = bombardier.Linux.Linux()
    else:
        import bombardier.Windows
        operatingSystem = bombardier.Windows.Windows()

    repository = bombardier.Repository.Repository(config, filesystem, server)
    repository.getPackageData()
    bc = bombardier.BombardierClass.Bombardier(repository, config,
                                               filesystem, server,
                                               operatingSystem)
    return bc

def processAction(action, packageName, scriptName):
    if action in [ UNINSTALL, VERIFY, CONFIGURE, EXECUTE ]:
        packageName = findLikelyPackageName(packageName)         

    status = FAIL
    try:
        if action in [ FIX, PURGE ]:
            status = fixSpkg(packageName, action)
            return status
        bc = getBc()
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
        elif action == RECONCILE:
            status = bc.reconcileSystem()
        else:
            status = bc.usePackage(packageName, actionDict[action], scriptName)

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

    packageName = ""    
    scriptName  = ""
    
    parser = optparse.OptionParser("usage: %prog [option] [package-name]")

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

    (options, args) = parser.parse_args()
    if options.action not in [ RECONCILE, STATUS]:
        if len(args) < 1:
            print "This command requires a package name as an argument."
            parser.print_help()
            sys.exit( 1 )
        packageNames = args
        if options.action == EXECUTE:
            if len(args) != 2:
                print "This command requires a package name and a script name."
                parser.print_help()
                sys.exit( 1 )
            packageNames = args[:-1]
            scriptName = args[-1]

        for packageName in packageNames:
            status = processAction(options.action, packageName, scriptName)
            if status != OK:
                 sys.exit(status)
    else:
        status = processAction(options.action, packageName, scriptName)
    
    sys.exit(status)
