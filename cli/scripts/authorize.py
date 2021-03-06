#!/opt/python2.5/bin/python
from commonUtil import *
import yaml, sys
from commands import getstatusoutput
from UserAuth import UserAuth

SYSTEM_INFO = "serverConfig.yml"
SERVER_CONFIG = "/etc/serverConfig.yml"
#SYSTEM_INFO = "systemInfo.yml"

def showAllRights(userName):
    systemData = yaml.load(open(SYSTEM_INFO).read())
    if not userName:
        print yaml.dump(systemData["system"]["rights"], default_flow_style=False)
    else:
        userData = systemData["system"]["users"].get(userName)
        if userData:
            print "rights for %s: %s" % (userName, yaml.dump(userData.get("rights")))
        else:
            print "user %s does not have any rights." % userName

if __name__ == "__main__":
    import optparse
    parser = optparse.OptionParser("usage: %prog user-name [options] [RIGHT1] <RIGHT2> ...")
    parser.add_option("-l", "--list", dest="list",
                      action="store_true", default=False,
                      help="list the user's rights or all possible rights if no user is specified.")
    parser.add_option("-c", "--comment", dest="comment", default='',
                      help="Insert a comment about the user", metavar="COMMENT")
    parser.add_option("-v", "--vpn-only", dest="vpnOnly",
                      action="store_true", default=False,
                      help="Set the user up for VPN access only")
    parser.add_option("-d", "--del", dest="delete",
                      action="store_true", default=False,
                      help="delete the user's rights")
    parser.add_option("-y", "--yes", dest="autoConfirm",
                      action="store_true", default=False,
                      help="Answer yes to all questions")

    (options, args) = parser.parse_args()
    rightsList = []
    if options.list:
        if len(args) > 0:
            userName = args[0].lower()
        else:
            userName = ''
        showAllRights(userName)
        sys.exit(OK)
    if not args:
        print "ERROR: Must specify the name of a user"
        parser.print_help()
        sys.exit(FAIL)
    if options.delete:
        rightsList = []
    else:
        if len(args) < 2 and not options.vpnOnly:
            print "ERROR: Must specify at least one right"
            parser.print_help()
            sys.exit(FAIL)
        rightsList = args[1:]
    if options.list and options.delete:
        print "ERROR: List and delete are mutually exclusive"
        parser.print_help()
        sys.exit(FAIL)

    userName   = args[0].lower()
    user = UserAuth(userName, rightsList, options.comment, SYSTEM_INFO, 
                    SERVER_CONFIG, options.autoConfirm)
    if not options.vpnOnly:
        print "Modifying systemInfo.yml..."
        user.modifySystemInfo()
    if options.delete:
        print "Cleaning out web directory for this user..."
        user.removeCredentials()
    else:
        print "Creating certificate..."
        user.createVpnCert()
        print "Creating password safe..."
        user.createPwsafe()
        print "Making data downloadable..."
        user.prepareWebData()
    if not options.vpnOnly:
        print "Pushing configuration out to servers..."
        user.bombardierUpdate()
