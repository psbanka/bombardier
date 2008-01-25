#!/opt/python2.5/bin/python
from commonUtil import *
import yaml, sys, random
from commands import getstatusoutput
from UserAuth import UserAuth

SYSTEM_INFO = "deploy/include/systemInfo.yml"
#SYSTEM_INFO = "systemInfo.yml"

def showAllRights():
    userData = yaml.load(open("deploy/include/systemInfo.yml").read())
    print yaml.dump(userData["system"]["rights"], default_flow_style=False)

if __name__ == "__main__":
    import optparse
    parser = optparse.OptionParser("usage: %prog user-name [options] [RIGHT1] <RIGHT2> ...")
    parser.add_option("-l", "--list", dest="list",
                      action="store_true", default=False,
                      help="list the user's rights or all possible rights if no user is specified.")
    parser.add_option("-c", "--comment", dest="comment", default='',
                      help="Insert a comment about the user", metavar="COMMENT")
    parser.add_option("-d", "--del", dest="delete",
                      action="store_true", default=False,
                      help="delete the user's rights")

    (options, args) = parser.parse_args()
    if not args:
        if not options.list:
            print "ERROR: Must specify the name of a user"
            parser.print_help()
            sys.exit(FAIL)
        else:
            showAllRights()
            sys.exit(OK)
    if options.delete:
        print "Delete not implemented yet."
        sys.exit(FAIL)
    if len(args) < 2:
        print "ERROR: Must specify at least one right"
        parser.print_help()
        sys.exit(FAIL)
    if options.list and options.delete:
        print "ERROR: List and delete are mutually exclusive"
        parser.print_help()
        sys.exit(FAIL)

    userName   = args[0]
    rightsList = args[1:]
    user = UserAuth(userName, rightsList, options.comment, SYSTEM_INFO)
    user.modifySystemInfo()
    user.createVpnCert()
    user.createPwsafe()
    user.prepareWebData()
    user.bombardierUpdate()

    #^ make system name configurable as well as destination location
