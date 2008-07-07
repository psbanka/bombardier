#!/opt/python2.5/bin/python

import traceback, StringIO
import exceptions
import sys
import PinshCmd, Mode, libUi
from commonUtil import *

class Slash(PinshCmd.PinshCmd):
    def __init__(self, children):
        PinshCmd.PinshCmd.__init__(self, "")
        self.children = children
        self.cmdOwner = 1
        self.helpText = ''
        self.fpOut=sys.stdout
        self.fpErr=sys.stderr
    def setOutput(self, fpOut):
        self.fpOut = fpOut

    def setErr(self, fpErr):
        self.fpErr = fpErr

    def processCommand(self, command):
        try:
            if mode.currentState() != Mode.FREE_TEXT:
                noFlag, helpFlag, tokens, comment = libUi.processInput(command)
                if helpFlag: # Process the [?] key first
                    self.findHelp(tokens, 0)
                    #mode.reprompt()
                    return None
                if len(tokens) == 0:
                    return OK, []
                else:
                    status, output = self.run(tokens, noFlag, self)
                    if comment: 
                        makeComment(comment)
            libUi.userOutput(output, status, self.fpOut, self.fpErr)
            return status, output
        except exceptions.SystemExit, e:
            if mode.commentCommands:
                makeComment()
            sys.exit(0)
        except Exception, e:
            self.fpErr.write( "Error detected in %s (%s)." % (command, e))
            e = StringIO.StringIO()
            traceback.print_exc(file=e)
            e.seek(0)
            data = e.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            self.fpErr.write(ermsg)
            self.fpErr.write("\n")
        return FAIL,["processCommand Excepted"]


##########################################################################
## Adding new commands:
## 
## 0. Create a class that extends PinshCmd
## 1. Import the class.
## 2. add the class to the following vector of command or add it to
##    the Configure.py class
    
import Bash, Ping, Telnet, Ssh, Rsync, Terminal, Exit, Package, Enable, Show, Sleep
import For, Echo, UpdateClient, Set, Run, Push, Edit, Debug, Scheduler, Cmdb

commands = [Echo.Comment(), Set.Set(), Terminal.Terminal(), Show.Show(), Ping.Ping(), 
            Debug.Debug(), Scheduler.Scheduler(), Telnet.Telnet(), Ssh.Ssh(), Bash.Bash(), 
            Run.Run(), Push.Push(), Edit.Edit(), Echo.Pause(), Sleep.Sleep(),
            UpdateClient.UpdateClient(), Exit.Exit(), Enable.Enable(), For.For(), 
            Echo.Echo(), Package.Package(), Rsync.Rsync(), Cmdb.Cmdb()]
slash = Slash(commands)


