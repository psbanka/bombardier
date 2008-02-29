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

    def processCommand(self, command, fpOut=sys.stdout, fpErr=sys.stderr):
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
                    self.fpOut = fpOut
                    status, output = self.run(tokens, noFlag, self)
                    self.fpOut = ''
                    del self.fpOut
                    if comment: 
                        makeComment(comment)
            libUi.userOutput(output, status, fpOut, fpErr)
            return status, output
        except exceptions.SystemExit, e:
            if mode.commentCommands:
                makeComment()
            sys.exit(0)
        except Exception, e:
            fpErr.write( "Error detected in %s (%s)." % (file, e))
            e = StringIO.StringIO()
            traceback.print_exc(file=e)
            e.seek(0)
            data = e.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            fpErr.write(ermsg)
            fpErr.write("\n")
        return FAIL,["processCommand Excepted"]


##########################################################################
## Adding new commands:
## 
## 0. Create a class that extends PinshCmd
## 1. Import the class.
## 2. add the class to the following vector of command or add it to
##    the Configure.py class
    
import Bash, Ping, Telnet, Ssh, Terminal, Exit, Package, Enable, Show, Sleep
import For, Echo, UpdateClient, Set, Run, PushConfig, Edit, Debug, Scheduler

commands = [Echo.Comment(), Set.Set(), Terminal.Terminal(), Show.Show(), Ping.Ping(), Debug.Debug(), Scheduler.Scheduler(),
            Telnet.Telnet(), Ssh.Ssh(), Bash.Bash(), Run.Run(), PushConfig.PushConfig(), Edit.Edit(), Echo.Pause(), Sleep.Sleep(),
            UpdateClient.UpdateClient(), Exit.Exit(), Enable.Enable(), For.For(), Echo.Echo(), Package.Package()]
slash = Slash(commands)


