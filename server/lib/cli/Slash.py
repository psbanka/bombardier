#!/opt/python2.5/bin/python

import traceback, StringIO
import exceptions
import sys
import PinshCmd, libUi
from bombardier_server.cli.SystemStateSingleton import SystemState, FREE_TEXT
system_state = SystemState()
from bombardier_core.static_data import FAIL,OK


class Slash(PinshCmd.PinshCmd):
    def __init__(self, children):
        PinshCmd.PinshCmd.__init__(self, "")
        self.children = children
        self.cmdOwner = 1
        self.helpText = ''
        self.fp_out=sys.stdout
        self.fp_err=sys.stderr

    def set_output(self, fp_out):
        self.fp_out = fp_out

    def set_err(self, fp_err):
        self.fp_err = fp_err

    def process_command(self, command):
        try:
            if system_state.current_state() != FREE_TEXT:
                no_flag, help_flag, tokens, comment = libUi.process_input(command)
                if help_flag: # Process the [?] key first
                    self.find_help(tokens, 0)
                    return None
                if len(tokens) == 0:
                    return OK, []
                else:
                    status, output = self.run(tokens, no_flag, self)
                    if comment:
                        makeComment(comment) # MISSING
            libUi.user_output(output, status, self.fp_out, self.fp_err)
            return status, output
        except exceptions.SystemExit, e:
            if system_state.comment_commands:
                makeComment() # MISSING
            sys.exit(0)
        except Exception, e:
            self.fp_err.write( " %%%% Error detected in %s (%s)." % (command, e))
            e = StringIO.StringIO()
            traceback.print_exc(file=e)
            e.seek(0)
            data = e.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            self.fp_err.write(ermsg)
            self.fp_err.write("\n")
        return FAIL,["process_command Excepted"]


##########################################################################
## Adding new commands:
##
## 0. Create a class that extends PinshCmd
## 1. Import the class.
## 2. add the class to the following vector of command or add it to
##    the Configure.py class

import Show

commands = [Show.Show()]

