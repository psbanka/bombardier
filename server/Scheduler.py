#!/usr/bin/python

import sys, pexpect
import PinshCmd, SecureCommSocket
from commonUtil import *

def sendCommand(command):
    c = SecureCommSocket.SecureClient(TB_CTRL_PORT, mode.password)
    try:
        c.sendSecure(command, [])
    except:
        return FAIL, ["Command failed"]
    return OK, ["Command complete"]

class Scheduler(PinshCmd.PinshCmd):
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "scheduler")
        self.helpText = "scheduler\tbombardier scheduler control"
        save  = PinshCmd.PinshCmd("save", "save\tsave the current configuration to disk")
        load  = PinshCmd.PinshCmd("load", "load\tload the saved configuration from disk")
        start = PinshCmd.PinshCmd("start", "start\tstart the bombardier job scheduler")
        stop  = PinshCmd.PinshCmd("stop", "stop\tstop the bombardier job scheduler")
        self.children = [save, load, start, stop]
        self.auth = ADMIN
        self.level = 0
        self.cmdOwner = 1

    def cmd(self, tokens, noFlag, slash):
        pyChucker(slash)
        if noFlag:
            return FAIL, []
        if len(tokens) < 2:
            return FAIL, ["Incomplete command."]

        command = tokens[1]
        if command.startswith('sa'):
            return sendCommand(TB_SAVE)
        elif command.startswith("l"):
            return sendCommand(TB_LOAD)
        elif command.startswith('sto'):
            return sendCommand(TB_KILL)
        elif command.startswith('sta'):
            status = OK
            s = pexpect.spawn("%s TimeBom.py" % sys.executable, timeout=5)
            i = s.expect([pexpect.TIMEOUT, '[pP]assword:', 'socket.error:', pexpect.EOF], timeout=5)
            if i == 0:
                status = FAIL
                message = "Timed out trying to start server"
            elif i == 1:
                status = OK
                s.sendline(mode.password)
                message = "Started server."
            elif i == 2:
                status = FAIL
                message = "Address already in use."
            else:
                status = FAIL
                message = "Unable to start (%s)" % s.after
            s.expect(pexpect.EOF, timeout=1)
            s.close()
            if status == OK:
                return sendCommand(TB_LOAD)
            return status, [message]
            #os.system("%s TimeBom.py" % sys.executable)            
        else:
            return FAIL, ["Unrecognized command: %s" % command] 

