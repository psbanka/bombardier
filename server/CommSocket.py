#!/cygdrive/c/Python24/python.exe

# CommSocket.py: This little class provides a mechanism for threads to
# communicate between each other using UDP sockets.

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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

import socket, random, time
from staticData import *

class QuitException(Exception):
    pass

GO = "1"
STOP = "0"
CHALLENGE_SIZE = 10

IP = "127.0.0.1"

class CommSocket:

    def __init__(self, port=None):
        if not port:
            port = random.randint(1024,65535)
        self.sockObj = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.address = (IP, port)
        self.sockObj.bind(self.address)
        self.sockObj.setblocking(False)

    def write(self, message):
        self.sendMessage(message.replace('\n', COMMAND_LOG_MARKER))

    def flush(self):
        pass

    def sendMessage(self, message):
        tmpsockObj = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        tmpport    = random.randint(1024, 65535)
        tmpsockObj.bind((IP, tmpport))
        tmpsockObj.sendto(message, self.address)

    def read(self):
        output = ''
        strikes = 0
        while strikes < 4:
            data = self.getMessage()
            if data:
                output += data
            else:
                strikes += 1
                time.sleep(.2)
        return output

    def getMessage(self):
        try:
            data, clientAddress = self.sockObj.recvfrom(1024)
        except:
            return ''
        return data

    def unPause(self):
        self.sendMessage(GO)

    def paused(self):
        message = self.getMessage()
        if message != GO:
            if message == STOP:
                raise QuitException
            return True
        return False

    def waitForMessage(self):
        while True:
            message = self.getMessage()
            if message:
                return message
            time.sleep(0.1)
        return ''

    def sendStop(self):
        self.sendMessage(STOP)

    def testStop(self):
        if self.getMessage() == STOP:
            return True
        return False

if __name__ == "__main__":
    from libTest import startTest, runTest, endTest
    from staticData import OK
    s = CommSocket(1500)
    status = OK
    startTest()
    s.sendMessage("hello")
    status = runTest(s.getMessage, [], "hello", status)
    s.sendStop()
    status = runTest(s.testStop, [], True, status)
    endTest(status)
