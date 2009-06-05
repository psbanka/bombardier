#!/cygdrive/c/Python24/python.exe

# CommSocket.py: This little class provides a mechanism for threads to
# communicate between each other using UDP sockets.

# Copyright (C) 2005 Peter Banka

import socket, random, time
from bombardier_core.static_data import GO, STOP, COMMAND_LOG_MARKER

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
        self.sock_obj = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.address = (IP, port)
        self.sock_obj.bind(self.address)
        self.sock_obj.setblocking(False)

    def write(self, message):
        self.send_message(message.replace('\n', COMMAND_LOG_MARKER))

    def flush(self):
        pass

    def send_message(self, message):
        tmpsockObj = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        tmpport    = random.randint(1024, 65535)
        tmpsockObj.bind((IP, tmpport))
        tmpsockObj.sendto(message, self.address)

    def read(self):
        output = ''
        strikes = 0
        while strikes < 4:
            data = self.get_message()
            if data:
                output += data
            else:
                strikes += 1
                time.sleep(.2)
        return output

    def get_message(self):
        try:
            data, client_address = self.sock_obj.recvfrom(1024)
        except:
            return ''
        return data

    def un_pause(self):
        self.send_message(GO)

    def paused(self):
        message = self.get_message()
        if message != GO:
            if message == STOP:
                raise QuitException
            return True
        return False

    def wait_for_message(self):
        while True:
            message = self.get_message()
            if message:
                return message
            time.sleep(0.1)
        return ''

    def send_stop(self):
        self.send_message(STOP)

    def test_stop(self):
        if self.get_message() == STOP:
            return True
        return False

if __name__ == "__main__":
    from libTest import startTest, runTest, endTest
    s = CommSocket(1500)
    status = OK
    startTest()
    s.send_message("hello")
    status = runTest(s.get_message, [], "hello", status)
    s.send_stop()
    status = runTest(s.test_stop, [], True, status)
    endTest(status)
