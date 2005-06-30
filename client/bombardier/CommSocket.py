#!/cygdrive/c/Python23/python.exe
import socket, random, time
from staticData import *
import Exceptions

IP = "127.0.0.1"

class CommSocket:

    def __init__(self):
        tries = 0
        while tries < 4:
            port = random.randint(1024,65535)
            self.sockObj = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            self.address = (IP, port)
            try:
                self.sockObj.bind(self.address)
                self.sockObj.setblocking(False)
                break
            except socket.error, e:
                print "cannot bind to %s / %s" % (str(self.address),  str(e))
                tries += 1

    def sendMessage(self, message):
        tmpsockObj = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        tmpport    = random.randint(1024, 65535)
        tmpsockObj.bind((IP, tmpport))
        tmpsockObj.sendto(message, self.address)

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
                raise Exceptions.QuitException
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
    s = CommSocket()
    s.sendMessage("hello")
    print s.getMessage()
