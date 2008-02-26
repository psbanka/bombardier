#!/cygdrive/c/Python24/python.exe

import socket, random, time, sys, yaml
from select import select
from libCipher import encrypt, decryptString, pad
from staticData import *

class QuitException(Exception):
    pass

class UnexpectedServerDisconnectException(Exception):
    pass

CHALLENGE_SIZE = 10
IP = "127.0.0.1"
A_TO_Z = ''.join([ chr(x) for x in range(ord('a'), ord('z')) ] )
START_CONST = "HELLO"
END_CONST   = "DONE"
END_RESPONSE = "***"
FIELD_DELIMITER = ":::"
MESSAGE_DELIMITER = "|||"
CLIENT_TIMEOUT = 10

DIZEBUG=0

def debugger(msg):
    if DIZEBUG:
        print msg

class SecureServer:
    def __init__(self, port, password):
        self.sockObj = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.address = (IP, port)
        self.password = password
        self.sockObj.bind(self.address)
        self.sockObj.listen(1)
        self.clientSockets = {}

    def getSecure(self):
        EMPTY_RETURN =  '', [], None

        readable, writable, errored = select([self.sockObj], [], [], 1)
        if not readable:
            return EMPTY_RETURN
        clientSocket, clientAddressTuple = self.sockObj.accept()        
        clientAddress = str(clientAddressTuple)
        self.clientSockets[clientAddress] = clientSocket
        data = clientSocket.recv(1024)

        if data != START_CONST:
            debugger( "improper start sequence: %s" % data )
            return EMPTY_RETURN

        randomString = ''.join(random.sample(A_TO_Z, CHALLENGE_SIZE))
        expectedCipherText = encrypt(randomString, self.password)
        debugger( "sending a message back to %s" %str(clientAddress ))
        clientSocket.send(randomString)
        response = ''
        while not response.endswith(END_RESPONSE):
            debugger( "waiting for a response..." )
            try:
                response += clientSocket.recv(1024)
            except:
                debugger( "Client disconnected" )
                return EMPTY_RETURN
            #time.sleep(1)
        debugger( "response to our challenge: %s" %response )
        response = response[:-3]
        if FIELD_DELIMITER in response:
            cipherText, command, message = response.split(FIELD_DELIMITER)
            if cipherText != expectedCipherText:
                debugger( "Dude screwed up the password" )
                return EMPTY_RETURN
            return command, message.split(MESSAGE_DELIMITER), clientAddress

    def cleanup(self):
        for clientAddress in self.clientSockets:
            self.disconnectClient(clientAddress)

    def disconnectClient(self, clientAddress):
        if not clientAddress:
            return
        clientSocket = self.clientSockets[clientAddress]
        clientSocket.send(END_CONST+MESSAGE_DELIMITER)
        now = time.time()
        while True:
            debugger("Waiting for graceful disconnect")
            readable, writable, errored = select([clientSocket], [], [], 1)
            if readable:
                clientData = readable[0].recv(1024)
                if clientData != '':
                    debugger("Client still thinks it wants to talk to us: %s" % clientData)
                else:
                    break
            if time.time() - now > CLIENT_TIMEOUT:
                debugger("UNGracefully disconnecting client")
                clientSocket.close()
                break
        debugger( "Disconnected client: %s" %clientAddress )
        
    def sendClient(self, clientAddress, object):
        message = yaml.dump(object)
        encMessage = encrypt(message, self.password)
        debugger("ENC: " + encMessage )
        self.clientSockets[clientAddress].send(encMessage + MESSAGE_DELIMITER)

class SecureClient:
    def __init__(self, port, password):
        self.port = port
        self.password = password

    def sendSecure(self, command, messageList):
        tmpsockObj = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        tmpsockObj.connect((IP, self.port))
        tmpsockObj.send(START_CONST)
        data = tmpsockObj.recv(1024)
        debugger( data )
        cipherText = encrypt(data, self.password)
        message = MESSAGE_DELIMITER.join([ str(m) for m in messageList ])
        fullMessage = FIELD_DELIMITER.join([cipherText, command, message]) + END_RESPONSE
        tmpsockObj.send(fullMessage)
        response = [] 
        while True:
            data = tmpsockObj.recv(1024)
            if not data:
                raise UnexpectedServerDisconnectException
            debugger( "DATER::: %s"%data )
            while MESSAGE_DELIMITER in data:
                response.append(data.split( MESSAGE_DELIMITER )[0])
                data = MESSAGE_DELIMITER.join(data.split( MESSAGE_DELIMITER )[1:])
                debugger( "RESPONSE: %s"%response )
                debugger( "DATA    : %s"%data )
            
            if response[-1] == END_CONST:
                return [ yaml.load(decryptString(r, self.password, YAML_CHARS)) for r in response[:-1] ]

if __name__ == "__main__":
    pass
