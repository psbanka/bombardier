#!/usr/bin/env python

import sys, os, random, re

from CADir import CADir

class CACA:
    def __init__(self, home, data, base):
        self.home   = home
        self.data   = data
        self.base   = base
        self.infile = sys.stdin

    def __repr__(self):
        response =  self.handle_request()
        if response:
            out = "content-type: text/plain\n\n"
            out = out + response
            return out
        else:
            return ""

    def handle_request(self):
        if os.environ['REQUEST_METHOD'] == "GET":
            return self.handle_GET()

        elif os.environ['REQUEST_METHOD'] == "PUT":
            return self.handle_PUT()

        else:
            raise "UnkownRequestType"

    def handle_GET(self):
        fullPath = os.environ['REQUEST_URI']
        path = os.path.join("/", fullPath[len(self.base):])

        self.caInstanceName = path.split("/")[1]
        self.ca = CADir(os.path.join(self.data, self.caInstanceName)) 

        self.subCAPath = path[len(self.caInstanceName)+1:]

        if self.subCAPath == "/ca-cert.pem":
            dataPath = os.path.join(self.ca.directory, 
                                        self.subCAPath[1:])
            return open(dataPath).read()

        reqMatch = re.search(".req.(new|\d+?)/?(crt|csr|error)?$",
                             self.subCAPath)

        if self.subCAPath == "/":
            return self.index()

        if self.subCAPath == "/req/":
            return self.index_req()

        if reqMatch:
            return self.service_req(reqMatch)

        return "Unknown request %s" % fullPath

    def index(self):
        return """---
        services: 
          certificate request: { src: ./req/ }
        """

    def index_req(self):
        return """---
        upload request: { src: ./csr, note: relative to ticket directory }
        download certificate: { src: ./crt, note: relative to ticket directory }
        request ticket: { src: ./new }
        """



    def service_req(self, reqMatch): 
        ticket, ticketItem = reqMatch.groups()

        if ticket == 'new':
            ticket = random.randrange(0, 2140000000)
            os.mkdir(os.path.join(self.ca.directory, "req", `ticket`))
            return `ticket`
        else:
            if ticketItem == "csr":
                dataPath = os.path.join(self.ca.directory, 
                                        self.subCAPath[1:])
                if os.path.isfile(dataPath):
                    handle = open(dataPath)
                    return handle.read(10000)

            elif ticketItem == "crt":
                dataPath = os.path.join(self.ca.directory, 
                                        self.subCAPath[1:])
                dataDir = os.path.split(dataPath)[0]

                if os.path.isfile(dataPath):
                    handle = open(dataPath)
                    return handle.read(10000)

                elif os.path.isdir(dataDir):
                    csrPath = os.path.join(dataDir,
                                           "csr")
                    csr = open(csrPath).read()

                    if os.path.isfile(csrPath):
                        crt = self.ca.sign(csr)
                        if crt:
                            open(dataPath, 'w').write(crt)
                            return crt
        return "Unknown request: should be 'new', 'csr', 'crt'\n"

    def handle_PUT(self):
        fullPath = os.environ['REQUEST_URI']
        path = os.path.join("/", fullPath[len(self.base)-1:]) 

        self.caInstanceName = path.split("/")[1]
        self.ca = CADir(os.path.join(self.data, self.caInstanceName)) 

        self.subCAPath = path[len(self.caInstanceName)+1:]
        reqMatch = re.search(".req.(new|\d+?)/?(crt|csr|error)?$",
                                                    self.subCAPath)
        
        if reqMatch:
            ticket, ticketItem = reqMatch.groups()
            if ticketItem == 'csr':
                try:
                    reqFile = open(os.path.join(self.ca.directory, 
                                                self.subCAPath[1:]), 'w')
                    data = self.infile.read(10000)
                    reqFile.write(data)
                except Exception, e:
                    return "Error: %s\n" % e
        return "OK\n"
