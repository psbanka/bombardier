#!/cygdrive/c/Python24/python.exe

# Server.py: This module is used primarily to abstract all
# functionality that is needed to interact with an http server. It is
# written in a similar vein to Filesystem.py, so that it can be
# mocked and unit-tests simplified.

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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

import os, md5, yaml, getpass, base64

import Exceptions, miniUtility, Logger
from staticData import *

def computeMd5(filename, checksum):
    if not checksum:
        return OK, ''
    mchk = md5.new()
    fileHandle = open(filename, 'rb')
    data = fileHandle.read(BLOCK_SIZE)
    while data:
        mchk.update(data)
        data = fileHandle.read(BLOCK_SIZE)
    fileHandle.flush()
    fileHandle.close()
    del fileHandle # necessary because the file may need to be moved before gc. -pbanka
    computedMd5 = mchk.hexdigest()
    if computedMd5 != checksum:
        return FAIL, computedMd5
    return OK, computedMd5

class Server:

    """The purpose of this class is to provide an abstraction layer
    around all those activities that require interaction with a webserver.
    Probably the whole webops module aught to be refactored into this
    class."""

    def __init__(self, filesystem=None, serverData = None, password = None):
        self.filesystem = filesystem
        self.serverData = serverData
        self.cache      = {}
        self.username   = getpass.getuser()
        self.password   = password

    def clearCache(self):
        self.cache = {}

    def getServerData(self):
        serverDataPath = os.path.join(miniUtility.getSpkgPath(), SERVERDATA_FILE)
        try:
            serverDataDirectory = self.filesystem.loadYaml(serverDataPath)
        except:
            Logger.error("The %s file is corrupt" % serverDataPath)
            return
        if not serverDataDirectory:
            Logger.error("The %s file is empty" % serverDataPath)
            return
        serverName = serverDataDirectory.keys()[0]
        #Logger.info("Using server %s because it is the only one" % serverName)
        self.serverData = serverDataDirectory[serverName]
        return

    def packageRequest(self, filename):
        dosPath = miniUtility.getPackagePath()
        cygPath = miniUtility.cygpath(dosPath)
        Logger.info("==REQUEST-PACKAGE==:%s:%s" % (filename, cygPath))
        response = sys.stdin.read(3)
        if response.strip() != "OK":
            Logger.error("Received an invalid response from the server")
            raise Exceptions.FileNotFound(filename, "server told us that it didn't have our file.")
        filepath = dosPath +'/'+filename
        if os.path.isfile(filepath):
            return filepath
        raise Exceptions.FileNotFound(filepath, "did not receive from the server")

    def configRequest( self ):
        config = self.dataRequest("==REQUEST-CONFIG==")
        return config

    def bomRequest( self, bomName ):
        bom = self.dataRequest("==REQUEST-BOM==:%s" % bomName)
        return bom

    def dataRequest(self, requestString):
        Logger.info(requestString)
        STREAM_BLOCK_SIZE= 77
        b64Data = []
        while True:
            chunk = sys.stdin.read(STREAM_BLOCK_SIZE)
            if chunk[0] == ' ':
                break
            b64Data.append(chunk)
        yamlData = ''
        yamlData = base64.decodestring(''.join(b64Data))
        Logger.debug("Received %s lines of yaml" % len(yamlData.split('\n')))

        if 1 == 1:
        #try:
            config = yaml.load(yamlData)
            if type(config) == type("string"):
                Logger.error("Invalid Yaml on server: %s" % config)
                raise Exceptions.ServerUnavailable, ("config", "invalid yaml")
            if type(config) != type({}) and type(config) != type([]): # backwards comptible yaml
                config = config.next()
            return config
        else:
        #except:
            ermsg = "Received bad YAML: %s" % (repr(yamlData))
            raise Exceptions.ServerUnavailable, ("config", ermsg)
