#!/cygdrive/c/Python23/python.exe

# This class is for convenience sake for package files

from utility import *
from serviceUtil import *
from staticData import *
from Config import getSpkgPath, getPythonPath, getPackagePath, getBombardierPath, getProgressPath, getBomPath, getIpAddress, Config
import Logger
logger = Logger.Logger()
config = Config(logger)
config.freshen()
import os, sys

def getHostname():
    return os.environ["COMPUTERNAME"]

def getPythonPath():
    return sys.prefix

def getRepsoitory():
    return config.repository["address"]
