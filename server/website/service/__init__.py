import os, string
from webUtil import *

list = [{"name":"pkggroups"}, {"name":"package"}, {"name":"clientlog"},
        {"name":"vlan"}, {"name":"clientstatus"}]

default = "pkggroups"

def readScratch(filename = None, location = None):
    if validateFilename(filename) == FAIL:
        return FAIL, []
    if not os.path.isdir(location):
        return FAIL, []
    filePath = os.path.join(location, filename)
    if not os.path.isfile(filePath):
        return FAIL, []
    data = open(filePath, 'r').readlines()
    return OK, map(string.strip, data)


def validateFilename(filename):
##     if filename.rfind(os.path.sep) != -1:
##         return FAIL
    if filename.rfind('..') != -1:
        return FAIL
