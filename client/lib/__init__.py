#!/cygdrive/c/Python23/python.exe

from _version import version_info

CLIENT_VERSION = str("%(branch_nick)s-%(revno)d" % version_info)
