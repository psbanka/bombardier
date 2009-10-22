#!/cygdrive/c/Python23/python.exe

from _version import version_info

CLIENT_VERSION = str("%(branch_nick)s-%(revno)d" % version_info)


def getSpkgPath():
    import _winreg as winreg
    keyName = r"Software\GE-IT\Bombardier"
    key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                         keyName, 0, winreg.KEY_QUERY_VALUE)
    spkgPath, dummy = winreg.QueryValueEx(key, "InstallPath")
    return spkgPath



