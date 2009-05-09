#!/cygdrive/c/Python23/python.exe

#from old_static_data import *

def getSpkgPath():
    import _winreg as winreg
    keyName = r"Software\GE-IT\Bombardier"
    key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                         keyName, 0, winreg.KEY_QUERY_VALUE)
    spkgPath, dummy = winreg.QueryValueEx(key, "InstallPath")
    return spkgPath



