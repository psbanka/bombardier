#!/cygdrive/c/Python25/python.exe

import os, gc, sys

if sys.platform != 'linux2':
    import _winreg as winreg
else:
    import yaml

def setIniConfig(configData):
    fh = open("../server/deploy/config/%s.ini" % os.environ["COMPUTERNAME"], 'w')
    fh.write(configData)
    fh.flush()
    fh.close()
    del fh
    gc.collect()

def getLinuxConfig():
    data = open("/etc/bombardier.yml", 'r').read()
    config = yaml.load(data)
    return config

def putLinuxConfig(config):
    data = open("/etc/bombardier.yml", 'w')
    data.write(yaml.dump(config))

class Tcommon:

    def __init__(self):
        self.startPath = self.getSpkgPath()
    
    def getSpkgPath(self):
        spkgPath = ''
        if sys.platform == "linux2":
            config = getLinuxConfig()
            spkgPath = config.get("spkgPath")
        else:
            keyName = r"Software\GE-IT\Bombardier"
            key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                                 keyName, 0, winreg.KEY_QUERY_VALUE)
            spkgPath, type = winreg.QueryValueEx(key, "InstallPath")
        return spkgPath

    def setSpkgPath(self, value):
        if sys.platform == "linux2":
            config = getLinuxConfig()
            config["spkgPath"] = value
            putLinuxConfig(config)
        else:
            keyName = r"Software\GE-IT\Bombardier"
            key = winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE,
                                 keyName, 0, winreg.KEY_SET_VALUE)
            winreg.SetValueEx(key, "InstallPath", 0, winreg.REG_SZ, value)
            
    def setForTest(self):
        self.setSpkgPath(os.getcwd())

    def unsetForTest(self):
        self.setSpkgPath(self.startPath)

