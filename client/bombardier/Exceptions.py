#!/cygdrive/c/Python23/python.exe

class BadPackage(Exception):
    def __init__(self, packageName, errmsg):
        self.errmsg      = errmsg
        self.packageName = packageName
    def __repr__(self):
        return "%s: %s" % (self.packageName, self.errmsg)
    def __str__(self):
        return "%s: %s" % (self.packageName, self.errmsg)

class NoYamlData(Exception):
    def __init__(self, filename):
        self.filename = filename
    def __str__(self):
        return self.filename

class ServiceNotFound(Exception):
    def __init__(self, serviceName):
        self.serviceName = serviceName
    def __str__(self):
        return self.serviceName

class ServiceShutdown(Exception):
    pass

class ServerUnavailable(Exception):
    def __init__(self, url, errmsg):
        self.errmsg = errmsg
        self.url    = url
    def __str__(self):
        return "%s: %s" % (self.url, self.errmsg)

class PipeNotListenable(Exception):
    def __init__(self, pipeName):
        self.pipeName = pipeName
    def __str__(self):
        return self.pipeName
    
class StoppedExecution(Exception):
    pass

class MissingComponent(Exception):
    def __init__(self, name):
        self.name = name
    def __str__(self):
        return self.name
    
