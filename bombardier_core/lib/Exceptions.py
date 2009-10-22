class PipeNotListenable(Exception):
    def __init__(self, pipeName):
        e = Exception()
        Exception.__init__(e)
        self.pipeName = pipeName
    def __str__(self):
        return self.pipeName
    
class ServiceShutdown(Exception):
    pass

class ServiceNotFound(Exception):
    def __init__(self, serviceName):
        e = Exception()
        Exception.__init__(e)
        self.serviceName = serviceName
    def __str__(self):
        return self.serviceName

class InvalidProgress(Exception):
    def __init__(self, progressData):
        e = Exception()
        Exception.__init__(e)
        self.badData = progressData
    def __str__(self):
        return "Invalid progress: %s" % self.badData
    def __repr__(self):
        return "Invalid progress: %s" % self.badData
    
class StatusException(Exception):
    def __init__(self, yamlFile):
        e = Exception()
        Exception.__init__(e)
        self.yamlFile  = yamlFile
    def __repr__(self):
        return "Bad YAML data in file %s" % (self.yamlFile)
    def __str__(self):
        return "Bad YAML data in file %s" % (self.yamlFile)

class NoYamlData(Exception):
    def __init__(self, filename):
        e = Exception()
        Exception.__init__(e)
        self.filename = filename
    def __str__(self):
        return self.filename


