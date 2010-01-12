class InvalidConfigData(Exception):
    def __init__(self, section, t1, t2):
        Exception.__init__(self)
        self.section = section
        self.t1      = t1
        self.t2      = t2
    def __repr__(self):
        msg = "Unable to read configuration data: %s. [expected %s, got %s]"
        return msg % (self.section, self.t1, self.t2)
    def __str__(self):
        return self.__repr__()

class StatusException(Exception):
    def __init__(self, path):
        Exception.__init__(self)
        self.path = path
    def __repr__(self):
        msg = "Error in status data located at %s"
        return msg % (self.path)
    def __str__(self):
        return self.__repr__()
