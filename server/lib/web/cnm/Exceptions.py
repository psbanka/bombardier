class InvalidJobName(Exception):
    def __init__(self, job_name="NO_JOB_NAME"):
        Exception.__init__(self)
        self.job_name = job_name
    def __repr__(self):
        return "Requesting a job that doesn't exist: %s" % self.job_name
    def __str__(self):
        return self.__repr__()

class InvalidServerHome(Exception):
    def __init__(self, server_home):
        Exception.__init__(self)
        self.server_home = server_home
    def __repr__(self):
        return "Server home value is not valid: %s" % self.server_home
    def __str__(self):
        return self.__repr__()

class MachineConfigurationException(Exception):
    def __init__(self, server, message=''):
        e = Exception()
        Exception.__init__(e)
        self.server = server
        self.message = message
    def __str__(self):
        return "Could not find valid configuration data for %s (%s)" % (self.server, self.message)
    def __repr__(self):
        return "Could not find valid configuration data for %s (%s)" % (self.server, self.message)

