class ExceptionBase(Exception):
    def __init__(self):
        Exception.__init__(self)
    def __str__(self):
        return self.__repr__()

class CnmServerException(ExceptionBase):
    def __init__(self, msg):
        ExceptionBase.__init__(self)
        self.msg = msg
    def __repr__(self):
        return "Error from CNM server: %s" % self.msg

class MachineStatusException(ExceptionBase):
    def __init__(self, msg):
        ExceptionBase.__init__(self)
        self.msg = msg
    def __repr__(self):
        return "Error in status data: %s" % self.msg

class InvalidAction(Exception):
    def __init__(self, package_name, action_name):
        Exception.__init__(self)
        self.package_name = package_name
        self.action_name = action_name
    def __repr__(self):
        msg = "Cannot run action %s on package %s"
        return msg % (self.action, self.package_name)
    def __str__(self):
        return self.__repr__()

class QueuedJob(ExceptionBase):
    def __init__(self, job_name):
        ExceptionBase.__init__(self)
        self.job_name = job_name
    def __repr__(self):
        return self.job_name

class CannotQueueJob(ExceptionBase):
    def __init__(self, reason):
        ExceptionBase.__init__(self)
        self.reason = reason
    def __repr__(self):
        return self.reason

class DispatcherAlreadyStarted(ExceptionBase):
    def __repr__(self):
        return ""

class DispatcherError(ExceptionBase):
    def __init__(self, explanation):
        ExceptionBase.__init__(self)
        self.explanation = explanation
    def __repr__(self):
        return "Dispatcher error: %s" % self.explanation

class InvalidDispatcherAction(ExceptionBase):
    def __init__(self, action_name):
        ExceptionBase.__init__(self)
        self.action_name = action_name
    def __repr__(self):
        return "Invalid action: %s" % self.action
    def __str__(self):
        return self.__repr__()

class JobAlreadySet(Exception):
    def __init__(self, job_name):
        Exception.__init__(self)
        self.job_name = job_name
    def __repr__(self):
        return "Another job is already running: %s" % self.job_name
    def __str__(self):
        return self.__repr__()

class EnableRequiredException(Exception):
    def __init__(self):
        Exception.__init__(self)
    def __repr__(self):
        return "Must be in enable mode to connect to this server"
    def __str__(self):
        return self.__repr__()

class IncompleteConfigurationException(Exception):
    def __init__(self, server, errmsg):
        Exception.__init__(self)
        self.server = server
        self.errmsg = errmsg
    def __repr__(self):
        msg = "Server configuration for %s is incomplete (%s)"
        return msg % (self.server, self.errmsg)
    def __str__(self):
        return self.__repr__()

class BombardierMachineException(Exception):
    def __init__(self):
        Exception.__init__(self)
    def __repr__(self):
        return "Error running bc.py"
    def __str__(self):
        return self.__repr__()

class MachineUnavailableException(Exception):
    def __init__(self, server, errmsg):
        Exception.__init__(self)
        self.server = server
        self.errmsg = errmsg
    def __repr__(self):
        return "Unable to connect to %s (%s)" % (self.server, self.errmsg)
    def __str__(self):
        return self.__repr__()

class SecureCopyException(Exception):
    def __init__(self, source, dest, errmsg):
        Exception.__init__(self)
        self.source = source
        self.dest = dest
        self.errmsg = errmsg
    def __repr__(self):
        msg = "Unable to copy (%s) to (%s): %s"
        msg = msg % (self.source, self.dest, self.errmsg)
        return msg
    def __str__(self):
        return self.__repr__()

class InvalidInput(Exception):
    def __init__(self, bad_characters):
        Exception.__init__(self)
        self.bad_characters = bad_characters
    def __repr__(self):
        return "Invalid input: %s" % (self.bad_characters)
    def __str__(self):
        return self.__repr__()

class JoinTimeout(Exception):
    def __init__(self, job_name, timeout):
        Exception.__init__(self)
        self.job_name = job_name
        self.timeout = timeout
    def __repr__(self):
        return "Timed out joining a job %s (%s)" % (self.job_name, self.timeout)
    def __str__(self):
        return self.__repr__()

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
        self.server = server
        self.message = message
    def __repr__(self):
        msg = "Could not find valid configuration data for %s (%s)" 
        return msg % (self.server, self.message)
    def __str__(self):
        return self.__repr__()

class PackageNotFound(Exception):
    def __init__(self, package_name, path=''):
        Exception.__init__(self)
        self.package_name = package_name
        self.path = path
    def __repr__(self):
        return "Package not found: %s (%s)" % (self.package_name, self.path)
    def __str__(self):
        return self.__repr__()

class DispatcherOffline(Exception):
    def __init__(self):
        Exception.__init__(self)
    def __repr__(self):
        return "Dispatcher offline"
    def __str__(self):
        return self.__repr__()
