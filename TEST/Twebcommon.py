#!/cygdrive/c/Python25/python.exe

import StringIO, sys, yaml, time

NOT_FINISHED = 1
FAIL = 1
OK   = 0

class Logger:
    def __init__(self):
        self.log = {}
        self.lognames = ["debug", "info", "warning", "error", "critical"]
        for logname in self.lognames:
            self.log[logname] = StringIO.StringIO()
    def debug(self, string):
        self.log["debug"].write(string)
    def info(self, string):
        self.log["info"].write(string)
    def warning(self, string):
        self.log["warning"].write(string)
    def error(self, string):
        self.log["error"].write(string)
    def critical(self, string):
        self.log["critical"].write(string)
    def dump(self):
        for logname in self.lognames:
            self.log[logname].seek(0)
            for line in self.log[logname].readlines():
                print "%s: %s" % (logname, line)
    def rmFileLogging(self):
        pass

class Request:
    def __init__(self, args={}, content=None):
        self.args    = args
        self.content = StringIO.StringIO(content)
        self.output  = StringIO.StringIO()
        self.header  = {}
        self.result  = {}
        self.data    = ''
        self.responseCode = 0
        self.finished  = False
        self.responseString = ""
        
    def write(self, string):
        self.output.write(string)

    def setHeader(self, key, value):
        self.header[key] = value

    def setResponseCode(self, number, string):
        self.responseCode = number
        self.responseString = string

    def finish(self):
        assert self.finished == False, "Error double-finishing"
        self.output.seek(0)
        self.data = self.output.read()
        try:
            self.result = yaml.load(self.data).next()
        except:
            self.result = {}
        self.finished = True

    def getClient(self):
        return "test"

    def getClientIP(self):
        return "127.0.0.1"

    def waitToFinish(self, maxWait=5):
        currentWait = 0
        while self.finished == False:
            time.sleep(1)
            currentWait += 1
            assert currentWait <= maxWait, "Time to process the task exceeded (%s)" % self.output.read()

