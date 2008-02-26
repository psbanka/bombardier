#!/cygdrive/c/Python24/python.exe

import time
import SecureCommSocket
import traceback, StringIO
#import JobThread
import syslog
import os, yaml
from libCipher import encrypt, decryptString, pad
from threading import Thread
from staticData import *

DIZEBUG = 0
TB_CHECK_INTERVAL = 300
TB_JOB_FILE = "jobs.yml"

class MessageListLengthException(Exception):
    pass

syslog.openlog("timebom", syslog.LOG_PID, syslog.LOG_USER)


class Logger:
    def __init__(self, user, process):
        self.user = user
        self.process = process

    def info(self, message):
        self.log(syslog.LOG_INFO, message)

    def debug(self, message):
        self.log(syslog.LOG_DEBUG, message)

    def warning(self, message):
        self.log(syslog.LOG_WARNING, message)

    def error(self, message):
        self.log(syslog.LOG_ERR, message)

    def log(self, level, message):
        if DIZEBUG:
            print level, "%-15s|timebom: %s" % (self.user, message)
        else:
            syslog.syslog(level, "%-15s|timebom: %s" % (self.user, message))

class TimeBom(Thread):
    def __init__(self, password, jobFile=TB_JOB_FILE):
        Thread.__init__(self)
        self.password          = password
        self.jobFile           = jobFile
        self.commSocketToJob   = None
        self.commSocketFromJob = None
        self.jobThread         = None
        self.checkTime         = 0

        self.logger = Logger("scheduler", 0)
        self.controlSocket = SecureCommSocket.SecureServer(TB_CTRL_PORT, self.password)
        self.jobs = {}

    def getJob(self, bomshCmd, freq, lastRun, user):
        return { "freq": freq, "user": user,
                 "lastRun": lastRun, "bomshCmd" : bomshCmd }

    def loadJobs(self):
        try:
            if os.path.isfile(self.jobFile):
                cy = open(self.jobFile).read()
                dec = decryptString(cy, self.password, YAML_CHARS)
                self.jobs = yaml.load(dec)
            return OK
        except:
            return FAIL

    def saveJobs(self):
        fd = open(self.jobFile, "w")
        yamlly = yaml.dump(self.jobs)
        crypty = encrypt(yamlly, self.password) 
        fd.write(crypty )
        return OK

    def runBomshCommand(self, user, command):
        self.logger.info("RUNNING")
        self.logger.debug("runBomshCommand: %s : %s " %(user, command) )
        return

    def checkTimers(self):
        if time.time() > self.checkTime:
            self.logger.info("Time for job run")
            self.checkTime = time.time() + TB_CHECK_INTERVAL
            return TB_CHECK
        return TB_WAIT

    def addJob(self, argList ):
        if len( argList ) == 4:
            bomshCmd, freq, name, user = argList
            self.jobs[name] = self.getJob( bomshCmd, freq, 0, user )
        else:
            raise MessageListLengthException

    def showJobs(self):
        return self.jobs

    def run(self):
        self.logger.info("Timebom starting...")
        try:
            while True:
                self.logger.debug("IM IN YUR LOOP")
                command, messageList, clientAddress = self.controlSocket.getSecure()
                self.logger.debug(command)
                self.logger.debug(str(messageList))
                if not command:
                    command = self.checkTimers()
                self.logger.info("COMMAND: (%s)" % command)
                if command == TB_KILL:
                    self.controlSocket.disconnectClient(clientAddress)
                    break
                elif command == TB_CHECK:
                    self.logger.info( command)
                    self.runBomshCommand("CHECK", command)
                elif command == TB_SHOW:
                    self.controlSocket.sendClient(clientAddress, self.jobs)
                elif command == TB_SAVE:
                    self.saveJobs()
                elif command == TB_LOAD:
                    self.loadJobs()
                elif command == TB_ADD: 
                    try:
                        self.addJob( messageList )
                    except MessageListLengthException:
                        self.logger.error("Message list length error in addJob: expected 4, got %s" %len(messageList))
                elif command == TB_DEL: 
                    for message in messageList:
                        if message in self.jobs:
                            del self.jobs[message]
                elif command == TB_WAIT:
                    time.sleep(1)
                else:
                    self.logger.warning( "UNKNOWN COMMAND: %s" %command )
                self.controlSocket.disconnectClient(clientAddress)
                      
        except Exception, e:
            self.logger.error("Error detected in %s (%s)." % (file, e))
            e = StringIO.StringIO()
            traceback.print_exc(file=e)
            e.seek(0)
            data = e.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            self.logger.error(ermsg)
            self.logger.error("\n")
        self.logger.info("Exiting.")
           
def test():
    from libTest import startTest, runTest, endTest 
    status = OK
    TEST_JOB_FILE = "testJobFile.yml"
    startTest()

    tb = TimeBom("fooman", TEST_JOB_FILE)
    open( TEST_JOB_FILE, 'w').write("FLIFFY")
    status = runTest(tb.loadJobs, [], FAIL, status)
    tb.jobs = {"statlilap": tb.getJob( "sho stat lilap", 5, 0, "shawn")} 
    status = runTest(tb.saveJobs, [], OK, status)
    tb.jobs = {}
    status = runTest(tb.loadJobs, [], OK, status)
    tb.start()

    JOB_statlilap = {'statlilap': {'lastRun': 0, 'freq': 5, 'bomshCmd': 'sho stat lilap', 'user': 'shawn'}}
    JOB_checker2  = {'checker': {'lastRun': 0, 'freq': '3600', 'bomshCmd': 'sho package sqlback', 'user': 'chawn'}}
    combined = [{'checker': {'lastRun': 0, 'freq': '3600', 'bomshCmd': 'sho package sqlback', 'user': 'chawn'}, 'statlilap': {'lastRun': 0, 'freq': 5, 'bomshCmd': 'sho stat lilap', 'user': 'shawn'}}]
    c = SecureCommSocket.SecureClient(TB_CTRL_PORT, "fooman")
    status = runTest(c.sendSecure, [TB_SHOW, []], [JOB_statlilap], status)
    status = runTest(c.sendSecure, [TB_SHOW, ["show"]], [JOB_statlilap], status)
    status = runTest(c.sendSecure, [TB_ADD, ["sho package sqlback", 3600, "checker", "chawn" ]], [], status)
    status = runTest(c.sendSecure, [TB_SHOW, []], combined, status)
    status = runTest(c.sendSecure, [TB_DEL, ["statlilap"]], [], status)
    status = runTest(c.sendSecure, [TB_SHOW, []], [JOB_checker2], status)
    status = runTest(c.sendSecure, [TB_SAVE, []], [], status)
    tb.jobs = []
    status = runTest(c.sendSecure, [TB_LOAD, []], [], status)
    status = runTest(c.sendSecure, [TB_SHOW, []], [JOB_checker2], status)
    status = runTest(c.sendSecure, [TB_KILL,[ "kill"]], [], status)
    
    endTest(status)
    sys.exit(0)

if __name__ == "__main__":
    #test()
    from getpass import getpass
    import sys
    passwd = getpass("GETPASS: ")
    tb = TimeBom(passwd)
    tb.loadJobs()
    
    # do the UNIX double-fork magic, see Stevens' "Advanced 
    # Programming in the UNIX Environment" for details (ISBN 0201563177)
    try: 
        pid = os.fork() 
        if pid > 0:
            # exit first parent
            sys.exit(0) 
    except OSError, e: 
        print >>sys.stderr, "fork #1 failed: %d (%s)" % (e.errno, e.strerror) 
        sys.exit(1)

    # decouple from parent environment
    os.chdir("/") 
    os.setsid() 
    os.umask(0) 

    # do second fork
    try: 
        pid = os.fork() 
        if pid > 0:
            # exit from second parent, print eventual PID before
            print "Daemon PID %d" % pid 
            sys.exit(0) 
    except OSError, e: 
        print >>sys.stderr, "fork #2 failed: %d (%s)" % (e.errno, e.strerror) 
        sys.exit(1) 

    # start the daemon main loop
    #tb.start()
    tb.run()
