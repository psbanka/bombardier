#!/cygdrive/c/Python24/python.exe

import time
import SecureCommSocket
import CommSocket
import traceback, StringIO
import syslog
import os, yaml
from libCipher import encrypt, decryptString, pad
from threading import Thread
from staticData import *
from Slash import *

DIZEBUG = 0
TB_CHECK_INTERVAL = 300
TB_JOB_FILE = "jobs.yml"

class MessageListLengthException(Exception):
    pass

class JobNotFoundException(Exception):
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

class JobCollection(Logger):
    def __init__(self, jobFile, password):
        Logger.__init__(self, 'root', 0)
        self.jobFile = jobFile
        self.password = password
        self.jobDict = {}
        self.timeOfNextJob = time.time() + 99999
        self.nextProcessNumber = 1

    def load(self):
        #if 1 == 1:
        try:
            if os.path.isfile(self.jobFile):
                cipherText = open(self.jobFile).read()
                plainText  = decryptString(cipherText, self.password, YAML_CHARS)
                tmpDict = yaml.load(plainText)
                for jobName in tmpDict:
                    tmpDict[jobName]['name'] = jobName
                    self.jobDict[jobName] = Job(self.password)
                    self.jobDict[jobName].initDict(tmpDict[jobName])
                self.getTimeOfNextJob()
            return OK
        #else:
        except:
            return FAIL

    def save(self):
        fd = open(self.jobFile, "w")
        plainText = yaml.dump(self.toDict())
        cipherText = encrypt(plainText, self.password) 
        fd.write(cipherText)
        return OK

    def toDict(self):
        output = {}
        for jobName in self.jobDict:
             output[jobName] = self.jobDict[jobName].toDict()
        return output

    def getTimeOfNextJob(self):
        now = time.time()
        for jobName in self.jobDict:
            job = self.jobDict[jobName]
            if job.nextRun < self.timeOfNextJob:
                self.timeOfNextJob = job.nextRun
                if self.timeOfNextJob < now:
                    break

    def getRunnable(self):
        now = time.time()
        jobList = []
        for jobName in self.jobDict:
            job = self.jobDict[jobName]
            if job.nextRun < now:
                jobList.append(jobName)
        return jobList

    def addJob(self, bomshCmd, freq, name, user):
        self.jobDict[name] = Job(self.password)
        self.jobDict[name].initArgs( name, bomshCmd, freq, user )
        self.getTimeOfNextJob()

    def delJobs(self, jobNames):
        self.killJobs(jobNames)
        for jobName in jobNames:
            if jobName in self.jobDict:
                del self.jobDict[jobName]
        self.getTimeOfNextJob()

    def clear(self):
        self.delJobs(self.jobDict.keys())

    def run(self, jobList):
        self.info("run: %s" % str(jobList))
        for jobName in jobList:
            if jobName in self.jobDict.keys():
                job = self.jobDict[jobName]
                self.info("Spawning job %s (%s)" % (jobName, self.nextProcessNumber))
                job.spawn(self.nextProcessNumber)
                self.nextProcessNumber += 1
                now = time.time()            
                while not job.running:
                    time.sleep(.1)
                    if time.time() - now > 5:
                        break
                self.info("...done")

    def killJobs(self, jobNames, timeout=10):
        status = OK
        for jobName in jobNames:
            if not jobName in self.jobDict:
                raise JobNotFoundException
            job = self.jobDict[jobName]
            if job.running:
                job.killThread()
        
        for jobName in jobNames:
            now = time.time()
            job = self.jobDict[jobName]
            while job.running:
                self.info("Waiting for job %s to die..." % jobName)
                if (time.time() - now) > timeout:
                    status = FAIL
                    break
                time.sleep(1)
        return status

    def killAll(self, timeout=10):
        return self.killJobs(self.jobDict.keys(), timeout)

    def checkRunning(self):
        runningList = []
        for jobName in self.jobDict:
            job = self.jobDict[jobName]
            job.checkThread()
            if job.running:
                runningList.append(jobName)
        return runningList

class JobThread(Thread, Logger):
    def __init__(self, user, process, bomshCmd, password, timeout=100):
        Thread.__init__(self)
        Logger.__init__(self, user, process)
        self.bomshCmd = bomshCmd
        self.timeout = timeout
        self.password = password

    def run(self): 
        now = time.time()
        mode.auth = ADMIN
        mode.batch = True
        mode.password = self.password
        if self.to.testStop():
            self.fro.sendStop()
            return
        self.info("Running %s..." % self.bomshCmd)
        cmdStatus, cmdOutput = slash.processCommand(self.bomshCmd.strip())
        self.fro.sendStop()

class Job(Logger):
    def __init__(self, password):
        self.password = password
        self.freq = None
        self.bomshCmd = None
        self.user = None
        self.lastRun = None
        self.running = False
        Logger.__init__(self, self.user, 0)

    def initArgs(self, name, bomshCmd, freq, user, lastRun=0):
        self.name = name
        self.bomshCmd = bomshCmd
        self.freq = int(freq)
        self.user = user
        if lastRun:
            self.lastRun = int(lastRun)
        else:
            self.lastRun = 0
        self.nextRun = self.lastRun + self.freq

    def initDict(self, dict):
        self.initArgs(dict["name"], dict["bomshCmd"], dict["freq"], dict["user"], dict.get("lastRun"))

    def toDict(self):
        return { "freq": self.freq, "user": self.user,
                 "lastRun": self.lastRun, "bomshCmd" : self.bomshCmd }

    def spawn(self, process):
        if self.running:
            self.error("Refusing to run %s; it is already running" % self.name)
            return FAIL
        jt = JobThread(self.user, process, self.bomshCmd, self.password)
        jt.to = CommSocket.CommSocket()
        self.to = jt.to 
        jt.fro = CommSocket.CommSocket()
        self.fro = jt.fro 
        jt.start()
        self.lastRun = time.time()
        self.nextRun = self.lastRun + self.freq
        self.running = True
        return OK

    def checkThread(self):
        if not self.running:
            return OK
        if self.fro.testStop():
            self.running = False
        return OK

    def killThread(self):
        if not self.running:
            return OK
        self.to.sendStop()
        now = time.time()
        while not self.fro.testStop():
            time.sleep(.1)
            if time.time() - now > 5:
                self.info( "killThread: %s timeout" % self.name)
                return FAIL
        self.running = False
        return OK

class TimeBom(Thread):
    def __init__(self, password, jobFile=TB_JOB_FILE, port=TB_CTRL_PORT):
        Thread.__init__(self)
        self.password          = password
        self.jobFile           = jobFile
        self.commSocketToJob   = None
        self.commSocketFromJob = None
        self.jobThread         = None
        self.checkTime         = 0
        self.status            = OK
        self.timeOfNextJob     = time.time()+999999

        self.logger = Logger("scheduler", 0)
        try:
            self.controlSocket = SecureCommSocket.SecureServer(port, self.password)
        except:
            self.status = FAIL
        self.jobs = JobCollection(jobFile, self.password)

    # PASS-Through methods ########

    def loadJobs(self):
        return self.jobs.load()

    def saveJobs(self):
        return self.jobs.save()

    def clearJobs(self):
        return self.jobs.clear()
    
    def showJobs(self):
        return self.jobs.toDict()

    def getRunnableJobs(self):
        return self.jobs.getRunnable()

    def runJobs(self, jobList):
        return self.jobs.run(jobList)

    def killJobs(self, jobList):
        return self.jobs.killJobs(jobList)

    def killAll(self):
        return self.jobs.killAll()

    def getRunningJobs(self):
        return self.jobs.checkRunning()

    ################################

    def checkTimers(self):
        if time.time() > self.jobs.timeOfNextJob:
            self.logger.info("Time for job run")
            runnableJobs = self.getRunnableJobs()
            return TB_RUN_JOB, [runnableJobs]
        return TB_WAIT, []

    def run(self):
        if self.status == FAIL:
            self.logger.error("CANNOT START. NO CONTROL SOCKET.")
            return FAIL
        self.logger.info("Timebom starting...")
        try:
            while True:
                self.logger.debug("IM IN UR LOOP")
                command, messageList, clientAddress = self.controlSocket.getSecure()
                self.logger.debug(command)
                self.logger.debug(str(messageList))
                if not command:
                    command, messageList = self.checkTimers()
                self.logger.info("COMMAND: (%s)" % command)
                if command == TB_KILL:
                    self.controlSocket.disconnectClient(clientAddress)
                    break
                elif command == TB_RUN_JOB:
                    self.runJobs(messageList)
                elif command == TB_SHOW:
                    self.controlSocket.sendClient(clientAddress, self.jobs.toDict())
                elif command == TB_SAVE:
                    self.saveJobs()
                elif command == TB_LOAD:
                    self.loadJobs()
                elif command == TB_ADD: 
                    if len(messageList) == 4:
                        bomshCmd, freq, name, user = messageList
                        self.jobs.addJob( bomshCmd=bomshCmd, freq=freq, name=name, user=user )
                    else:
                        self.logger.error("Message list length error in addJob: expected 4, got %s" %len(messageList))
                elif command == TB_DEL: 
                    self.jobs.delJobs(messageList)
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
        time.sleep(2)
        self.controlSocket.cleanup()
        self.logger.info("Exiting.")
           
def test():
    from libTest import startTest, runTest, endTest 
    status = OK
    TEST_PORT     = 2384
    TEST_JOB_FILE = "testJobFile.yml"
    tb = TimeBom("fooman", TEST_JOB_FILE, TEST_PORT)
    JOB_sleepTest = {'sleepTest': {'lastRun': 0, 'freq': 5, 'bomshCmd': 'sleep 2', 'user': 'shawn'}}
    JOB_echoTest  = {'echoTest': {'lastRun': 0, 'freq': 3600, 'bomshCmd': 'echo foo', 'user': 'chawn'}}
    combined = [{'echoTest': {'lastRun': 0, 'freq': 3600, 'bomshCmd': 'echo foo', 'user': 'chawn'}, 
                 'sleepTest': {'lastRun': 0, 'freq': 5, 'bomshCmd': 'sleep 2', 'user': 'shawn'}}]
    c = SecureCommSocket.SecureClient(TEST_PORT, "fooman")

    startTest()

    # job file manipulation
    open( TEST_JOB_FILE, 'w').write("FLIFFY")
    status = runTest(tb.loadJobs, [], FAIL, status)
    status = runTest(tb.saveJobs, [], OK, status)
    tb.clearJobs()
    status = runTest(tb.loadJobs, [], OK, status)

    # Try spawning some jobs and killing them...
    tb.jobs.addJob(name="sleepTest", bomshCmd="sleep 2", freq=5, user="shawn")
    status = runTest(tb.getRunnableJobs, [], ["sleepTest"], status)
    testJob = tb.jobs.jobDict['sleepTest']
    status = runTest(testJob.spawn, [23], OK, status)
    status = runTest(testJob.spawn, [23], FAIL, status)
    status = runTest(testJob.killThread, [], OK, status)
    assert testJob.running == False

    # make sure a job can be spawned again...
    status = runTest(testJob.spawn, [24], OK, status)
    status = runTest(testJob.killThread, [], OK, status)
    status = runTest(testJob.killThread, [], OK, status)
    assert testJob.running == False

    # higher-level job threading testing
    status = runTest(tb.runJobs, [["sleepTest"]], None, status)
    status = runTest(tb.getRunningJobs, [], ["sleepTest"], status)
    status = runTest(tb.killJobs, [['sleepTest']], OK, status)
    status = runTest(tb.killAll, [], OK, status)
    status = runTest(tb.getRunningJobs, [], [], status)

    # make sure we can detect when a job dies
    status = runTest(tb.runJobs, [["sleepTest"]], None, status)
    status = runTest(tb.getRunningJobs, [], ["sleepTest"], status)
    time.sleep(2.5)
    status = runTest(tb.getRunningJobs, [], [], status)
    
    # testing socket interaction with the server
    JOB_sleepTest["sleepTest"]["lastRun"] = tb.jobs.jobDict['sleepTest'].lastRun
    combined[0]["sleepTest"]["lastRun"] = tb.jobs.jobDict['sleepTest'].lastRun
    tb.start()

    status = runTest(tb.getRunningJobs, [], [], status)
    status = runTest(c.sendSecure, [TB_SHOW, []], [JOB_sleepTest], status)
    status = runTest(c.sendSecure, [TB_SHOW, ["show"]], [JOB_sleepTest], status)
    status = runTest(c.sendSecure, [TB_ADD, ["echo foo", 3600, "echoTest", "chawn" ]], [], status)
    status = runTest(c.sendSecure, [TB_SHOW, []], combined, status)
    status = runTest(c.sendSecure, [TB_DEL, ["sleepTest"]], [], status)
    status = runTest(c.sendSecure, [TB_SHOW, []], [JOB_echoTest], status)
    status = runTest(tb.getRunningJobs, [], [], status)
    status = runTest(c.sendSecure, [TB_SAVE, []], [], status)
    status = runTest(tb.getRunningJobs, [], [], status)
    tb.clearJobs()
    status = runTest(c.sendSecure, [TB_LOAD, []], [], status)
    status = runTest(c.sendSecure, [TB_SHOW, []], [JOB_echoTest], status)
    status = runTest(tb.getRunningJobs, [], [], status)

    #^ TEST SCHEDULING A JOB TO RUN ON ITS OWN
    time.sleep(1)
    status = runTest(tb.getRunningJobs, [], [], status)
    time.sleep(1)
    status = runTest(tb.getRunningJobs, [], [], status)
    
    status = runTest(c.sendSecure, [TB_KILL,[ "kill"]], [], status)

    endTest(status)
    sys.exit(0)

def startService(passwd):
    tb = TimeBom(passwd, port=TB_CTRL_PORT)
    if tb.status == FAIL:
        print >>sys.stderr, "Unable to start: Address already in use."
        sys.exit(0)
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

if __name__ == "__main__":
    #test()
    from getpass import getpass
    import sys
    passwd = getpass("Server password: ")
    startService(passwd)
