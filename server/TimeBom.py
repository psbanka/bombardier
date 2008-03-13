#!/cygdrive/c/Python25/python.exe

import time
import SecureCommSocket
import CommSocket
import traceback, StringIO
import syslog
import os, yaml
from libCipher import encrypt, decryptString, pad
from threading import Thread
from staticData import *
from getpass import getpass
from Slash import *

DIZEBUG = 0
OK_TO_PROMPT = True
TB_CHECK_INTERVAL = 300
TB_JOB_FILE = "jobs.yml"

class MessageListLengthException(Exception):
    pass

class JobNotFoundException(Exception):
    pass

syslog.openlog("timebom", syslog.LOG_PID, syslog.LOG_USER)
syslog.LOG_UPTO(syslog.LOG_INFO)


class Logger:
    def __init__(self, user, process):
        self.user = user
        self.process = process

    def info(self, message):
        self.log(syslog.LOG_INFO, message)

    def debug(self, message):
        #self.log(syslog.LOG_INFO, "reallyDEBUG " + message)
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
        Logger.__init__(self, 'scheduler', 0)
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
        self.timeOfNextJob = time.time() * 2 
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
                self.info("Removing job %s" % jobName)
                del self.jobDict[jobName]
        self.getTimeOfNextJob()

    def clear(self):
        self.delJobs(self.jobDict.keys())

    def run(self, jobList, timeout=5):
        self.checkRunning()
        for jobName in jobList:
            if jobName in self.jobDict.keys():
                job = self.jobDict[jobName]
                self.debug("Spawning job -- name:%s process: %s command: %s" % (jobName, self.nextProcessNumber, job.bomshCmd))
                job.spawn(self.nextProcessNumber)
                self.nextProcessNumber += 1
                now = time.time()            
                while not job.isRunning():
                    time.sleep(.1)
                    if time.time() - now > timeout:
                        self.error("Timed out waiting for job %s to start..." % jobName)
                        break
                self.debug("Started job %s" %jobName)
        self.getTimeOfNextJob()

    def killJobs(self, jobNames, timeout=10):
        status = OK
        for jobName in jobNames:
            if not jobName in self.jobDict:
                raise JobNotFoundException
            job = self.jobDict[jobName]
            if job.isRunning():
                job.killThread()
        
        for jobName in jobNames:
            now = time.time()
            job = self.jobDict[jobName]
            while job.isRunning():
                self.info("Waiting for job %s to die..." % jobName)
                if (time.time() - now) > timeout:
                    self.error("Timed out waiting for job %s to die..." % jobName)
                    status = FAIL
                    break
                time.sleep(1)
        return status

    def enableJobs(self, jobNames):
        for jobName in jobNames:
            if not jobName in self.jobDict:
                raise JobNotFoundException
            self.jobDict[jobName].enabled = "yes"

    def disableJobs(self, jobNames):
        for jobName in jobNames:
            if not jobName in self.jobDict:
                raise JobNotFoundException
            self.jobDict[jobName].enabled = "no"

    def killAll(self, timeout=10):
        return self.killJobs(self.jobDict.keys(), timeout)

    def checkRunning(self):
        runningList = []
        for jobName in self.jobDict:
            job = self.jobDict[jobName]
#            if job.jt and job.fro:
#                infoStr = job.fro.read()
#                if infoStr:
#                    self.info( "Job %s buffer: %s" %(jobName, infoStr) )
            if job.isRunning():
                runningList.append(jobName)
        return runningList

class JobThread(Thread, Logger):
    def __init__(self, user, process, bomshCmd, password, timeout=100):
        Thread.__init__(self)
        Logger.__init__(self, user, process)
        self.bomshCmd = bomshCmd
        self.timeout = timeout
        self.password = password
        self.to = None
        self.fro = None
        self.checkMe = False
        self.cmdStatus = None
        self.cmdOutput = []

    def run(self): 
        mode.auth = ADMIN
        mode.batch = True
        mode.password = self.password
        if self.to.testStop():
            return
        self.info("Running %s..." % self.bomshCmd)
        try:
            self.checkMe = False
            self.cmdStatus, self.cmdOutput = slash.processCommand(self.bomshCmd.strip(), self.fro, self.fro)
            self.checkMe = True
        except:
            self.error( "Failed to run %s" % self.bomshCmd )

class Job(Logger):
    def __init__(self, password):
        self.password = password
        self.freq = None
        self.bomshCmd = None
        self.user = None
        self.lastRun = None
        self.jt = None
        self.to = None
        self.fro = None
        self.lastStatus = None
        self.enabled = "no"
        Logger.__init__(self, self.user, 0)

    def isRunning(self):
        if self.jt:
            if self.jt.isAlive():
                return True
            if self.jt.checkMe:
                self.info("%s -- status: %s" % (self.name, self.jt.cmdStatus))
                self.info("%s -- cmdOutput: %s" %(self.name, self.jt.cmdOutput))
                commandLog = self.fro.read()
                [ self.info(logLine) for logLine in commandLog.split( COMMAND_LOG_MARKER )] 
                self.jt.checkMe = False
                self.lastStatus = RETURN_DICT[self.jt.cmdStatus]
        return False

    def initArgs(self, name, bomshCmd, freq, user, lastRun=0, enabled="yes"):
        self.name = name
        self.bomshCmd = bomshCmd
        self.freq = int(freq)
        self.user = user
        if lastRun:
            self.lastRun = int(lastRun)
        else:
            self.lastRun = 0
        self.enabled = enabled
        self.nextRun = self.lastRun + self.freq

    def initDict(self, dict):
        self.initArgs(dict["name"], dict["bomshCmd"], dict["freq"], dict["user"], dict.get("lastRun"), dict.get("enabled"))

    def toDict(self):
        running = "No"
        if self.isRunning():
            running = "Yes"
        return { "freq": self.freq, "user": self.user, "running":running, "enabled": self.enabled,
                 "lastRun": self.lastRun, "bomshCmd" : self.bomshCmd, "lastStatus": self.lastStatus}

    def spawn(self, process):
        if self.enabled == "no":
            self.info("Not running job %s: it is disabled." % self.name)
            return FAIL
        self.lastRun = time.time()
        self.nextRun = self.lastRun + self.freq
        self.debug("spawn: lastRun: %s nextRun: %s" %(self.lastRun, self.nextRun))
        if self.isRunning():
            self.error("Refusing to run %s; it is already running" % self.name)
            return FAIL
        self.jt = JobThread(self.user, process, self.bomshCmd, self.password)
        self.jt.to = CommSocket.CommSocket()
        self.jt.fro = CommSocket.CommSocket()
        self.to = self.jt.to 
        self.fro = self.jt.fro 
        self.jt.start()
        return OK

    def killThread(self, timeout=5):
        if not self.isRunning():
            return OK
        self.to.sendStop()
        self.jt.join(timeout)
        return OK

class TimeBom(Thread, Logger):
    def __init__(self, password, dataPath, jobFile=TB_JOB_FILE, port=TB_CTRL_PORT):
        Thread.__init__(self)
        Logger.__init__(self, "scheduler", 0)
        self.password          = password
        self.jobFile           = dataPath + '/' + jobFile
        self.commSocketToJob   = None
        self.commSocketFromJob = None
        self.jobThread         = None
        self.checkTime         = 0
        self.status            = OK
        self.timeOfNextJob     = time.time()+999999
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

    def enableJobs(self, jobList):
        return self.jobs.enableJobs(jobList)

    def disableJobs(self, jobList):
        return self.jobs.disableJobs(jobList)

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
        now = time.time()
        self.debug("checkTimers: next: %s : current: %s" %(self.jobs.timeOfNextJob, now))
        if now > self.jobs.timeOfNextJob:
            self.debug("Time for job run")
            runnableJobs = self.getRunnableJobs()
            return TB_RUN_JOB, runnableJobs
        return TB_WAIT, []

    def run(self):
        if self.status == FAIL:
            self.error("CANNOT START. NO CONTROL SOCKET.")
            return FAIL
        self.info("Timebom starting...")
        try:
            while True:
                self.debug("IM IN UR LOOP")
                command, messageList, clientAddress = self.controlSocket.getSecure()
                self.debug(command)
                self.debug(str(messageList))
                if not command:
                    command, messageList = self.checkTimers()
                if command != TB_WAIT:
                    self.debug("COMMAND: (%s)" % command)
                if command == TB_KILL:
                    self.killAll()
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
                elif command == TB_ENABLE:
                    self.enableJobs(messageList)
                elif command == TB_DISABLE:
                    self.disableJobs(messageList)
                elif command == TB_ADD: 
                    if len(messageList) == 4:
                        bomshCmd, freq, name, user = messageList
                        self.jobs.addJob( bomshCmd=bomshCmd, freq=freq, name=name, user=user )
                    else:
                        self.error("Message list length error in addJob: expected 4, got %s" %len(messageList))
                elif command == TB_DEL: 
                    self.jobs.delJobs(messageList)
                elif command == TB_WAIT:
                    time.sleep(1)
                else:
                    self.warning( "UNKNOWN COMMAND: %s" %command )
                self.controlSocket.disconnectClient(clientAddress)
                      
        except Exception, e:
            self.error("Error detected in %s (%s)." % (file, e))
            e = StringIO.StringIO()
            traceback.print_exc(file=e)
            e.seek(0)
            data = e.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            self.error(ermsg)
            self.error("\n")
        time.sleep(2)
        self.controlSocket.cleanup()
        self.info("Exiting.")

    def waitForJobStart(self, jobName, timeout=5):
        jobList = self.getRunningJobs()
        if jobName in jobList:
            return OK
        jobList.append(jobName)
        return self.waitForJobList(jobList, timeout)

    def waitForJobStop(self, jobName, timeout=5):
        jobList = self.getRunningJobs()
        if not jobName in jobList:
            return OK
        jobList.remove(jobName)
        return self.waitForJobList(jobList, timeout)

    def waitForJobList(self, targetJobList, timeout):
        while timeout > 0: 
            if set(self.getRunningJobs()) == set(targetJobList):
                return OK
            time.sleep(0.1)
            timeout -= 0.1
        return FAIL

def areJobsEqual(j1, j2, ignoreProps = ['lastRun', 'running']):
    if not set(j1.keys()) == set(j2.keys()):
        return False
    job1 = j1[j1.keys()[0]] 
    job1['name'] = j1.keys()[0] 
    job2 = j2[j2.keys()[0]] 
    job2['name'] = j2.keys()[0] 
    properties = set(job1.keys()) - set(ignoreProps)
    for prop in properties:
        if prop not in job2 or prop not in job1:
            return False
        prop1 = job1[prop]
        prop2 = job2[prop]
        if not prop1 == prop2:
            return False
    return True

def areJobListsEqual( jobList1, jobList2, ignoreProps = ['lastRun', 'running'] ):
    numJobs = len(jobList1)
    if numJobs != len(jobList2):
        return False
    jobList1.sort()
    jobList2.sort()           
    for i in range(numJobs):
        if not areJobsEqual(jobList1[i], jobList2[i], ignoreProps):
            return False
    return True

def test():
    from libTest import startTest, runTest, endTest, assertOk
    os.chdir('/')
    status = OK
    TEST_PORT     = 2384
    TEST_JOB_FILE = mode.dataPath + "/testJobFile.yml"
    TEST_PASSWORD = 'fooman'
    password = TEST_PASSWORD
    if OK_TO_PROMPT:
        password = getpass("Gimme the password, fool: ")

    if password == '':
        password = TEST_PASSWORD

    tb = TimeBom(password, '.', TEST_JOB_FILE, TEST_PORT)
    JOB_sleepTest = {'sleepTest': {'lastRun': 0, 'lastStatus': None, 'freq': 5, 'bomshCmd': 'sleep 2', 'user': 'shawn'}}
    JOB_sleepTestRunning = {'sleepTest': {'lastRun': 0, 'lastStatus': None, 'freq': 5, 'user': 'shawn', 'running': 'Yes', 'bomshCmd': 'sleep 2'}}
    JOB_sleepTestNotRunning = {'sleepTest': {'lastRun': 0, 'lastStatus': None, 'freq': 5, 'user': 'shawn', 'running': 'No', 'bomshCmd': 'sleep 2'}}
    JOB_echoTest  = {'echoTest': {'lastRun': 0, 'freq': 3600, 'lastStatus': None, 'bomshCmd': 'echo foo', 'user': 'chawn'}}
    combined = [{'echoTest': {'lastRun': 0, 'freq': 3600, 'lastStatus': None, 'bomshCmd': 'echo foo', 'user': 'chawn'}, 
                 'sleepTest': {'lastRun': 0, 'freq': 5, 'lastStatus': None, 'bomshCmd': 'sleep 2', 'user': 'shawn'}}]
    different = [{'echoTest': {'lastRun': 1, 'freq': 3600, 'lastStatus': None, 'bomshCmd': 'echo foo', 'user': 'chawn'}, 
                 'sleepTest': {'lastRun': 0, 'freq': 5, 'lastStatus': None, 'bomshCmd': 'sleep 2', 'user': 'shawn'}}]
    pyChucker(JOB_sleepTest, JOB_sleepTestRunning, JOB_sleepTestNotRunning, JOB_echoTest, combined, different)
    c = SecureCommSocket.SecureClient(TEST_PORT, password)

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
    status = runTest(testJob.spawn, [24], OK, status)
    status = runTest(testJob.spawn, [23], FAIL, status)
    status = runTest(testJob.killThread, [], OK, status)
    #assert not testJob.isRunning()

    # make sure a job can be spawned again...
    status = runTest(testJob.spawn, [24], OK, status)
    status = runTest(testJob.killThread, [], OK, status)
    status = runTest(testJob.killThread, [], OK, status)
    #assert not testJob.isRunning()

    # higher-level job threading testing
    status = runTest(tb.runJobs, [["sleepTest"]], None, status)
    status = runTest(tb.getRunningJobs, [], ["sleepTest"], status)
    status = runTest(tb.killJobs, [['sleepTest']], OK, status)
    status = runTest(tb.killAll, [], OK, status)
    status = runTest(tb.getRunningJobs, [], [], status)

    # make sure we can detect when a job dies
    status = runTest(tb.runJobs, [["sleepTest"]], None, status)
    status = assertOk(tb.waitForJobStart, ["sleepTest"], status)
    status = assertOk(tb.waitForJobStop, ["sleepTest"], status)

    # testing socket interaction with the server
    JOB_sleepTest["sleepTest"]["lastRun"] = tb.jobs.jobDict['sleepTest'].lastRun
    combined[0]["sleepTest"]["lastRun"] = tb.jobs.jobDict['sleepTest'].lastRun
    tb.start()

    status = runTest(tb.getRunningJobs, [], [], status)
    #assert( areJobListsEqual(c.sendSecure(TB_SHOW, [] ), [JOB_sleepTest]) ) 
    #assert( areJobListsEqual(c.sendSecure(TB_SHOW, ["show"] ), [JOB_sleepTest]) ) 
    status = runTest(c.sendSecure, [TB_ADD, ["echo foo", 3600, "echoTest", "chawn" ]], [], status)
    #assert( areJobListsEqual(c.sendSecure(TB_SHOW, [] ), combined) ) 
    status = runTest(c.sendSecure, [TB_ENABLE, ["sleepTest"]], [], status)
    status = runTest(c.sendSecure, [TB_DISABLE, ["sleepTest"]], [], status)
    status = runTest(c.sendSecure, [TB_ADD, ["echo foo", 3600, "echoTest", "chawn" ]], [], status)
    status = runTest(c.sendSecure, [TB_DEL, ["sleepTest"]], [], status)
    #assert( areJobListsEqual(c.sendSecure(TB_SHOW, [] ), [JOB_echoTest]) ) 
    status = runTest(tb.getRunningJobs, [], [], status)
    status = runTest(c.sendSecure, [TB_SAVE, []], [], status)
    status = runTest(tb.getRunningJobs, [], [], status)
    tb.clearJobs()
    status = runTest(c.sendSecure, [TB_LOAD, []], [], status)
    #assert( areJobListsEqual(c.sendSecure(TB_SHOW, [] ), [JOB_echoTest]) ) 
    status = runTest(tb.getRunningJobs, [], [], status)

    # TEST SCHEDULING A JOB TO RUN ON ITS OWN
    tb.clearJobs()
    tb.jobs.addJob(name="sleepTest", bomshCmd="sleep 3", freq=5, user="shawn")
    status = assertOk(tb.waitForJobStart, ["sleepTest"], status)
    status = assertOk(tb.waitForJobStop, ["sleepTest"], status)
    status = runTest(tb.getRunningJobs, [], [], status)

    # BY NOW sleepTest SHOULD BE RUNNING AGAIN
    status = assertOk(tb.waitForJobStart, ["sleepTest"], status)

    # In-depth package test
    if password != TEST_PASSWORD:
        tb.clearJobs()
        tb.jobs.addJob(name="mcsLiveTest", bomshCmd="package exec bigsam CargoMcs_R2AD-65366 testDb", freq=15, user="peter")
        status = assertOk(tb.waitForJobStart, ["mcsLiveTest"], status)
        status = assertOk(tb.waitForJobStop, ["mcsLiveTest", 25], status)
        status = assertOk(tb.waitForJobStart, ["mcsLiveTest"], status)
        status = assertOk(tb.waitForJobStop, ["mcsLiveTest", 15], status)

    print "/////////////////////////////////////////////////////////////////////////"
    print "\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\" 
    print "/////////////////////////////////////////////////////////////////////////"
    status = runTest(c.sendSecure, [TB_KILL,[ "kill"]], [], status)
    endTest(status)
    sys.exit(0)

def startService(passwd):
    dataPath = os.getcwd()
    tb = TimeBom(passwd, dataPath, port=TB_CTRL_PORT)
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
    import sys
    passwd = getpass("Server password: ")
    startService(passwd)
