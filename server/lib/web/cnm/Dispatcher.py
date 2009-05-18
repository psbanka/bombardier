import Pyro.core
import time
import Pyro.naming
from MachineConfig import MachineConfig
from MachineInterface import MachineInterface
import CommSocket
from bombardier_core.static_data import OK, FAIL, COMMAND_LOG_MARKER
import syslog
from threading import Thread

syslog.openlog("dispatcher", syslog.LOG_PID, syslog.LOG_USER)
syslog.LOG_UPTO(syslog.LOG_INFO)

class Logger:
    def __init__(self, username, process):
        self.username = username
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
        syslog.syslog(level, "%-15s|dispatcher: %s" % (self.username, message))

class Job(Thread, Logger):
    def __init__(self, name, username, machine, command):
        self.name = name
        self.username = username
        self.machine = machine
        self.command = command
        self.output = None
        Logger.__init__(self, self.username, 0)

    def is_running(self):
        if self.isAlive():
            return True

    def run(self):
        self.info("Running %s..." % self.command)
        try:
            self.output = self.machine.run_cmd(command)
        except:
            self.error( "Failed to run %s" % self.command )
            pass
        self.info("job %d is complete (%s)" % (self.name, self.output))

class Dispatcher(Pyro.core.ObjBase):
    def __init__(self):
        Pyro.core.ObjBase.__init__(self)
        self.start_time = time.time()
        self.calls = 0
        self.password = None
        self.server_home = None
        self.jobs = {}
        self.next_process = 1

    def set_server_home(self, server_home):
        self.server_home = server_home

    def start_job(self, username, machine_name):
        machine_config = MachineConfig(machine_name, self.password,
                                       self.server_home)
        machine_config.merge()
        machine = MachineInterface(machine_config, response)
        job = Job(self.next_job, machine, "echo foo")
        self.jobs[self.next_job] = job
        self.next_job += 1
        job.start()
        return job.name

    def job_report(self, job_name):
        job = self.jobs[job_name]
        if self.job.isAlive():
            return "ALIVE"
        return "DEAD"

    def check_in(self):
        time_running = time.time() - self.start_time
        self.calls += 1
        return "time running: %5.2f; calls: %d" % (time_running, self.calls)

    def set_password(self, password):
        self.password = password

    def check_password(self):
        return self.password


#Pyro.core.initServer()
daemon = Pyro.core.Daemon()
ns = Pyro.naming.NameServerLocator().getNS()
daemon.useNameServer(ns)

#daemon=Pyro.core.Daemon()
#print "The daemon runs on port:",daemon.port
#print "The object's uri is:",uri

uri = daemon.connect(Dispatcher(),"dispatcher")
daemon.requestLoop()

