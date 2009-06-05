import Pyro.core
import time
import Pyro.naming
from MachineConfig import MachineConfig
from BombardierMachineInterface import BombardierMachineInterface
from bombardier_core.static_data import OK, FAIL, COMMAND_LOG_MARKER
import syslog
from threading import Thread
import StringIO, traceback

syslog.openlog("dispatcher", syslog.LOG_PID, syslog.LOG_USER)
syslog.LOG_UPTO(syslog.LOG_INFO)

from bombardier_core import Logger
from Exceptions import InvalidJobName
import logging, sys, os, shutil
import logging.handlers

FORMAT_STRING = '%(asctime)s|%(levelname)s|%(message)s|'

class SysLogger:
    def __init__(self):
        self.python_logger = None
        self.formatter = None
        self.std_err_handler = None
        self.file_handler = None
        self.std_err()

    def std_err(self):
        self.python_logger = logging.getLogger("Dispatcher")
        self.formatter = logging.Formatter(FORMAT_STRING)
        self.std_err_handler = logging.StreamHandler(sys.stderr)
        self.std_err_handler.setFormatter(self.formatter)
        self.python_logger.addHandler(self.std_err_handler)
        self.python_logger.setLevel(logging.DEBUG)

    def debug(self, msg, username):
        self.log(syslog.LOG_DEBUG, msg, username)
        self.python_logger.debug("%s|%s" % (username, msg))

    def info(self, msg, username):
        self.log(syslog.LOG_INFO, msg, username)
        self.python_logger.info("%s|%s" % (username, msg))

    def warning(self, msg, username):
        self.log(syslog.LOG_WARNING, msg, username)
        self.python_logger.warning("%s|%s" % (username, msg))

    def error(self, msg, username):
        self.log(syslog.LOG_ERR, msg, username)
        self.python_logger.error("%s|%s" % (username, msg))

    def log(self, level, msg, username):
        syslog.syslog(level, "%-15s|dispatcher: %s" % (username, msg))

def exception_dumper(func):
    argnames = func.func_code.co_varnames[:func.func_code.co_argcount]
    fname = func.func_name
    def traceback_func(*args,**kwargs):
        try:
            print "watching the %s function" % fname
            return func(*args, **kwargs)
        except Exception, err:
            exc = StringIO.StringIO()
            traceback.print_exc(file=exc)
            exc.seek(0)
            data = exc.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg = "%% %s" % line
                print ermsg
    return traceback_func


class Job(Thread):
    def __init__(self, name, machine_interface, command, logger):
        self.name = name
        self.command = command
        self.logger = logger
        self.output_handle = StringIO.StringIO()
        self.machine_interface = machine_interface
        self.machine_interface.set_output_handle(self.output_handle)
        self.start_time = None
        self.elaped_time = None
        self.command_output = None
        self.output_pointer = 0
        Thread.__init__(self)
        self.machine_interface.freshen()

    def get_new_output(self):
        self.output_handle.seek(self.output_pointer)
        new_output = self.output_handle.read()
        self.output_pointer += len(new_output)
        return new_output

    def run(self):
        self.logger.info("Starting...", self.name)
        self.start_time = time.time()
        try:
            self.command_output = self.machine_interface.run_cmd(self.command)
        except Exception, err:
            self.logger.error( "Failed to run %s" % self.command, self.name )
            exc = StringIO.StringIO()
            traceback.print_exc(file=exc)
            exc.seek(0)
            data = exc.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg = "%% %s" % line
                self.logger.error(ermsg, self.name)
        self.logger.info("Finishing", self.name)
        self.elapsed_time = time.time() - self.start_time

class Dispatcher(Pyro.core.ObjBase):
    def __init__(self):
        Pyro.core.ObjBase.__init__(self)
        self.start_time = time.time()
        self.calls = 0
        self.password = None
        self.server_home = None
        self.jobs = {}
        self.next_job = 1
        self.logger = SysLogger()
        self.machine_interface_pool = {}

    def dump_exception(self, username, err):
        exc = StringIO.StringIO()
        traceback.print_exc(file=exc)
        exc.seek(0)
        data = exc.read()
        ermsg = ''
        traceback_data = []
        for line in data.split('\n'):
            traceback_data.append(line)
            ermsg = "%% %s" % line
            self.logger.error(ermsg, username)
        return {"status": FAIL, "traceback": traceback_data}

    def set_server_home(self, username, server_home):
        self.logger.info("Setting server home: %s" % server_home, username)
        self.server_home = server_home

    def start_job(self, username, machine_name):
        output = {"status": OK}
        try:
            machine_config = MachineConfig(machine_name, self.password,
                                           self.server_home)
            machine_config.merge()
            machine_interface = None
            if machine_name in self.machine_interface_pool:
                self.logger.info("Reusing existing connection to %s" % machine_name, username)
                machine_interface = self.machine_interface_pool[machine_name]
            else:
                self.logger.info("Connecting to %s" % machine_name, username)
                machine_interface = BombardierMachineInterface(machine_config)
                self.machine_interface_pool[machine_name] = machine_interface
            job_name = "%s@%s-%d" % (username, machine_name, self.next_job)
            job = Job(job_name, machine_interface, "echo foo", self.logger)
            self.jobs[job_name] = job
            self.next_job += 1
            job.start()
            output["job_name"] = job.name
        except Exception, err:
            output.update(self.dump_exception(username, err))
        return output

    def cleanup(self, username):
        output = {"status": OK}
        try:
            for machine_name in self.machine_interface_pool:
                machine_interface = self.machine_interface_pool[machine_name]
                msg = "Requested connection termination to %s" % machine_name
                self.logger.warning(msg, username)
                status = machine_interface.terminate()
                output[machine_name] = status
            self.machine_interface_pool = {}
        except Exception, err:
            output.update(self.dump_exception(username, err))
        return output

    def job_join(self, username, job_name, timeout):
        output = {"status": OK}
        try:
            job = self.jobs.get(job_name)
            start_time = time.time()
            end_time = start_time + timeout
            if not job:
                raise InvalidJobName(job_name)
            while job.isAlive():
                if time.time() > end_time:
                    output["status"] = FAIL
                    return output
                time.sleep(1)
            output["command_output"] = job.command_output
        except Exception, err:
            output.update(self.dump_exception(username, err))
        return output

    def job_poll(self, username, job_name):
        output = {"status": OK}
        try:
            job = self.jobs.get(job_name)
            if not job:
                raise InvalidJobName(job_name)
            if job.isAlive():
                output["alive"] = True
                output["command_output"] = None
                output["elapsed_time"] = time.time() - job.start_time
            else:
                output["alive"] = False
                output["command_output"] = job.command_output
                output["elapsed_time"] = job.elapsed_time
            output["new_output"] = job.get_new_output()
        except Exception, err:
            output.update(self.dump_exception(username, err))
        return output

    def check_in(self):
        time_running = time.time() - self.start_time
        self.calls += 1
        return "time running: %5.2f; calls: %d" % (time_running, self.calls)

    def set_password(self, password):
        self.password = password

    def check_password(self):
        return self.password


from django.core.management import setup_environ
import settings
setup_environ(settings)

#Pyro.core.initServer()
daemon = Pyro.core.Daemon()
ns = Pyro.naming.NameServerLocator().getNS()
daemon.useNameServer(ns)

#daemon=Pyro.core.Daemon()
#print "The daemon runs on port:",daemon.port
#print "The object's uri is:",uri

uri = daemon.connect(Dispatcher(),"dispatcher")
print "Started server."
daemon.requestLoop()

