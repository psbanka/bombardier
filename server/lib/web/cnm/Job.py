"""This is a job that runs underneath the dispatcher. Typically is going to
perform some kind of action on a remote machine. Could also be used to
run a build on the CNM server itself"""

import ServerLogMixin
from bombardier_core.static_data import OK, FAIL
from threading import Thread
import time
import StringIO, traceback

class Job(Thread, ServerLogMixin.ServerLogMixin):
    """Job class: Runs remote commands or copies files to a remote machine.
     Watches status of job"""
    def __init__(self, username, machine_name, job_id):
        ServerLogMixin.ServerLogMixin.__init__(self)
        self.name = "%s@%s-%d" % (username, machine_name, job_id)
        self.username = username
        self.machine_name = machine_name
        self.copy_dict = {}
        self.commands = []
        self.require_status = True
        self.machine_interface = None
        self.start_time = None
        self.end_time = None
        self.command_status = OK
        self.command_output = []
        self.output_pointer = 0
        self.predecessors = []
        self.final_logs = []
        Thread.__init__(self)
        self.complete_log = ''
        self.status = OK
        self.kill_switch = False
        self.acknowledged = False
        self.description = None
        self.require_comment = False

    def info(self):
        "Tell me something about all your commands, job."
        if self.description:
            return self.description
        output = []
        for command in self.commands:
            output.append(command.info())
        return ": ".join(output)

    def record_exception(self):
        "Called when an exception is trapped on a job"
        self.command_status = FAIL
        self.command_output.append( self.dump_exception(self.username) )

    def setup(self, machine_interface, commands, copy_dict,
              require_status, predecessors):
        "Set the job up to be used"
        self.copy_dict = copy_dict
        self.commands = commands
        self.require_status = require_status
        self.machine_interface = machine_interface
        self.predecessors = predecessors

    def get_all_predecessors(self):
        "find out all the possible jobs I'm dependent on"
        all_predecessors = list(self.predecessors)
        for job in self.predecessors:
            all_predecessors += job.get_all_predecessors()
        return all_predecessors

    def __cmp__(self, other):
        """Compare myself to another job to see if I should run first
        or if the other job should run first"""
        if other in self.get_all_predecessors():
            return 1
        elif self in other.get_all_predecessors():
            return -1
        else:
            return 0

    def __repr__(self):
        "Representation of this class"
        return self.name

    def _get_elapsed_time(self):
        "Figure out how long this job took to run"
        if self.isAlive:
            if self.start_time != None:
                return time.time() - self.start_time 
            else:
                return 0
        else:
            return self.end_time - self.start_time

    def get_status_dict(self):
        "Returns the job's current status data"
        output = {}
        output["alive"] = self.isAlive()
        output["command_status"] = self.command_status
        output["job_name"] = self.name
        output["elapsed_time"] = self._get_elapsed_time()
        output["complete_log"] =  self.complete_log
        output["command_output"] = self.command_output
        if self.isAlive():
            output["new_output"] = self.machine_interface.get_new_logs()
        else:
            output["new_output"] = self.final_logs
        return output

    def terminate(self):
        "Be crusty about killing a job"
        if self.isAlive():
            self.machine_interface.terminate()
        self.end_time = time.time()

    def kill(self):
        "Nicely end a job that is currently running"
        self.machine_interface.control_c()
        self.end_time = time.time()
        self.status = FAIL

    def run(self):
        "Runs commands in command list and watches status"
        self.start_time = time.time()
        try:
            self.machine_interface.freshen(self.name, self.require_status)
            self.machine_interface.scp_dict(self.copy_dict)
            self.server_log.info("Starting...", self.name)

            for command in self.commands:
                status = self.run_command(command)
                if status != OK:
                    self.status = FAIL
                    break
            self.server_log.info("Finishing", self.name)
            self.final_logs = self.machine_interface.polling_log.get_final_logs()
            polling_log = self.machine_interface.polling_log 
            self.complete_log = polling_log.get_complete_log()
        except Exception:
            exc = StringIO.StringIO()
            traceback.print_exc(file=exc)
            exc.seek(0)
            data = exc.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg = "%% %s" % line
                self.server_log.error(ermsg, self.name)
            self.command_status = FAIL
            msg = "Exception in server. Check log for details"
            self.command_output.append(msg)
            self.status = FAIL
        self.machine_interface.unset_job()
        self.end_time = time.time()

    def run_command(self, command):
        "Run a command and get its status"
        try:
            msg = "Processing command: %s" % command.name, self.name
            self.server_log.info(msg)
            status, output = command.execute(self.machine_interface)
            self.command_status = status
            self.command_output = output
            return status
        except Exception:
            msg = "Command %s: Failed to run %s"
            msg = msg % (command.name, command.info())
            self.server_log.error( msg, self.name)
            self.command_status = FAIL
            return FAIL

