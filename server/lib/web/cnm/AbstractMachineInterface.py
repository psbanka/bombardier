"abstract machine interface module"
#!/usr/bin/env python

import sys
from bombardier_core.static_data import OK, TRACEBACK, INFO
from Exceptions import JobAlreadySet
from ServerLogger import ServerLogger
DISCONNECTED = 0
CONNECTED    = 1
BROKEN       = 2

class AbstractMachineInterface:
    "Interface to a machine"
    def __init__(self, machine_config, server_log):
        self.server_log    = server_log
        self.polling_log   = None
        self.job_name      = None
        self.output_handle = sys.stdout
        self.status        = DISCONNECTED
        self.machine_name  = "CNM_Server"
        self.server_home   = None
        self.data          = {}
        self.user_name     = None
        self.connect_time  = 0
        self.set_data(machine_config)

    def set_data(self, machine_config):
        "extract machine_config for stuff we care about"
        self.machine_name  = machine_config.machine_name
        self.server_home   = machine_config.server_home

    def is_available(self):
        "Whether or not we can run a job on this interface"
        if self.job_name:
            return False
        return True

    def set_job(self, job_name):
        "Sets the job name / locks the object"
        if self.job_name:
            raise JobAlreadySet(self.job_name)
        self.job_name = job_name
        self.polling_log = ServerLogger(job_name)
        self.polling_log.add_stringio_handle()
        msg = "Binding interface %s to job %s" % (self.machine_name, job_name)
        self.server_log.info(msg, self.machine_name)

    def unset_job(self):
        "Un-Sets the job name / Un-locks the object"
        msg = "UN-Binding interface %s to job %s"
        msg = msg % (self.machine_name, self.job_name)
        self.job_name = None
        self.polling_log = None
        self.server_log.info(msg, self.machine_name)

    def get_new_logs(self):
        "Pulls logs from the polling_log object"
        if self.polling_log:
            return self.polling_log.get_new_logs()
        return []

    def traceback_output(self, source, msg):
        "Formats traceback messages"
        self.log(source, TRACEBACK, msg)

    def from_output(self, msg, severity=INFO):
        "Formats log messages"
        self.log(self.machine_name, severity, msg)

    def log(self, source, severity, msg):
        "Log outputter"
        formatted_msg = "<<%s|%d|%s>>\n" % (source, severity, msg)
        self.output_handle.write(formatted_msg)
        self.output_handle.flush()

    def terminate(self):
        "Kill all actions"
        pass

    def scp_dict(self, copy_dict):
        "copy data; should be renamed"
        pass

    def connect(self):
        "needed to connect to the machine"
        pass

    def freshen(self, job_name, require_status):
        "If we have a connection to the machine, make sure it's still usable"
        self.set_job(job_name)

    def chdir(self, path):
        "change directories"
        pass

    def log_raw_data(self, output_queue):
        "Transfer full lines from queue into the polling log"
        while '\n' in output_queue:
            position = len(output_queue.split('\n')[0])
            self.polling_log.debug(output_queue[:position-1])
            output_queue = output_queue[position+2:]
        return output_queue

    def gso(self, cmd, raise_on_error=True, cmd_debug=False):
        "Run a remote shell command"
        pass

    def run_cmd(self, command_string):
        "Run a remote shell command"
        return [OK, []]

    def dump_trace(self):
        "Pretty print a stack trace into the logs"
        pass

    def disconnect(self):
        "close our connection nicely"
        self.connect_time = 0
        self.status = DISCONNECTED



