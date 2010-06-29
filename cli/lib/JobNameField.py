""" JobNameField is a field that can become the name of a job
"""

import PinshCmd
import yaml
from bombardier_core.static_data import PARTIAL, COMPLETE, NO_MATCH
from SystemStateSingleton import SystemState
import re

system_state = SystemState()

class JobNameField(PinshCmd.PinshCmd):
    "This is a field which represents the name of a job running on a mahcine"
    def __init__(self, machine_name=None):
        "Allows a guy to find a job"
        PinshCmd.PinshCmd.__init__(self, "<job_name>")
        self.help_text = "<job_name>\tname of an active job"
        self.cmd_owner = 0
        self.machine_name = machine_name

    @classmethod
    def get_jobs(cls, server_output):
        "Return a list of jobs currently running on a machine"
        data = yaml.load('\n'.join(server_output))
        active_jobs = data.get("active_jobs", [])
        return active_jobs

    def preferred_names(self, command_line, index):
        '''Provide a list of names that the system would prefer to use, other
        than that which was typed in by the user. For example, 'sho mach localh'
        will return 'localhost' for the machine name if strict is off,
        otherwise, it will return 'localh'.

        '''
        post_data = {}
        _status, data = system_state.cnm_connector.dispatcher_status()
        if not data:
            return []
        job_names = data.get("active_jobs", [])
        job_names += data.get("broken_jobs", [])
        job_names += data.get("finished_jobs", [])
        command_line[index] = command_line[index].replace('"', '')
        if len(job_names) == 0:
            return []
        if self.machine_name:
            new_job_names = []
            matcher = re.compile("(\S+)\@(\S+)\-(\d+)")
            for job_name in job_names:
                _user, machine_name, _job_id = matcher.findall(job_name)
                if machine_name == self.machine_name:
                    new_job_names.append(job_name)
            job_names = new_job_names

        possible_names = []
        for job_name in job_names:
            if job_name.startswith(command_line[index]):
                possible_names.append(job_name)
        return possible_names

    def match(self, command_line, index):
        '''Determines if what has been typed in by the user matches a
        configuration item that the system is keeping track of.'''
        possible_matches = self.acceptable_names(command_line, index)
        if not possible_matches:
            return NO_MATCH, 1
        if len(possible_matches) > 1:
            return PARTIAL, 1
        return COMPLETE, 1


