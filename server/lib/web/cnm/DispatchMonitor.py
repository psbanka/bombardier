"Keeps track of all the jobs running on the CNM"

import ServerLogMixin
from threading import Thread
from bombardier_core.static_data import OK, FAIL
import time

KILL_TIMEOUT = 20

def clutch_wrapper(func):
    """This is a decorator to turn on the clutch attribute, which tells
       the DispatchMonitor to not manipulate the queues if it is True"""
    def wrapper(self, *args, **kwargs):
        "Wrap a function, engaging the clutch before and disengaging after"
        self.clutch = True
        retval = func(self, *args, **kwargs)
        self.clutch = False
        return retval
    return wrapper

class DispatchMonitor(Thread, ServerLogMixin.ServerLogMixin):
    """Watches the dispatcher's jobs and starts queued ones if necessary"""
    def __init__(self):
        """
        this class watches the job queue for runnable jobs and runs them. it
        also watches the active jobs and puts them into the broken_jobs list
        or the finished_jobs list.

        job_queue -- list of job objects to be run
        active_jobs -- dictionary of jobs that are currently running
        broken_jobs -- dictionary of job_name/job that are failed
        finished_jobs -- dictionary of job_name/job that have successfully run

        """
        Thread.__init__(self)
        ServerLogMixin.ServerLogMixin.__init__(self)
        self.job_queue = []
        self.active_jobs = {}
        self.broken_jobs = {}
        self.finished_jobs = {}
        self.kill_switch = False
        self.clutch = False
        self.jobs_to_comment = []

    def kill_job(self, job_name):
        "We have a job that needs killin'"
        job = self.active_jobs.get(job_name)
        if job:
            job.kill_switch = True
            return OK
        return FAIL

    @clutch_wrapper
    def clear_broken(self, machine_name):
        "Clear all the broken jobs for a machine"
        jobs_cleared = []

        new_broken_jobs = {}
        for job_name in self.broken_jobs:
            broken_job = self.broken_jobs[job_name]
            if broken_job.machine_name != machine_name:
                new_broken_jobs[job_name] = broken_job
            else:
                jobs_cleared.append(job_name)
        self.broken_jobs = new_broken_jobs
        return jobs_cleared

    @clutch_wrapper
    def kill_jobs(self, machine_name):
        "We want to get rid of all jobs for a given machine"
        jobs_killed = []
        jobs_removed = []

        for job_name in self.active_jobs:
            job = self.active_jobs[job_name]
            if job.machine_name == machine_name:
                job.terminate()
                jobs_killed.append(job_name)

        new_job_queue = []
        for job in self.job_queue:
            if job.machine_name == machine_name:
                self.broken_jobs[job.name] = job
                jobs_removed.append(job.name)
            else:
                new_job_queue.append(job)
        self.job_queue = new_job_queue
        return jobs_killed, jobs_removed

    @clutch_wrapper
    def show_job_info(self, machine_name):
        """
        Get me a whole bunch of descriptive information about all a
        machine's jobs
        """
        self.server_log.info("show_job_info %s" % machine_name)
        output = {"active_jobs": [],
                  "pending_jobs": [],
                  "broken_jobs": [],
                  "jobs_to_comment": [],
                  "finished_jobs": [],
                 }
        for job_name in self.active_jobs:
            self.server_log.info("0 Checking %s." % job_name)
            job = self.active_jobs[job_name]
            job_str = "%s: %s" % (job_name, job.info())
            output["active_jobs"].append(job_str)
        for job_name in self.broken_jobs:
            job = self.broken_jobs[job_name]
            self.server_log.info("1 Checking %s." % job_name)
            job_str = "%s: %s" % (job_name, job.info())
            output["broken_jobs"].append(job_str)
        for job_name in self.finished_jobs:
            job = self.finished_jobs[job_name]
            self.server_log.info("2 Checking %s." % job_name)
            job_str = "%s: %s" % (job_name, job.info())
            output["finished_jobs"].append(job_str)
        for job in self.job_queue:
            job_str = "%s: %s" % (job.name, job.info())
            self.server_log.info("3 Checking %s." % job.name)
            output["pending_jobs"].append(job_str)
        for job in self.jobs_to_comment:
            self.server_log.info("4 Checking %s." % job.name)
            job_str = "%s: %s" % (job.name, job.info())
            output["jobs_to_comment"].append(job_str)
        return output

    def note_comment(self, job_name):
        "remove a job from the list of jobs that needs comments"
        new_jobs_to_comment = []
        for job in self.jobs_to_comment:
            if job.name != job_name:
                new_jobs_to_comment.append(job)
            else:
                self.server_log.info("Noting comment for %s." % job.name)
        self.jobs_to_comment = new_jobs_to_comment

    def get_machine_name(self, comment_job_name):
        "Gives you the name of the machine you're commenting on"
        for job in self.jobs_to_comment:
            if job.name == comment_job_name:
                return job.machine_name
        return None

    def get_uncommented_job_info(self, username):
        "Given a user name, return a list of job names"
        output = []
        for job in self.jobs_to_comment:
            if job.username == username:
                output.append((job.name, job.info()))
        return output

    def get_job_children(self, job):
        "Get all jobs which are waiting for this job to finish"
        job_children = []
        for job_name in self.active_jobs: 
            active_job = self.active_jobs[job_name]
            if job in active_job.get_all_predecessors():
                job_children.append(active_job.name)

        if job_children:
            return job_children

        for queued_job in self.job_queue:
            if job in queued_job.get_all_predecessors():
                job_children.append(queued_job.name)
        return job_children

    def clean_job_dependencies(self, broken_job):
        "When a job breaks, remove all jobs that depended on it"
        new_job_queue = []
        for job in self.job_queue:
            if broken_job in job.get_all_predecessors():
                self.broken_jobs[job.name] = job
            else:
                new_job_queue.append(job)
        self.job_queue = new_job_queue    

    def queue_reason(self, job):
        "A job is queued. What is it waiting on?"
        if not job.machine_interface.is_available():
            return job.machine_interface.job_name
        for predecessor_job in job.get_all_predecessors():
            if predecessor_job.name in self.broken_jobs:
                return predecessor_job.name
            if predecessor_job.name in self.active_jobs:
                return predecessor_job.name
            if predecessor_job in self.job_queue:
                return predecessor_job.name
        return None

    def is_runnable(self, job):
        "We want to know if a job can be run"
        if self.queue_reason(job):
            return False
        return True

    def _process_active_jobs(self):
        "Work with any jobs that are currently running"
        new_active_jobs = {}
        for job_name in self.active_jobs:
            job = self.active_jobs[job_name]
            if not job.isAlive():
                self.server_log.info("Job %s is no longer alive" % job_name)
                if job.status == OK:
                    self.finished_jobs[job.name] = job
                    self.server_log.info("it's finished")
                else:
                    self.server_log.info("it's broke")
                    self.broken_jobs[job.name] = job
                    self.clean_job_dependencies(job)
            else: # Job is alive
                if job.kill_switch:
                    job.kill()
                    self.broken_jobs[job_name] = job
                    del self.active_jobs[job_name]
                else:
                    new_active_jobs[job_name] = job
        self.active_jobs = new_active_jobs

    def _process_job_queue(self):
        "Starting runnable jobs"
        self.job_queue.sort()

        new_job_queue = []
        for job in self.job_queue:
            if self.is_runnable(job):
                job.start()
                self.active_jobs[job.name] = job
                if job.require_comment:
                    self.jobs_to_comment.append(job)
            else:
                new_job_queue.append(job)
        self.job_queue = new_job_queue

    def _process_broken_jobs(self):
        "Perform last rites on broken jobs"
        new_broken_jobs = {}
        for job_name in self.broken_jobs:
            job = self.broken_jobs[job_name]
            if job.isAlive():
                if (time.time() - job.end_time) > KILL_TIMEOUT:
                    job.terminate()
                    new_broken_jobs[job_name] = job
            if not job.acknowledged:
                new_broken_jobs[job_name] = job
        if not self.job_queue and not self.active_jobs:
            if self.broken_jobs:
                self.broken_jobs = new_broken_jobs

    def _process_finished_jobs(self):
        "See if anyone has collected status on finished jobs"
        new_finished_jobs = {}
        for job_name in self.finished_jobs:
            job = self.finished_jobs[job_name]
            if not job.acknowledged:
                new_finished_jobs[job_name] = job
        self.finished_jobs = new_finished_jobs

    def _log_queues(self):
        "Spam out our queue information"
        msg = ">> Queue: %s  /  active: %s  / broken: %s / finished: %s"
        msg = msg % (self.job_queue, self.active_jobs,
                     self.broken_jobs, self.finished_jobs)
        self.server_log.info(msg)

    def run(self):
        "Watchdog method for maintaining all queues"
        while not self.kill_switch:
            try:
                time.sleep(0.1)
                if self.clutch:
                    time.sleep(0.1)
                    continue
                self.job_queue.sort()
                #self._log_queues()
                self._process_active_jobs()
                self._process_job_queue()
                self._process_broken_jobs()
                self._process_finished_jobs()

            except Exception:
                traceback_lines = self.dump_exception("DispatchMonitor")
                for line in traceback_lines:
                    self.server_log.error(line, "DISPATCHER_MONITOR")


