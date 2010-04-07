"Dispatcher module"
import Pyro.core
import time, os
import re
import glob
import yaml
import Pyro.naming

from bombardier_core.static_data import OK, FAIL, WAIT
from bombardier_core.Cipher import Cipher, DecryptionException
from bombardier_core.static_data import ACTION_LOOKUP

from Exceptions import InvalidJobName, JoinTimeout
from Exceptions import QueuedJob

from MachineConfig import MachineConfig
from BombardierMachineInterface import BombardierMachineInterface
from LocalMachineInterface import LocalMachineInterface 
import Job
import ServerLogMixin
from Commands import BuildCommand, ShellCommand, BombardierCommand
import DispatchMonitor

LOCAL_MACHINE_NAME = "CNM_Server"

class Dispatcher(Pyro.core.ObjBase, ServerLogMixin.ServerLogMixin):
    "Dispatcher class, manages jobs on remote machines"
    def __init__(self):
        Pyro.core.ObjBase.__init__(self)
        ServerLogMixin.ServerLogMixin.__init__(self)
        self.start_time = time.time()
        self.password = None
        self.server_home = None
        self.next_job = 1
        self.machine_interface_pool = {}
        self.monitor = DispatchMonitor.DispatchMonitor()
        self.new_jobs = {}
        self.monitor.start()
        self.jobs_to_comment = {}

    def set_server_home(self, username, server_home):
        "Set server_home"
        self.server_log.info("Setting server home: %s" % server_home, username)
        self.server_home = server_home

    def get_machine_interface(self, username, machine_name):
        "Returns an interface to a machine, creating a new one if needed"

        if machine_name == LOCAL_MACHINE_NAME:
            machine_config = MachineConfig(LOCAL_MACHINE_NAME, self.password,
                                           self.server_home)
            machine_interface = LocalMachineInterface(machine_config,
                                                      self.server_log)
            return machine_interface 

        machine_config = MachineConfig(machine_name, self.password,
                                       self.server_home)
        machine_config.merge()
        if self.password:
            machine_config.decrypt_config()
        machine_interface = None
        if machine_name in self.machine_interface_pool:
            msg = "Reusing existing connection to %s" % machine_name
            self.server_log.info(msg, username)
            machine_interface = self.machine_interface_pool[machine_name]
            machine_interface.set_data(machine_config)
        else:
            msg = "Instantiating a MachineInterface for %s" % machine_name
            self.server_log.info(msg, username)
            machine_interface = BombardierMachineInterface(machine_config,
                                                           self.server_log)
            self.machine_interface_pool[machine_name] = machine_interface
        return machine_interface 

    def _create_next_job(self, username, machine_name):
        "Creates a job, keeping track of the master job number"
        job = Job.Job(username, machine_name, self.next_job)
        self.new_jobs[job.name] = job
        self.next_job += 1
        return job

    def _setup_job(self, job, commands, copy_dict, require_status,
                  job_predecessors = []):
        "When you want a job to run after another job is finished"
        machine_interface = self.get_machine_interface(job.username,
                                                       job.machine_name)
        job.setup(machine_interface, commands, copy_dict, require_status,
                 job_predecessors)

    def queue_job(self, job_name): 
        "Take a job and get it ready to be run"
        self.server_log.info("Getting: %s (%s)" % (job_name, type(job_name)))
        job = self.new_jobs[job_name]
        self.server_log.info("Here is the job: %s" % job)
        for predecessor_job in job.predecessors:
            if predecessor_job.name in self.monitor.broken_jobs:
                reason = "Predecessor job %s is broken."
                reason = reason % predecessor_job.name
                predecessor_job.command_status = FAIL
                predecessor_job.command_output.append(reason)
                continue
            if not predecessor_job in self.monitor.job_queue:
                if not predecessor_job.name in self.monitor.active_jobs:
                    if not predecessor_job.name in self.monitor.finished_jobs:
                        predecessor_job.command_status = FAIL
                        reason = "Cannot find predecessor job %s"
                        reason = reason % predecessor_job.name
                        predecessor_job.command_output.append(reason)
        if job.command_status == OK:
            self.monitor.job_queue.append(job)
        else:
            self.monitor.broken_jobs[job_name] = job
            self.server_log.info("BROKEN %s" % job_name)

    def get_job_status(self, job_name):
        "Find out how the job is doing"
        job = self.new_jobs[job_name]
        return job.get_status_dict()
    
    def unpush_job(self, username, machine_name):
        "remove cleartext configuration data to the remote machine for testing"
        commands = []
        job = self._create_next_job(username, machine_name)
        job.description = "Un-push"
        try:
            machine_interface = self.get_machine_interface(username,
                                                           machine_name)
            config_path = os.path.join(machine_interface.spkg_dir,
                                       machine_interface.machine_name)
            clear = ShellCommand("Removing cleartext configuration file", 
                                 "rm -f config.yml", config_path)
            commands = [clear]
            self._setup_job(job, commands, {}, False)
        except Exception:
            job.record_exception()
        return job.name

    def push_job(self, username, machine_name):
        "push cleartext configuration data to the remote machine for testing"
        job = self._create_next_job(username, machine_name)
        job.description = "Push"
        try:
            machine_interface = self.get_machine_interface(username,
                                                           machine_name)
            spkg_dir = machine_interface.spkg_dir
            src = os.path.join("machine", "%s.yml" % machine_name)
            dst = os.path.join(spkg_dir, machine_interface.machine_name,
                               "config.yml")
            copy_dict = {"config": [ src, dst ]}
            self._setup_job(job, [], copy_dict, False)
        except Exception:
            job.record_exception()
        msg = "This is the job we're making: %s (%s)"
        msg = msg % (job.name, len(job.name))
        self.server_log.info(msg)
        return job.name

    def init_job(self, username, machine_name):
        "Initialize a bombardier client install"
        job = self._create_next_job(username, machine_name)
        job.description = "Initialize"
        try:
            machine_interface = self.get_machine_interface(username,
                                                           machine_name)
            spkg_dir = machine_interface.spkg_dir
            commands = []
            if machine_interface.platform == "win32":
                rtool = "regtool"
                bom_hkey = "/HKEY_LOCAL_MACHINE/SOFTWARE/Bombardier"
                cmd = "export SPKG_DOS_DIR=$(cygpath -w %s);" % spkg_dir
                cmd += '%s add -v "%s";' % ( rtool, bom_hkey )
                cmd += '%s set -v "%s/InstallPath" $SPKG_DOS_DIR'
                cmd = cmd % ( rtool, bom_hkey )
            else:
                cmd = 'echo spkg_path: %s > /etc/bombardier.yml' % spkg_dir
            set_spkg_config = ShellCommand("Setting spkg path", cmd, '.')
            bombardier_init = BombardierCommand("init", None, None)

            commands = [set_spkg_config, bombardier_init]
            job.description = "INIT"
            job.require_comment = True
            self._setup_job(job, commands, {}, False)
        except Exception:
            job.command_output = self.dump_exception(username)
            job.command_status = FAIL
        return job.name

    def bom_job(self, username, machine_name, action_string, status_required):
        "Run a bom level job on a machine"
        job = self._create_next_job(username, machine_name)
        try:
            if action_string == 'reconcile':
                job.require_comment = True
            bombardier_recon = BombardierCommand(action_string, None, None)
            commands = [bombardier_recon]
            self._setup_job(job, commands, {}, status_required)
        except Exception:
            job.command_output = self.dump_exception(username)
            job.command_status = FAIL
        return job.name

    def check_status_job(self, username, machine_name):
        "Check a machine's install status against its bom"
        return self.bom_job(username, machine_name, "check_status",
                                              status_required=False)

    def reconcile_job(self, username, machine_name):
        "Reconciles a machine to its bom"
        return self.bom_job(username, machine_name, "reconcile",
                                            status_required=True)

    def package_build_job(self, username, package_name, svn_user,
                          svn_password, debug, prepare):
        '''
        Looks at a package .yml file and revises data regarding it
        username -- the name of the user issuing the command
        package_name -- the name of the package to deal with
        svn_user -- svn credentials
        svn_password -- svn credentials
        '''
        job = self._create_next_job(username, LOCAL_MACHINE_NAME)
        try:
            build_cmd = BuildCommand(package_name, svn_user, svn_password,
                                     debug, prepare)
            commands = [build_cmd]
            self._setup_job(job, commands, {}, True)
        except Exception:
            job.command_output = self.dump_exception(username)
            job.command_status = FAIL
        return job.name

    def package_action_job(self, username, package_name, action_string, 
                           machine_name):
        '''
        Runs a bc command for a certain machine and package
        username -- the name of the user issuing the command
        package_name -- the name of the package to deal with
        action_string -- one of the following: uninstall, configure, install,
                         verify, reconcile, check_status, execute, fix, purge,
                         dry_run, or init
        machine_name -- name of the machine to run the job on
        '''
        job = self._create_next_job(username, machine_name)
        job.require_comment = True
        script_name = ''
        if action_string not in ACTION_LOOKUP:
            script_name = action_string
            action_string = "execute"
        if package_name:
            if action_string == "check_status" or action_string == "reconcile":
                script_name = action_string
                action_string = "execute"
        try:
            bom_cmd = BombardierCommand(action_string, package_name, 
                                        script_name)
            commands = [bom_cmd]

            self._setup_job(job, commands, {}, True)
        except Exception:
            job.command_output = self.dump_exception(username)
            job.command_status = FAIL
        return job.name

    def dist_job(self, username, machine_name, dist_name):
        "Job that unpacks and installs a distutils package"
        job = self._create_next_job(username, machine_name)
        job.description = "DIST: %s" % dist_name
        job.require_comment = True
        try:
            unpack = ShellCommand("Unpacking %s on the client" % dist_name, 
                             "tar -xzf %s.tar.gz" % dist_name, "~")
            install = ShellCommand("Installing client libraries...",
                              "python setup.py install",
                              "~/%s" % dist_name)
            commands = [unpack, install]
            src_file = dist_name+".tar.gz"
            copy_dict = {"dist": [os.path.join("dist", src_file), '.']}
            self._setup_job(job, commands, copy_dict, False)
        except Exception:
            job.command_output = self.dump_exception(username)
            job.command_status = FAIL
        return job.name

    def enable_job(self, username, machine_name, password):
        "Set up ssh shared keys with a remote machine"
        job = self._create_next_job(username, machine_name)
        job.description = "Enable"
        try:
            machine_interface = self.get_machine_interface(username,
                                                           machine_name)
            machine_interface.ssh_pass = password
            public_key = "id_dsa.pub"
            copy_dict = { "admin" : [os.path.join("admin", public_key), '.'] }
            ssh_keys_dir = "~/.ssh"
            ssh_keys_dir_cmd = "[ -e %s ] || mkdir %s && chmod 700 %s" \
                             % (ssh_keys_dir, ssh_keys_dir, ssh_keys_dir)
            make_ssh_dir = ShellCommand("Creating .ssh dir if necessary",
                                        ssh_keys_dir_cmd, "~")

            cat_cmd = "cat ~/%s >> authorized_keys2" % public_key
            cat_key = ShellCommand("Adding ssh key to %s" % machine_name,
                                   cat_cmd, ssh_keys_dir)

            commands = [make_ssh_dir, cat_key]
            self._setup_job(job, commands, copy_dict, False)
        except Exception:
            job.command_output = self.dump_exception(username)
            job.command_status = FAIL
        return job.name

    def disable_job(self, username, machine_name):
        "Remove shared ssh key from a remote machine"
        job = self._create_next_job(username, machine_name)
        job.description = "Disable"
        try:
            public_key = "id_dsa.pub"
            auth_keys = ".ssh/authorized_keys2"
            tmp_auth = "tmp_auth_keys2"
            copy_dict = { "admin" : [os.path.join("admin", public_key), '.'] }

            filter_cmd = """grep -v "$( awk '{print $3}' < %s""" % public_key
            filter_cmd += """)" < %s > %s""" % ( auth_keys, tmp_auth )
            filter_sc = ShellCommand("Filtering ssh key from %s" % machine_name,
                                      filter_cmd, '~')
            overwrite_cmd = "cat %s > %s && rm %s"
            overwrite_cmd = overwrite_cmd  % ( tmp_auth, auth_keys, tmp_auth )
            overwrite_sc = ShellCommand("Overwriting authorized_keys",
                                     overwrite_cmd, '~')
            
            commands = [filter_sc, overwrite_sc]
            self._setup_job(job, commands, copy_dict, False)
        except Exception:
            job.command_output = self.dump_exception(username)
            job.command_status = FAIL
        return job.name

    def test_job(self, username, machine_name):
        "Simple test job"
        job = self._create_next_job(username, machine_name)
        job.description = "Self-test"
        try:
            cmd = 'for i in 1 2 3 4; do sleep 1; echo "Testing $i/4"; done'
            commands = [ShellCommand("self_test", cmd, '.')]
            self._setup_job(job, commands, {}, False)
        except Exception:
            job.command_output = self.dump_exception(username)
            job.command_status = FAIL
        return job.name

    def terminate(self, username):
        "Close connections for machine interfaces in interface pool"
        output = {"command_status": OK}
        try:
            self.monitor.kill_switch = True
            for machine_name in self.machine_interface_pool:
                machine_interface = self.machine_interface_pool[machine_name]
                msg = "Requested connection termination to %s" % machine_name
                self.server_log.warning(msg, username)
                status = machine_interface.terminate()
                output[machine_name] = status
            self.machine_interface_pool = {}
            self.monitor.join()
        except Exception:
            output.update(self.dump_exception(username))
        return output

    def get_jobs_pending_comments(self, username):
        "find all the jobs this user needs to comment on"
        job_info = self.monitor.get_uncommented_job_info(username)
        output = {"job_info": job_info}
        return output

    def get_machine_name(self, comment_job_name):
        "get the name of a machine, based on job name"
        return self.monitor.get_machine_name(comment_job_name)

    def note_comment(self, job_name):
        "record that the user is making a comment on a job"
        self.monitor.note_comment(job_name)

    def job_join(self, username, job_name, timeout):
        "Wait for a job to finish"
        require_comment = False
        try:
            self.server_log.info("Attempting to join %s" % job_name)
            job = self.get_job(job_name)
            if job:
                self.server_log.info("found the job")
            else:
                self.server_log.info("can't find the job")
            start_time = time.time()
            end_time = start_time + timeout
            if not job:
                raise InvalidJobName(job_name)
            while job_name in self.monitor.active_jobs:
                self.server_log.info("waiting for %s to time out" % job_name)
                if time.time() > end_time:
                    raise JoinTimeout(job_name, timeout)
                time.sleep(1)
            job.acknowledged = True
            if job.require_comment:
                require_comment = True
            self.server_log.info("finishing join %s" % job_name)
        except QueuedJob:
            output = {"command_status": WAIT,
                     }
            return output
        except Exception:
            self.server_log.info("exception in join %s" % job_name)
            output = {"command_status": FAIL,
                      "command_output": self.dump_exception(username),
                     }
            return output
        output = job.get_status_dict()
        output["jobs_requiring_comment"] = self.monitor.get_uncommented_job_info(username)
        output["children"] = self.monitor.get_job_children(job)
        return output

    def get_job(self, job_name):
        "Get a job from the appropriate bucket, whetever that is"
        job = self.monitor.active_jobs.get(job_name)
        if not job:
            job = self.monitor.finished_jobs.get(job_name)
            if not job:
                job = self.monitor.broken_jobs.get(job_name)
                if not job:
                    for job in self.monitor.job_queue:
                        if job_name == job.name:
                            predecessor = self.monitor.queue_reason(job)
                            raise QueuedJob(predecessor)
                    raise InvalidJobName(job_name)
        return job

    def job_poll(self, username, job_names):
        "Poll a job for status info and log data"
        job_status_dict = {}
        for job_name in job_names:
            try:
                job = self.get_job(job_name)
                job_status_dict[job_name] = job.get_status_dict()
            except QueuedJob, qj:
                job_status_dict[job_name] = {"command_status": "QUEUED",
                                             "pending_job": qj.job_name}
        return job_status_dict

    def job_kill(self, username, job_name):
        "Be mean to a job and get rid of it"
        status = FAIL
        self.server_log.info("%s killing job %s" % (username, job_name))
        try:
            status = self.monitor.kill_job(job_name)
        except Exception:
            return {"command_status": FAIL}
        return {"command_status": status}

    def clear_broken(self, username, machine_name):
        "clear out the broken dictionary for a given machine"
        jobs_cleared = self.monitor.clear_broken(machine_name)
        command_output = []
        for job_name in jobs_cleared:
            command_output.append("%s cleared" % job_name)
        output = {"command_output": command_output,
                  "command_status": OK
                 }
        return output

    def show_jobs(self, username, machine_name):
        "show detailed job information for a machine"
        job_info = self.monitor.show_job_info(machine_name)
        output = {"command_output": '',
                  "command_status": OK,
                  "job_info": job_info,
                 }
        return output

    def stop_all_jobs(self, username, machine_name):
        "stop all jobs for a given machine"
        jobs_killed, jobs_removed = self.monitor.kill_jobs(machine_name)
        command_output = []
        for job_name in jobs_killed:
            command_output.append("%s killed" % job_name)
        for job_name in jobs_removed:
            command_output.append("%s removed" % job_name)
        output = {"command_output": command_output,
                  "command_status": OK
                 }
        return output

    def check_status(self):
        "Return elapsed time"
        time_running = time.time() - self.start_time
        return { "uptime": time_running,
                 "job_queue": str(self.monitor.job_queue),
                 "active_jobs": self.monitor.active_jobs.keys(),
                 "finished_jobs": self.monitor.finished_jobs.keys(),
                 "broken_jobs": self.monitor.broken_jobs.keys(),
               }

    def set_password(self, password):
        "Set password for configuration item encryption"
        status, validate_output = self._validate_password(password)
        if status == OK:
            self.server_log.info("Setting configuration password.")
            self.password = password
            for machine_name in self.machine_interface_pool:
                machine_config = MachineConfig(machine_name, self.password,
                                               self.server_home)
                machine_config.merge()
                machine_config.decrypt_config()
                machine_interface = self.machine_interface_pool[machine_name]
                self.server_log.info("Decrypting config for %s" % machine_name)
                machine_interface.set_data(machine_config)

        output = {"command_status": status,
                  "command_output": validate_output}
        return output

    def check_password(self):
        "Returns configuration password"
        return self.password

    def _validate_password(self, password):
        "Very shallow password validation"
        lazy_dog = "the_quick_brown_fox_jumped_over_the_lazy_dog\n"
        test_decrypt_file = os.path.join(self.server_home, 'admin',
                                         'encryption_validation.yml')
        if not os.path.isfile(test_decrypt_file):
            msg = "%s doesn't exist, creating..." % test_decrypt_file
            self.server_log.warning( msg )
            cipher = Cipher(password)
            enc_lazy = cipher.encrypt_string(lazy_dog)
            enc_dict = { "enc_test" : enc_lazy }
            open( test_decrypt_file, 'w' ).write(yaml.dump( enc_dict ))
        try:
            cipher_dict = yaml.load(open(test_decrypt_file, 'r').read())
        except IOError:
            return FAIL, "Encryption not set up properly. %s not readable"
        try:
            cipher = Cipher(password)
            clear_dict = cipher.decrypt_dict(cipher_dict)
        except DecryptionException:
            return FAIL, ["Invalid configuration key."]
        if clear_dict.get("test") == lazy_dog:
            return OK, ['Configuration key set.']
        return FAIL, ["Invalid configuration key."]

    def _find_latest_file(self, matcher):
        "Looks in the dist directory for the newest version of a file"
        dist_glob_path = os.path.join(self.server_home, "dist", "*.tar.gz")
        latest_filename = ''
        max_version = 0
        for filename in glob.glob(dist_glob_path):
            matches = matcher.findall(filename)
            if matches:
                new_version = int(matches[0])
                if new_version > max_version:
                    latest_filename = filename
                    max_version = new_version
        return latest_filename

    def _dist_install_job(self, search_string, username, machine_name):
        """Find the latest version of a distribution file and create a job to
        deploy it on a machine"""
        matcher = re.compile(search_string)
        newest_dist = self._find_latest_file(matcher)
        newest_dist = newest_dist.split('.tar.gz')[0]
        newest_dist = newest_dist.split(os.path.sep)[-1]
        job_name = self.dist_job(username, machine_name, newest_dist)
        return job_name

    def setup_machine(self, username, machine_name, password):
        """Sets up a machine. This requires
           (1) enable
           (2) set up the Bombardier client/core libraries
           (3) Install PyYAML
           (4) run an 'init' job
           (5) run a 'check_status' job
        """
        job1_name = self.enable_job(username, machine_name, password)
        job1 = self.new_jobs[job1_name]

        dist_name = "bombardier_core\-1\.00\-(\d+)\.tar\.gz"
        job2_name = self._dist_install_job(dist_name, username, machine_name)
        job2 = self.new_jobs[job2_name]
        job2.predecessors = [job1]

        dist_name = "bombardier_client\-1\.00\-(\d+)\.tar\.gz"
        job3_name = self._dist_install_job(dist_name, username, machine_name)
        job3 = self.new_jobs[job3_name]
        job3.predecessors = [job2]

        dist_name = "PyYAML-3.(\d+).tar.gz"
        job4_name = self._dist_install_job(dist_name, username, machine_name)
        job4 = self.new_jobs[job4_name]
        job4.predecessors = [job3]

        job5_name = self.init_job(username, machine_name)
        job5 = self.new_jobs[job5_name]
        job5.predecessors = [job4]

        job6_name = self.package_action_job(username, '', "check_status",
                                            machine_name)
        job6 = self.new_jobs[job6_name]
        job6.predecessors = [job5]
        job_names = [job1_name, job2_name, job3_name, job4_name,
                     job5_name, job6_name]
        for job_name in job_names:
            self.queue_job(job_name)
        return job1_name

if __name__ == '__main__':
    from django.core.management import setup_environ
    import settings
    setup_environ(settings)

    Pyro.core.initServer()
    daemon = Pyro.core.Daemon()
#    ns = Pyro.naming.NameServerLocator().getNS()
#    daemon.useNameServer(ns)
#
#    #daemon=Pyro.core.Daemon()
    uri = daemon.connect(Dispatcher(), "dispatcher")
    print "The daemon runs on port:", daemon.port
    print "The object's uri is:", uri
#
    print "Started server."
    daemon.requestLoop()
#
