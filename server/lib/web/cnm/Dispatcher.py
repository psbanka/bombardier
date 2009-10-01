"Dispatcher module"
import Pyro.core
import time, os
import yaml
import Pyro.naming
from MachineConfig import MachineConfig
from BombardierMachineInterface import BombardierMachineInterface
from bombardier_core.static_data import OK, FAIL
from bombardier_core.libCipher import encrypt, decrypt, DecryptionException
from threading import Thread
import StringIO, traceback
import ServerLogger

from Exceptions import InvalidJobName, JoinTimeout
from Exceptions import InvalidAction
from bombardier_core.static_data import ACTION_LOOKUP

PENDING = 4

def exception_dumper(func):
    "This is a troubleshooting tool for exceptions"
    #argnames = func.func_code.co_varnames[:func.func_code.co_argcount]
    fname = func.func_name
    def traceback_func(*args, **kwargs):
        "Catching exceptions in traced function"
        try:
            print "watching the %s function" % fname
            return func(*args, **kwargs)
        except Exception:
            exc = StringIO.StringIO()
            traceback.print_exc(file=exc)
            exc.seek(0)
            data = exc.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg = "%% %s" % line
                print ermsg
    return traceback_func


class AbstractCommand:
    "Abstract class for a dispatched command"
    def __init__(self, name):
        self.name = name
        self.dump_config = False
        self.status = PENDING

    def execute(self, machine_interface):
        "Virtual function for executing command"
        pass

    def info(self):
        "Default info function"
        return self.name

class ShellCommand(AbstractCommand):
    "Remote shell command"
    def __init__(self, name, cmd, working_dir):
        AbstractCommand.__init__(self, name)
        self.working_dir = working_dir
        self.cmd = cmd

    def execute(self, machine_interface):
        "Run the command on a remote machine"
        machine_interface.chdir(self.working_dir)
        return machine_interface.run_cmd(self.cmd)

    def info(self):
        "Return command to be run on the remote machine"
        return self.cmd

class BombardierCommand(AbstractCommand):
    """Bombardier command is a command running bc.py for 
       package or status actions"""
    def __init__(self, action_string, package_name=None,
                 script_name = '', package_revision=None, debug=False):
        action_const = ACTION_LOOKUP.get(action_string.lower().strip())
        if action_const == None:
            raise InvalidAction(package_name, action_string)
        name = "Bombardier-%s-%s" % (action_string, package_name)
        AbstractCommand.__init__(self, name)
        self.action = action_const
        self.package_name = package_name
        self.script_name = script_name
        self.package_revision = package_revision
        self.debug = debug

    def execute(self, machine_interface):
        "Run bc command"
        return machine_interface.take_action(self.action, self.package_name,
                                             self.script_name,
                                             self.package_revision, self.debug)
                                         

class Job(Thread):
    """Job class: Runs remote commands or copies files to a remote machine.
     Watches status of job"""
    def __init__(self, name, machine_interface, copy_dict, commands):
        self.name = name
        self.copy_dict = copy_dict
        self.commands = commands
        self.server_log = machine_interface.server_log
        self.machine_interface = machine_interface
        self.machine_interface.set_job(name)
        self.start_time = None
        self.elaped_time = None
        self.command_status = FAIL
        self.command_output = None
        self.output_pointer = 0
        self.final_logs = []
        self.elapsed_time = 0
        Thread.__init__(self)
        self.machine_interface.freshen()
        self.complete_log = ''

    def run(self):
        "Runs commands in command list and watches status"
        self.server_log.info("Starting...", self.name)
        self.start_time = time.time()
            
        for command in self.commands:
            self.run_command(command)
        self.server_log.info("Finishing", self.name)
        self.elapsed_time = time.time() - self.start_time
        self.final_logs = self.machine_interface.polling_log.get_final_logs()
        polling_log = self.machine_interface.polling_log 
        self.complete_log = polling_log.get_complete_log()
        self.machine_interface.unset_job()

    def run_command(self, command):
        "Run a command and get its status"
        try:
            msg = "Processing command: %s" % command.name, self.name
            self.server_log.info(msg)
            status, output = command.execute(self.machine_interface)
            self.command_status = status
            self.command_output = output
        except Exception:
            self.elapsed_time = time.time() - self.start_time
            msg = "Command %s: Failed to run %s"
            msg = msg % (command.name, command.info())
            self.server_log.error( msg, self.name)
            exc = StringIO.StringIO()
            traceback.print_exc(file=exc)
            exc.seek(0)
            data = exc.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg = "%% %s" % line
                self.server_log.error(ermsg, self.name)

class Dispatcher(Pyro.core.ObjBase):
    "Dispatcher class, manages jobs on remote machines"
    def __init__(self):
        Pyro.core.ObjBase.__init__(self)
        self.start_time = time.time()
        self.password = None
        self.server_home = None
        self.jobs = {}
        self.next_job = 1
        self.server_log = ServerLogger.ServerLogger("Dispatcher",
                                                    use_syslog=True)
        #self.server_log.add_std_err()
        self.machine_interface_pool = {}

    def dump_exception(self, username):
        """Pretty print an exception into the server_log,
         and return traceback info"""
        exc = StringIO.StringIO()
        traceback.print_exc(file=exc)
        exc.seek(0)
        data = exc.read()
        ermsg = ''
        traceback_data = []
        for line in data.split('\n'):
            traceback_data.append(line)
            ermsg = "%% %s" % line
            self.server_log.error(ermsg, username)
        return {"status": FAIL, "traceback": traceback_data}

    def set_server_home(self, username, server_home):
        "Set server_home"
        self.server_log.info("Setting server home: %s" % server_home, username)
        self.server_home = server_home

    def get_machine_interface(self, username, machine_name):
        "Returns an interface to a machine, creating a new one if needed"
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
    
    def start_job(self, username, machine_interface, commands, copy_dict=None):
        "Add a job into the jobs dictionary and start it"
        output = {"status": OK}
        if not copy_dict:
            copy_dict = {}
        try:
            machine_name = machine_interface.machine_name
            job_name = "%s@%s-%d" % (username, machine_name, self.next_job)
            job = Job(job_name, machine_interface, copy_dict, commands)
            self.jobs[job_name] = job
            self.next_job += 1
            machine_interface.scp_dict(copy_dict)
            job.start()
            output["job_name"] = job.name
        except Exception:
            output.update(self.dump_exception(username))
            output["status"] = FAIL
        return output

    def init_job(self, username, machine_name):
        "Initialize a bombardier client install"
        output = {"status": OK}
        copy_dict = {}
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
                cmd = 'echo spkgPath: %s > /etc/bombardier.yml' % spkg_dir
            set_spkg_config = ShellCommand("Setting spkg path", cmd, '.')
            bombardier_init = BombardierCommand("init")

            commands = [set_spkg_config, bombardier_init]
        except Exception:
            output.update(self.dump_exception(username))
            output["status"] = FAIL
            return output
        return self.start_job(username, machine_interface, commands, copy_dict)

    def bom_job(self, username, machine_name, action_string):
        "Run a bom level job on a machine"
        output = {"status": OK}
        copy_dict = {}
        try:
            machine_interface = self.get_machine_interface(username,
                                                           machine_name)
            bombardier_recon = BombardierCommand(action_string)
            commands = [bombardier_recon]
        except Exception:
            output.update(self.dump_exception(username))
            output["status"] = FAIL
            return output
        return self.start_job(username, machine_interface, commands, copy_dict)

    def status_job(self, username, machine_name):
        "Check a machine's install status against its bom"
        return self.bom_job(username, machine_name, "status")

    def reconcile_job(self, username, machine_name):
        "Reconciles a machine to its bom"
        return self.bom_job(username, machine_name, "reconcile")

    def package_action_job(self, username, package_name, action_string, 
                           machine_name, package_revision=None):
        "Runs a bc command for a certain machine and package"
        output = {"status": OK}
        try:
            machine_interface = self.get_machine_interface(username,
                                                           machine_name)
            bom_cmd = BombardierCommand(action_string,
                                        package_name=package_name, 
                                        package_revision=package_revision)
            commands = [bom_cmd]

        except Exception:
            output.update(self.dump_exception(username))
            output["status"] = FAIL
            return output
        return self.start_job(username, machine_interface, commands, {})

    def dist_job(self, username, machine_name, dist_name):
        "Job that unpacks and installs a distutils package"
        output = {"status": OK}
        try:
            machine_interface = self.get_machine_interface(username,
                                                           machine_name)
            unpack = ShellCommand("Unpacking %s on the client" % dist_name, 
                             "tar -xzf %s.tar.gz" % dist_name, "~")
            install = ShellCommand("Installing client libraries...",
                              "python setup.py install",
                              "~/%s" % dist_name)
            commands = [unpack, install]
            src_file = dist_name+".tar.gz"
            copy_dict = {"dist": [src_file]}
        except Exception:
            output.update(self.dump_exception(username))
            output["status"] = FAIL
            return output
        return self.start_job(username, machine_interface, commands, copy_dict)

    def enable_job(self, username, machine_name, password):
        "Set up ssh shared keys with a remote machine"
        output = {"status": OK}
        try:
            machine_interface = self.get_machine_interface(username,
                                                           machine_name)
            machine_interface.ssh_pass = password
            public_key = "id_dsa.pub"
            copy_dict = { "admin" : [public_key] }
            ssh_keys_dir = "~/.ssh"
            ssh_keys_dir_cmd = "[ -e %s ] || mkdir %s && chmod 700 %s" \
                             % (ssh_keys_dir, ssh_keys_dir, ssh_keys_dir)
            make_ssh_dir = ShellCommand("Creating .ssh dir if necessary",
                                        ssh_keys_dir_cmd, "~")

            cat_cmd = "cat ~/%s >> authorized_keys2" % public_key
            cat_key = ShellCommand("Adding ssh key to %s" % machine_name,
                                   cat_cmd, ssh_keys_dir)

            commands = [make_ssh_dir, cat_key]
        except Exception:
            output.update(self.dump_exception(username))
            output["status"] = FAIL
            return output
        return self.start_job(username, machine_interface, commands, copy_dict)

    def disable_job(self, username, machine_name):
        "Remove shared ssh key from a remote machine"
        output = {"status": OK}
        try:
            machine_interface = self.get_machine_interface(username,
                                                           machine_name)
            public_key = "id_dsa.pub"
            auth_keys = ".ssh/authorized_keys2"
            tmp_auth = "tmp_auth_keys2"
            copy_dict = { "admin" : [public_key] }

            filter_cmd = """grep -v "$( awk '{print $3}' < %s""" % public_key
            filter_cmd += """)" < %s > %s""" % ( auth_keys, tmp_auth )
            filter_sc = ShellCommand("Filtering ssh key from %s" % machine_name,
                                      filter_cmd, '~')
            overwrite_cmd = "cat %s > %s && rm %s"
            overwrite_cmd = overwrite_cmd  % ( tmp_auth, auth_keys, tmp_auth )
            overwrite_sc = ShellCommand("Overwriting authorized_keys",
                                     overwrite_cmd, '~')
            
            commands = [filter_sc, overwrite_sc]
        except Exception:
            output.update(self.dump_exception(username))
            output["status"] = FAIL
            return output
        return self.start_job(username, machine_interface, commands, copy_dict)

    def test_job(self, username, machine_name):
        "Simple test job"
        cmd = 'for i in 1 2 3 4; do sleep 1; echo "Testing $i/4"; done'
        commands = [ShellCommand("self_test", cmd, '.')]
        machine_interface = self.get_machine_interface(username, machine_name)
        return self.start_job(username, machine_interface, commands)

    def cleanup_connections(self, username):
        "Close connections for machine interfaces in interface pool"
        output = {"status": OK}
        try:
            for machine_name in self.machine_interface_pool:
                machine_interface = self.machine_interface_pool[machine_name]
                msg = "Requested connection termination to %s" % machine_name
                self.server_log.warning(msg, username)
                status = machine_interface.terminate()
                output[machine_name] = status
            self.machine_interface_pool = {}
        except Exception:
            output.update(self.dump_exception(username))
        return output

    def job_join(self, username, job_name, timeout):
        "Wait for a job to finish"
        output = {"status": OK}
        try:
            job = self.jobs.get(job_name)
            start_time = time.time()
            end_time = start_time + timeout
            if not job:
                raise InvalidJobName(job_name)
            while job.isAlive():
                if time.time() > end_time:
                    raise JoinTimeout(job_name, timeout)
                time.sleep(1)
            output["command_status"] = job.command_status
            output["command_output"] = job.command_output
            output["complete_log"] = job.complete_log
        except Exception:
            output.update(self.dump_exception(username))
        del self.jobs[job_name]
        return output

    def job_poll(self, username, job_name):
        "Poll a job for status info and log data"
        output = {"status": OK}
        try:
            job = self.jobs.get(job_name)
            if not job:
                raise InvalidJobName(job_name)
            if job.isAlive():
                output["alive"] = True
                output["command_status"] = None
                output["command_output"] = []
                output["elapsed_time"] = time.time() - job.start_time
                output["new_output"] = job.machine_interface.get_new_logs()
            else:
                output["alive"] = False
                output["command_status"] = job.command_status
                output["command_output"] = job.command_output
                output["elapsed_time"] = job.elapsed_time
                output["new_output"] = job.final_logs
        except Exception:
            output.update(self.dump_exception(username))
        return output

    def check_status(self):
        "Return elapsed time"
        time_running = time.time() - self.start_time
        return { "uptime": time_running,
                 "active_jobs": self.jobs.keys() } 

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
            enc_lazy = encrypt(lazy_dog, password)
            enc_dict = { "enc_test" : enc_lazy }
            open( test_decrypt_file, 'w' ).write(yaml.dump( enc_dict ))
        try:
            cipher_dict = yaml.load(open(test_decrypt_file, 'r').read())
        except IOError:
            return FAIL, "Encryption not set up properly. %s not readable"
        try:
            clear_dict = decrypt(cipher_dict, password)
        except DecryptionException:
            return FAIL, ["Invalid configuration key."]
        if clear_dict.get("test") == lazy_dog:
            return OK, ['Configuration key set.']
        return FAIL, ["Invalid configuration key."]

if __name__ == '__main__':
    from django.core.management import setup_environ
    import settings
    setup_environ(settings)

#    #Pyro.core.initServer()
#    daemon = Pyro.core.Daemon()
#    ns = Pyro.naming.NameServerLocator().getNS()
#    daemon.useNameServer(ns)
#
#    #daemon=Pyro.core.Daemon()
#    #print "The daemon runs on port:",daemon.port
#    #print "The object's uri is:",uri
#
#    uri = daemon.connect(Dispatcher(),"dispatcher")
#    print "Started server."
#    daemon.requestLoop()
#
