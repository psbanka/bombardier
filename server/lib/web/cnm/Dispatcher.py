import Pyro.core
import time, os
import yaml
import Pyro.naming
from MachineConfig import MachineConfig
from BombardierMachineInterface import BombardierMachineInterface
from bombardier_core.static_data import OK, FAIL, COMMAND_LOG_MARKER
from bombardier_core.libCipher import decrypt, DecryptionException
from threading import Thread
import StringIO, traceback
import ServerLogger

from Exceptions import InvalidJobName, JoinTimeout, PackageNotFound
from Exceptions import InvalidAction
from bombardier_core.static_data import INIT, INSTALL, ACTION_LOOKUP
from bombardier_core.static_data import ACTION_DICT

PENDING = 4

def exception_dumper(func):
    argnames = func.func_code.co_varnames[:func.func_code.co_argcount]
    fname = func.func_name
    def traceback_func(*args,**kwargs):
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
    def __init__(self, name):
        self.name = name
        self.dump_config = False
        self.status = PENDING

    def execute(self, machine_interface):
        pass

    def info(self):
        return "NONE"

class ShellCommand(AbstractCommand):
    def __init__(self, name, cmd, working_dir):
        AbstractCommand.__init__(self, name)
        self.working_dir = working_dir
        self.cmd = cmd

    def execute(self, machine_interface):
        machine_interface.chdir(self.working_dir)
        return machine_interface.run_cmd(self.cmd)

    def info(self):
        return self.cmd

class BombardierCommand(AbstractCommand):
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
        return machine_interface.take_action(self.action, self.package_name,
                                             self.script_name, self.package_revision,
                                             self.debug)
                                         
    def info(self):
        return self.name

class Job(Thread):
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
        Thread.__init__(self)
        self.machine_interface.freshen()
        self.complete_log = ''

    def run(self):
        self.server_log.info("Starting...", self.name)
        self.start_time = time.time()
            
        for command in self.commands:
            try:
                self.server_log.info("Processing command: %s" % command.name, self.name)
                status, output = command.execute(self.machine_interface)
                self.command_status = status
                self.command_output = output
            except Exception:
                self.elapsed_time = time.time() - self.start_time
                msg = "Command %s: Failed to run %s" % (command.name, command.info())
                self.server_log.error( msg, self.name)
                exc = StringIO.StringIO()
                traceback.print_exc(file=exc)
                exc.seek(0)
                data = exc.read()
                ermsg = ''
                for line in data.split('\n'):
                    ermsg = "%% %s" % line
                    self.server_log.error(ermsg, self.name)
        self.server_log.info("Finishing", self.name)
        self.elapsed_time = time.time() - self.start_time
        self.final_logs = self.machine_interface.polling_log.get_final_logs()
        self.complete_log = self.machine_interface.polling_log.get_complete_log()
        self.machine_interface.unset_job()

class Dispatcher(Pyro.core.ObjBase):
    def __init__(self):
        Pyro.core.ObjBase.__init__(self)
        self.start_time = time.time()
        self.calls = 0
        self.password = None
        self.server_home = None
        self.jobs = {}
        self.next_job = 1
        self.server_log = ServerLogger.ServerLogger("Dispatcher")
        self.server_log.add_std_err()
        self.machine_interface_pool = {}

    def dump_exception(self, username):
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
        self.server_log.info("Setting server home: %s" % server_home, username)
        self.server_home = server_home

    def get_machine_interface(self, username, machine_name):
        machine_config = MachineConfig(machine_name, self.password,
                                       self.server_home)
        machine_config.merge()
        machine_interface = None
        if machine_name in self.machine_interface_pool:
            self.server_log.info("Reusing existing connection to %s" % machine_name, username)
            machine_interface = self.machine_interface_pool[machine_name]
        else:
            self.server_log.info("Instantiating a MachineInterface for  %s" % machine_name, username)
            machine_interface = BombardierMachineInterface(machine_config, self.server_log)
            self.machine_interface_pool[machine_name] = machine_interface
        return machine_interface 
    
    def start_job(self, username, machine_interface, commands, copy_dict = {}):
        output = {"status": OK}
        try:
            machine_name = machine_interface.host_name
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
        output = {"status": OK}
        copy_dict = {}
        try:
            machine_interface = self.get_machine_interface(username, machine_name)
            commands = []
            if machine_interface.platform== "win32":
                script = ["export SPKG_DOS_DIR=$(cygpath -w %s)" % machine_interface.spkg_dir,
                          'regtool add -v "/HKEY_LOCAL_MACHINE/SOFTWARE/Bombardier"',
                          'regtool set -v "/HKEY_LOCAL_MACHINE/SOFTWARE/Bombardier/InstallPath" $SPKG_DOS_DIR'
                         ]
                cmd = ';'.join(script)
            else:
                cmd = 'echo spkgPath: %s > /etc/bombardier.yml' % machine_interface.spkg_dir
            set_spkg_config = ShellCommand("Setting spkg path", cmd, '.')
            bombardier_init = BombardierCommand("init")

            commands = [set_spkg_config, bombardier_init]
        except Exception:
            output.update(self.dump_exception(username))
            output["status"] = FAIL
            return output
        return self.start_job(username, machine_interface, commands, copy_dict)

    def bom_job(self, username, machine_name, action_string):
        output = {"status": OK}
        copy_dict = {}
        try:
            machine_interface = self.get_machine_interface(username, machine_name)
            bombardier_recon = BombardierCommand(action_string)
            commands = [bombardier_recon]
        except Exception:
            output.update(self.dump_exception(username))
            output["status"] = FAIL
            return output
        return self.start_job(username, machine_interface, commands, copy_dict)

    def status_job(self, username, machine_name):
        return self.bom_job(username, machine_name, "status")

    def reconcile_job(self, username, machine_name):
        return self.bom_job(username, machine_name, "reconcile")

    def package_action_job(self, username, package_name, action_string, 
                           machine_name, package_revision=None):
        output = {"status": OK}
        try:
            machine_interface = self.get_machine_interface(username, machine_name)
            bom_cmd = BombardierCommand(action_string, package_name=package_name, 
                                        package_revision=package_revision)
            commands = [bom_cmd]

        except Exception:
            output.update(self.dump_exception(username))
            output["status"] = FAIL
            return output
        return self.start_job(username, machine_interface, commands, {})

    def dist_job(self, username, machine_name, dist_name):
        output = {"status": OK}
        try:
            machine_interface = self.get_machine_interface(username, machine_name)
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

    def test_job(self, username, machine_name):
        cmd = 'for i in 1 2 3 4; do sleep 1; echo "Testing $i/4"; done'
        commands = [ShellCommand("self_test", cmd, '.')]
        machine_interface = self.get_machine_interface(username, machine_name)
        return self.start_job(username, machine_interface, commands)

    def cleanup(self, username):
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
                    output["status"] = FAIL
                    return output
                time.sleep(1)
            output["command_status"] = job.command_status
            output["command_output"] = job.command_output
            output["complete_log"] = job.complete_log
        except Exception:
            output.update(self.dump_exception(username))
        return output

    def job_poll(self, username, job_name):
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

    def check_in(self):
        time_running = time.time() - self.start_time
        self.calls += 1
        return "time running: %5.2f; calls: %d" % (time_running, self.calls)

    def set_password(self, password):
        status = OK
        status, validate_output = self._validate_password(password)
        if status == OK:
            self.password = password
        output = {"status": status,
                  "output": validate_output}
        return output

    def check_password(self):
        return self.password

    def _validate_password(self, password):
        "Very shallow password validation"
        test_decrypt_file = os.path.join(self.server_home, 'admin', 'encryption_validation.yml')
        if not os.path.isfile(test_decrypt_file):
            return FAIL, "Password is not set: %s doesn't exist" % test_decrypt_file
        try:
            cipher_dict = yaml.load(open(test_decrypt_file, 'r').read())
        except IOError:
            return FAIL, "Encryption not set up properly. %s not readable"
        try:
            clear_dict = decrypt(cipher_dict, password)
        except DecryptionException:
            return FAIL, "Incorrect password."
        if clear_dict.get("test") == "the_quick_brown_fox_jumped_over_the_lazy_dog\n":
            return OK, "Password valid."
        return FAIL, "Incorrect password."

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

