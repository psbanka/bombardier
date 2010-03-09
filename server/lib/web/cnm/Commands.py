"""A job runs one or more commands"""

from bombardier_core.static_data import ACTION_LOOKUP
from bombardier_core.static_data import ACTION_REVERSE_LOOKUP
from Exceptions import InvalidAction

PENDING = 4

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


class BuildCommand(AbstractCommand):
    "Package build command"
    def __init__(self, package_name, svn_user, svn_password,
                 debug, prepare):
        name = "Build-%s" % (package_name)
        AbstractCommand.__init__(self, name)
        self.package_name = package_name
        self.svn_user = svn_user
        self.svn_password = svn_password
        self.debug = debug
        self.prepare = prepare

    def execute(self, machine_interface):
        "Build the package"
        return machine_interface.build_components(self.package_name,
                                                  self.svn_user,
                                                  self.svn_password,
                                                  self.debug, self.prepare)

    def info(self):
        "Return command to be run on the remote machine"
        return "Build package %s" % self.package_name

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
    def __init__(self, action_string, package_name, script_name):
        '''
        action_string -- one of the following: uninstall, configure, install,
                         verify, reconcile, check_status, execute, fix, purge,
                         dry_run, or init
        package_name -- the name of a package to act upon
        script_name -- the name of a script to act upon
        debug -- defunct, apparently
        '''
        action_const = ACTION_LOOKUP.get(action_string.lower().strip())
        if action_const == None:
            raise InvalidAction(package_name, action_string)
        name = "Bombardier-%s-%s" % (action_string, package_name)
        AbstractCommand.__init__(self, name)
        self.action = action_const
        self.package_name = package_name
        self.script_name = script_name
        self.debug = False

    def execute(self, machine_interface):
        "Run bc command"
        return machine_interface.take_action(self.action, self.package_name,
                                             self.script_name, self.debug)

    def info(self):
        "Return information about this bombardier command"
        output = ACTION_REVERSE_LOOKUP[self.action]
        if self.package_name:
            output += ": %s" % self.package_name
        if self.script_name:
            output += " (%s)" % self.script_name
        return output


