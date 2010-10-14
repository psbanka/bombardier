#!/usr/bin/python
# BSD License
# Copyright (c) 2009, Peter Banka et al
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the name of the GE Security nor the names of its contributors may
#   be used to endorse or promote products derived from this software without
#   specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

"""This provides a top-level command for the bomsh shell. This is for
managing machines in the network, and subclasses the [PinshCmd] module
"""

__author__ =  'Peter Banka'
__version__ = '1.0'

import tempfile, os
import PinshCmd
import ConfigField
from bombardier_core.static_data import OK, FAIL
from bombardier_core.static_data import BDR_CLIENT_TYPE
from bombardier_core import mini_utility
from SystemStateSingleton import SystemState, ENABLE
from PackageField import PackageField, FIX, PURGE, NOT_INSTALLED, INSTALLED
from PackageField import BACKUP_PROVIDERS
import PackageActionField
import RestoreTargetField
from Exceptions import MachineTraceback, CommandError
from JobNameField import JobNameField
from Ssh import Ssh
import CommandLine
import Show
import Edit
system_state = SystemState()
import libUi, yaml

def setup_test():
    "Used by integration test code"
    fresh_status_yaml = """ 
client_version: 1.00-634
core_version: 1.00-629
install-progress: {}
timestamp: 1251918530.6349609"""

    status_file = "/opt/spkg/localhost/status.yml"
    open(status_file, "w").write(fresh_status_yaml)

def check_machine_name(command_line):
    "Verifies that the machine name entered is valid"
    machine_field = ConfigField.ConfigField(data_type=ConfigField.MACHINE)
    possible_machine_names = machine_field.preferred_names(command_line, 1)
    machine_name = possible_machine_names[0]
    if command_line.no_flag:
        raise CommandError("NO cannot be used here")
    if len(possible_machine_names) == 0:
        raise CommandError("Unknown machine name: %s" % command_line[2])
    if len(possible_machine_names) > 1:
        raise CommandError("Ambiguous machine name: %s" % command_line[2])
    return machine_name

def edit_config_file(conf_str, config_field, object_name):
    "edit the configuration on the server"
    descriptor, file_name = tempfile.mkstemp(suffix=".yml", text=True)
    handle = os.fdopen(descriptor, 'w+b')
    handle.write(conf_str)
    handle.close()
    os.system("%s %s" % (system_state.editor, file_name))
    post_data = yaml.load(open(file_name).read())
    submit = libUi.ask_yes_no("Commit changes to server", libUi.YES)
    if submit:
        output = config_field.post_data(object_name, post_data)
        os.unlink(file_name)
        return output["command_status"], output["command_output"]
    else:
        msg = "Discarded changes. Edits can be found here: %s" % file_name
        return OK, [msg]

class AssignCommand(PinshCmd.PinshCmd):
    "Handles all 'machine <machine> assign x' operations"
    def __init__(self):
        "Set up the command"
        PinshCmd.PinshCmd.__init__(self, "assign")
        self.help_text = "add a package to a machine"
        self.cmd_owner = 1
        self.auth = ENABLE
        self.pkg_field = ConfigField.ConfigField(data_type=ConfigField.PACKAGE)
        self.children = [self.pkg_field]

    def assign_package(self, machine_name, package_name, missing_data):
        "Modify the machine's configuration file to include a pacakge"
        prefix = ''
        if missing_data:
            prefix = '---\n%s\n\n---\n' % yaml.dump(missing_data, default_flow_style=False)
        machine_config = ConfigField.ConfigField(data_type=ConfigField.MACHINE)
        machine_config_data = machine_config.get_data(machine_name)
        package_list = machine_config_data.get("packages", [])
        package_list.append(package_name)
        machine_config_data["packages"] = package_list
        conf_str = prefix + yaml.dump(machine_config_data, default_flow_style=False)
        return edit_config_file(conf_str, machine_config, machine_name)

    def cmd(self, command_line):
        """
        command_line -- all of the keywords passed in the command string, parsed
        """
        machine_name = check_machine_name(command_line)

        if len(command_line) != 4:
            msg = "Incomplete command; need to include package name."
            raise CommandError(msg)

        package_name = command_line[3]
        pkg_data = self.pkg_field.get_specific_data(command_line, 3)
        pkg_config_data = pkg_data.get("configuration", {})
        merged_field = ConfigField.ConfigField(data_type=ConfigField.MERGED)
        merged_data = merged_field.get_specific_data(command_line, 1)
        missing_stuff = mini_utility.diff_dicts(pkg_config_data, merged_data)
        if package_name in merged_data.get("packages", []) and not missing_stuff:
            msg = "Package %s is already assigned and properly configured."
            return OK, [msg % package_name]
        return self.assign_package(machine_name, package_name, missing_stuff)

class EditCommand(PinshCmd.PinshCmd):
    "Handles all 'machine <machine> edit x' operations"
    def __init__(self):
        "Set up the command"
        PinshCmd.PinshCmd.__init__(self, "edit")
        self.help_text = "commands that edit configuration data"
        self.cmd_owner = 1
        self.auth = ENABLE

        bom = PinshCmd.PinshCmd("bom")
        bom.help_text = "Edit a Bill of Materials for this machine"
        include = PinshCmd.PinshCmd("include")
        include.help_text = "Edit an include file for this machine"
        config = PinshCmd.PinshCmd("config")
        config.help_text = "Edit the base configuration for this machine"
        self.children = [bom, include, config]
        bom_config = ConfigField.ConfigField(data_type=ConfigField.BOM,
                                             machine_field=1)
        include_config = ConfigField.ConfigField(data_type=ConfigField.INCLUDE,
                                                 machine_field=1)
        bom.children = [bom_config]
        include.children = [include_config]

    def cmd(self, command_line):
        """
        command_line -- all of the keywords passed in the command string, parsed
        """
        machine_name = check_machine_name(command_line)

        if len(command_line) == 3 or command_line[3] == "config":
            command_string = "edit machine %s" % machine_name
            command_line = CommandLine.process_input(command_string)
            return Edit.Machine().cmd(command_line)

        command = command_line[3]
        if len(command_line) != 5:
            msg = "Incomplete command; need to include %s name." % command
            raise CommandError(msg)

        object_name = command_line[4]

        if command == "bom":
            command_string = "edit bom %s" % object_name
            command_line = CommandLine.process_input(command_string)
            return Edit.Bom().cmd(command_line)
        elif command == "include":
            command_string = "edit include %s" % object_name
            command_line = CommandLine.process_input(command_string)
            return Edit.Include().cmd(command_line)

        raise CommandError("Unknown command: %s" % command)

class ShowCommand(PinshCmd.PinshCmd):
    "Handles all 'machine <machine> show x' operations"
    def __init__(self):
        "Set up the command"
        PinshCmd.PinshCmd.__init__(self, "show")
        self.help_text = "commands that show data about this machine"
        self.cmd_owner = 1
        self.auth = ENABLE

        status = PinshCmd.PinshCmd("status", "status reporting or manipulation")
        summary = PinshCmd.PinshCmd("summary")
        summary.help_text = "show the digested status information"
        bom = PinshCmd.PinshCmd("bom")
        bom.help_text = "Show the Bill of Materials for this machine"
        include = PinshCmd.PinshCmd("include")
        include.help_text = "Show an include file for this machine"
        merged = PinshCmd.PinshCmd("merged")
        merged.help_text = "Show the complete merged configuration for machine"
        config = PinshCmd.PinshCmd("config")
        config.help_text = "Show the basic configuration for machine"
        self.children = [config, bom, include, merged, summary, status]
        bom_config = ConfigField.ConfigField(data_type=ConfigField.BOM,
                                             machine_field=1)
        include_config = ConfigField.ConfigField(data_type=ConfigField.INCLUDE,
                                                 machine_field=1)
        bom.children = [bom_config]
        include.children = [include_config]

    def cmd(self, command_line):
        """
        command_line -- all of the keywords passed in the command string, parsed
        """
        machine_name = check_machine_name(command_line)

        if len(command_line) == 3 or command_line[3] == "config":
            command_string = "show machine %s" % machine_name
            command_line = CommandLine.process_input(command_string)
            return Show.Machine().cmd(command_line)

        command = command_line[3]
        if command == "merged":
            command_string = "show merged %s" % machine_name
            command_line = CommandLine.process_input(command_string)
            return Show.Merged().cmd(command_line)
        elif command == "status":
            command_string = "show status %s" % machine_name
            command_line = CommandLine.process_input(command_string)
            return Show.Status().cmd(command_line)
        elif command == "summary":
            command_string = "show summary %s" % machine_name
            command_line = CommandLine.process_input(command_string)
            return Show.Summary().cmd(command_line)

        if len(command_line) != 5:
            msg = "Incomplete command; need to include %s name." % command
            raise CommandError(msg)

        object_name = command_line[4]

        if command == "bom":
            command_string = "show bom %s" % object_name
            command_line = CommandLine.process_input(command_string)
            return Show.Bom().cmd(command_line)
        elif command == "include":
            command_string = "show include %s" % object_name
            command_line = CommandLine.process_input(command_string)
            return Show.Include().cmd(command_line)

        raise CommandError("Unknown command: %s" % command)

class JobCommand(PinshCmd.PinshCmd):
    "Handles all 'machine <machine> job x' operations"
    def __init__(self):
        "Set up the command"
        PinshCmd.PinshCmd.__init__(self, "job")
        self.help_text = "operations on the job queues"
        self.cmd_owner = 1
        self.auth = ENABLE

        clear_broken = PinshCmd.PinshCmd("clear-broken")
        clear_broken.help_text = "clear the broken jobs"
        stop = PinshCmd.PinshCmd("stop")
        stop.help_text = "immediately kill all active and pending jobs"
        show = PinshCmd.PinshCmd("show")
        show.help_text = "show active, pending, broken, and finished jobs"
        view = PinshCmd.PinshCmd("view")
        view.help_text = "view a running, finished, or broken job"
        self.children = [clear_broken, stop, show, view]

        job_name_field = JobNameField()
        view.children = [job_name_field]

    def cmd(self, command_line):
        """
        command_line -- all of the keywords passed in the command string, parsed
        """

        machine_name = check_machine_name(command_line)
        sub_command = "show"

        if len(command_line) >= 4:
            sub_command = command_line[3]
        
        if sub_command == "stop":
            return system_state.cnm_connector.stop_jobs(machine_name)
        elif sub_command == "clear-broken":
            return system_state.cnm_connector.clear_broken_jobs(machine_name)
        elif sub_command == "view":
            if len(command_line) < 5:
                msg = "Incomplete command; need to include the job name."
                raise CommandError(msg)
            job_name = command_line[4]
            return system_state.cnm_connector.watch_jobs([job_name])
        else:
            output = system_state.cnm_connector.show_jobs(machine_name)
            return output["command_status"], output

class Machine(PinshCmd.PinshCmd):
    '''
       bomsh# machine localhost reconcile
       [OK, ['install OK: TestPackageType4-7', 'verify OK: TestPackageType4-7', 'Finished installing']]
       bomsh# machine localhost check-status
       [OK, []]
       bomsh# machine localhost status purge TestPackageType4-7
       [OK, ['TestPackageType4-7 has been removed from localhost status']]
       bomsh# machine localhost status fix TestPackageType4
       [OK, ['TestPackageType4-7 has been set to INSTALLED.']]
       bomsh# machine localhost status purge TestPackageType4-7
       [OK, ['TestPackageType4-7 has been removed from localhost status']]
    '''

    def __init__(self):
        """Top-level object has a 'test' child: test the machine
        """
        PinshCmd.PinshCmd.__init__(self, "machine")
        self.help_text = "commands that operate on a given machine"
        self.cmd_owner = 1
        self.auth = ENABLE

        # Top-level commands
        self.machine_field = ConfigField.ConfigField(data_type=ConfigField.MACHINE)
        self.children = [self.machine_field]

        # Second-level commands
        test = PinshCmd.PinshCmd("test", "test a machine connection")
        dist = PinshCmd.PinshCmd("dist", "deploy a library to a machine")
        enable = PinshCmd.PinshCmd("enable", "share ssh keys with a machine")
        disable = PinshCmd.PinshCmd("disable", "remove ssh key from a machine")
        init = PinshCmd.PinshCmd("init", "intialize bombardier on a machine")
        reconcile = PinshCmd.PinshCmd("reconcile")
        reconcile.help_text = "reconcile machine state to bill of materials"
        check_status = PinshCmd.PinshCmd("check-status")
        check_status.help_text = "review the current state of the system"
        show = ShowCommand()
        assign = AssignCommand()
        install = PinshCmd.PinshCmd("install", "install a package")
        uninstall = PinshCmd.PinshCmd("uninstall", "remove a package")
        configure = PinshCmd.PinshCmd("configure", "configure a package")
        verify = PinshCmd.PinshCmd("verify", "verify a package")
        execute = PinshCmd.PinshCmd("execute", "run an action on a package")
        ssh = PinshCmd.PinshCmd("ssh")
        ssh.help_text = "connect directly to this machine from the cli"
        edit = EditCommand()
        push = PinshCmd.PinshCmd("push")
        push.help_text = "send cleartext configuration data to machine"
        unpush = PinshCmd.PinshCmd("unpush")
        unpush.help_text = "remove cleartext configuration data from machine"
        setup = PinshCmd.PinshCmd("setup")
        setup.help_text = "install bombardier software onto machine"
        job = JobCommand()
        fix = PinshCmd.PinshCmd("fix")
        fix.help_text = "sets the state of a package to INSTALLED"
        backup = PinshCmd.PinshCmd("backup")
        backup.help_text = "backs up the data associated with a package and copies it to server"
        restore = PinshCmd.PinshCmd("restore")
        restore.help_text = "restores data associated with a package"
        purge = PinshCmd.PinshCmd("purge")
        purge.help_text = "removes a package from the machine's status data"
        self.machine_field.children = [test, dist, init, enable, ssh, purge,
                                       disable, check_status, edit, fix,
                                       reconcile, execute, install, uninstall,
                                       configure, verify, push, unpush, setup,
                                       job, show, assign, backup, restore]

        # Third-level commands
        dist_field = ConfigField.ConfigField(data_type=ConfigField.DIST)
        
        dist.children = [dist_field]
        fix.children = [PackageField(action_type=FIX)]
        purge.children = [PackageField(action_type=PURGE)]
        installed_package_field = PackageField(INSTALLED)
        not_installed_package_field = PackageField(NOT_INSTALLED)
        executable_package_field = PackageField(INSTALLED)
        backup_provider_package_field = PackageField(BACKUP_PROVIDERS)
        restore_provider_package_field = PackageField(BACKUP_PROVIDERS)
        install.children = [not_installed_package_field]
        uninstall.children = [installed_package_field]
        verify.children = [installed_package_field]
        configure.children = [installed_package_field]
        execute.children = [executable_package_field]
        backup.children = [backup_provider_package_field]
        restore.children = [restore_provider_package_field]

        clear_broken = PinshCmd.PinshCmd("clear-broken")
        clear_broken.help_text = "clear the broken jobs"
        stop = PinshCmd.PinshCmd("stop")
        stop.help_text = "immediately kill all active and pending jobs"
        show = PinshCmd.PinshCmd("show")
        show.help_text = "show active, pending, broken, and finished jobs"
        view = PinshCmd.PinshCmd("view")
        view.help_text = "view a running, finished, or broken job"
        job.children = [clear_broken, stop, show, view]


        # Fourth-level commands
        package_actions = PackageActionField.PackageActionField()
        executable_package_field.children = [package_actions]
        job_name_field = JobNameField()
        view.children = [job_name_field]
        restore_targets = RestoreTargetField.RestoreTargetField()
        restore_provider_package_field.children = [restore_targets]

    def cmd(self, command_line):
        """
        command_line -- all of the keywords passed in the command string, parsed
        """
        if len(command_line) < 2:
            raise CommandError("Incomplete command.")
        machine_name = check_machine_name(command_line)
        if len(command_line) == 2:
            system_state.push_prompt(["machine", machine_name])
            return OK, []

        command = command_line[2].lower()
        bg_flag = command_line.bg_flag
        if command == "ssh":
            command_string = "ssh %s" % machine_name
            command_line = CommandLine.process_input(command_string)
            return Ssh().cmd(command_line)

        cnm = system_state.cnm_connector
        try:
            if command == "push":
                return cnm.push_machine_config(machine_name, bg_flag)
            elif command == "unpush":
                return cnm.unpush_machine_config(machine_name, bg_flag)
            elif command in ["install", "uninstall", "verify",
                             "configure", "execute", "purge", "fix",
                             "backup", "restore"]:
                argument = ''
                if command == "restore":
                    if len(command_line) != 5:
                        msg = "Incomplete command; require a restore target"
                        raise CommandError(msg)
                    argument = command_line[4]
                if command == "execute":
                    if len(command_line) != 5:
                        msg = "Incomplete command; require a package name "\
                              "and a command."
                        raise CommandError(msg)
                    command = command_line[4]
                if len(command_line) <= 3:
                    msg = "Incomplete command; require a package name."
                    raise CommandError(msg)
                package_name = command_line[3]
                return cnm.package_command(command, machine_name, package_name,
                                           argument, bg_flag)
            elif command in [ "enable", "setup" ]:
                prompt = "%s administrative ssh password: " % machine_name
                password = libUi.pwd_input(prompt)
                if command == "enable":
                    return cnm.enable_command(machine_name, password, bg_flag)
                else:
                    return cnm.setup_command(machine_name, password, bg_flag)
            elif command == [ "disable" ]:
                post_data = {"yaml": yaml.dump( {"machine_type": BDR_CLIENT_TYPE })}
                return cnm.machine_job(machine_name, "disable", bg_flag, post_data)
            elif command == "dist":
                if len(command_line) <= 3:
                    msg = "Incomplete command; requires a dist file name."
                    raise CommandError(msg)
                dist_name = command_line[-1]
                return cnm.dist_command(machine_name, dist_name, bg_flag)
            elif command in ['test', 'init', 'reconcile', 'check-status']:
                return cnm.machine_job(machine_name, command, bg_flag)
            else:
                raise CommandError("%s is not a valid command" % command)
        except MachineTraceback, m_err:
            libUi.process_traceback(m_err)
            return FAIL, []

