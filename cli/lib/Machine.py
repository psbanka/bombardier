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
from bombardier_core import mini_utility
from SystemStateSingleton import SystemState, ENABLE
from PackageField import PackageField, FIX, PURGE, NOT_INSTALLED, INSTALLED
import PackageActionField
from Exceptions import MachineTraceback, CommandError
from JobNameField import JobNameField
from Ssh import Ssh
import Show
import Edit
system_state = SystemState()
import libUi, yaml

URL_LOOKUP = {'test': "json/machine/start_test/%s",
              'dist': "json/machine/dist/%s",
              'init': "json/machine/init/%s",
              'reconcile': "json/machine/reconcile/%s",
              'check-status': "json/machine/check_status/%s",
              'fix': "json/machine/status/%s",
              'purge': "json/machine/status/%s",
              'enable': "json/machine/enable/%s",
              'disable': "json/machine/disable/%s",
              'setup': "json/machine/setup/%s",
             }

def setup_test():
    "Used by integration test code"
    fresh_status_yaml = """ 
client_version: 1.00-634
core_version: 1.00-629
install-progress: {}
timestamp: 1251918530.6349609"""

    status_file = "/opt/spkg/localhost/status.yml"
    open(status_file, "w").write(fresh_status_yaml)

def check_machine_name(tokens, no_flag):
    "Verifies that the machine name entered is valid"
    machine_field = ConfigField.ConfigField(data_type=ConfigField.MACHINE)
    possible_machine_names = machine_field.preferred_names(tokens, 1)
    machine_name = possible_machine_names[0]
    if no_flag:
        raise CommandError("NO cannot be used here")
    if len(possible_machine_names) == 0:
        raise CommandError("Unknown machine name: %s" % tokens[2])
    if len(possible_machine_names) > 1:
        raise CommandError("Ambiguous machine name: %s" % tokens[2])
    return machine_name

def edit_config_file(conf_str, config_field, object_name):
    "edit the configuration on the server"
    fd,fn = tempfile.mkstemp(suffix=".yml", text=True)
    fh = os.fdopen(fd, 'w+b')
    fh.write(conf_str)
    fh.close()
    os.system("%s %s" % (system_state.editor, fn))
    post_data = yaml.load(open(fn).read())
    submit = libUi.ask_yes_no("Commit changes to server", libUi.YES)
    if submit:
        output = config_field.post_data(object_name, post_data)
        os.unlink(fn)
        return output["command_status"], output["command_output"]
    else:
        msg = "Discarded changes. Edits can be found here: %s" % fn
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
        #output = machine_config.post_data(machine_name, machine_config_data)
        return output["command_status"], output["command_output"]

    def cmd(self, tokens, no_flag):
        """
        tokens -- all of the keywords passed in the command string, parsed
        no_flag -- whether the 'no' keyword was used in the command string
        """
        machine_name = check_machine_name(tokens, no_flag)

        if len(tokens) != 4:
            msg = "Incomplete command; need to include package name."
            raise CommandError(msg)

        package_name = tokens[3]
        pkg_data = self.pkg_field.get_specific_data(tokens, 3)
        pkg_config_data = pkg_data.get("configuration", {})
        merged_field = ConfigField.ConfigField(data_type=ConfigField.MERGED)
        merged_data = merged_field.get_specific_data(tokens, 1)
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

    def cmd(self, tokens, no_flag):
        """
        tokens -- all of the keywords passed in the command string, parsed
        no_flag -- whether the 'no' keyword was used in the command string
        """
        machine_name = check_machine_name(tokens, no_flag)

        if len(tokens) == 3 or tokens[3] == "config":
            return Edit.Machine().cmd(["edit", "machine", machine_name], 0)

        command = tokens[3]
        if len(tokens) != 5:
            msg = "Incomplete command; need to include %s name." % command
            raise CommandError(msg)

        object_name = tokens[4]

        if command == "bom":
            return Edit.Bom().cmd(["Edit", "bom", object_name], 0)
        elif command == "include":
            return Edit.Include().cmd(["Edit", "include", object_name], 0)

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

    def cmd(self, tokens, no_flag):
        """
        tokens -- all of the keywords passed in the command string, parsed
        no_flag -- whether the 'no' keyword was used in the command string
        """
        machine_name = check_machine_name(tokens, no_flag)

        if len(tokens) == 3 or tokens[3] == "config":
            return Show.Machine().cmd(["Show", "machine", machine_name], 0)

        command = tokens[3]
        if command == "merged":
            return Show.Merged().cmd(["Show", "merged", machine_name], 0)
        elif command == "status":
            return Show.Status().cmd(["show", "status", machine_name], 0)
        elif command == "summary":
            return Show.Summary().cmd(["show", "summary", machine_name], 0)

        if len(tokens) != 5:
            msg = "Incomplete command; need to include %s name." % command
            raise CommandError(msg)

        object_name = tokens[4]

        if command == "bom":
            return Show.Bom().cmd(["Show", "bom", object_name], 0)
        elif command == "include":
            return Show.Include().cmd(["Show", "include", object_name], 0)

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

    def cmd(self, tokens, no_flag):
        """
        tokens -- all of the keywords passed in the command string, parsed
        no_flag -- whether the 'no' keyword was used in the command string
        """

        machine_name = check_machine_name(tokens, no_flag)
        sub_command = "show"

        if len(tokens) >= 4:
            sub_command = tokens[3]
        
        post_data = None
        if sub_command == "stop":
            url = "/json/machine/stop-jobs/"
            post_data = {"machine": machine_name}
        elif sub_command == "clear-broken":
            url = "/json/machine/clear-broken-jobs/"
            post_data = {"machine": machine_name}
        elif sub_command == "view":
            if len(tokens) < 5:
                msg = "Incomplete command; need to include the job name."
                raise CommandError(msg)
            job_name = tokens[4]
            return system_state.cnm_connector.watch_jobs([job_name])
        else:
            url = "/json/machine/show-jobs/%s" % machine_name
            output = system_state.cnm_connector.service_yaml_request(url)
            return output["command_status"], output
        output = system_state.cnm_connector.service_yaml_request(url, post_data=post_data)
        return output["command_status"], output["command_output"]


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
        purge = PinshCmd.PinshCmd("purge")
        purge.help_text = "removes a package from the machine's status data"
        self.machine_field.children = [test, dist, init, enable, ssh, purge,
                                       disable, check_status, edit, fix,
                                       reconcile, execute, install, uninstall,
                                       configure, verify, push, unpush, setup,
                                       job, show, assign]

        # Third-level commands
        dist_field = ConfigField.ConfigField(data_type=ConfigField.DIST)
        
        dist.children = [dist_field]
        fix.children = [PackageField(action_type=FIX)]
        purge.children = [PackageField(action_type=PURGE)]
        installed_package_field = PackageField(INSTALLED)
        not_installed_package_field = PackageField(NOT_INSTALLED)
        executable_package_field = PackageField(INSTALLED)
        install.children = [not_installed_package_field]
        uninstall.children = [installed_package_field]
        verify.children = [installed_package_field]
        configure.children = [installed_package_field]
        execute.children = [executable_package_field]

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

    def get_url_and_post(self, machine_name, tokens):
        "for package-type operations, looks up the URL and gets POST data"
        command = tokens[2].lower()
        if command not in URL_LOOKUP:
            raise CommandError("%s is not a valid command" % command)

        post_data = {}
        url = URL_LOOKUP[command] % machine_name
        if command == "dist":
            post_data = {"dist": tokens[-1]}
        elif command in [ "enable", "setup" ]:
            prompt = "%s administrative ssh password: " % machine_name
            password = libUi.pwd_input(prompt)
            yml_dict = yaml.dump( {"password" : password } )
            post_data = {"yaml" : yml_dict}
        return url, post_data

    def cmd(self, tokens, no_flag):
        """
        tokens -- all of the keywords passed in the command string, parsed
        no_flag -- whether the 'no' keyword was used in the command string
        """
        if len(tokens) < 2:
            raise CommandError("Incomplete command.")
        machine_name = check_machine_name(tokens, no_flag)
        if len(tokens) == 2:
            system_state.push_prompt(["machine", machine_name])
            return OK, []

        command = tokens[2].lower()

        if command == "ssh":
            return Ssh().cmd(["ssh", machine_name], 0)

        if command == "push":
            url = "/json/machine/push/%s" % machine_name
            post_data = {"machine": machine_name}
            job_name = system_state.cnm_connector.get_job(url, post_data)
            return system_state.cnm_connector.watch_jobs([job_name])

        if command == "unpush":
            url = "/json/machine/unpush/%s" % machine_name
            post_data = {"machine": machine_name}
            job_name = system_state.cnm_connector.get_job(url, post_data)
            return system_state.cnm_connector.watch_jobs([job_name])

        if command in ["install", "uninstall", "verify",
                       "configure", "execute", "purge", "fix"]:
            if command == "execute":
                if len(tokens) != 5:
                    msg = "Incomplete command; require a package name "\
                          "and a command."
                    raise CommandError(msg)
                command = tokens[4]
            if len(tokens) <= 3:
                msg = "Incomplete command; require a package name."
                raise CommandError(msg)
            package_name = tokens[3]
            machine_name = tokens[1]

            url = "/json/package_action/%s" % package_name
            post_data = {"machine": machine_name,
                         "action": command}
        else:
            if command == "dist":
                if len(tokens) <= 3:
                    msg = "Incomplete command; requires a dist file name."
                    raise CommandError(msg)
            url, post_data = self.get_url_and_post(machine_name, tokens)

        try:
            job_name = system_state.cnm_connector.get_job(url, post_data)
            libUi.info("Watching job progress. Press ^C to abort or disconnect.")
            status, output = system_state.cnm_connector.watch_jobs([job_name])
            return status, output
        except MachineTraceback, m_err:
            libUi.process_traceback(m_err)
            return FAIL, []

