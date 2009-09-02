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

import PinshCmd
from ConfigField import ConfigField, MACHINE, DIST, PACKAGE
from bombardier_core.static_data import OK, FAIL
from SystemStateSingleton import SystemState, ENABLE
from Exceptions import MachineTraceback
import Integer
system_state = SystemState()
import libUi, time

URL_LOOKUP = {'test': "json/machine/start_test/%s",
              'dist': "json/machine/dist/%s",
              'init': "json/machine/init/%s",
              'reconcile': "json/machine/reconcile/%s",
              'status': "json/machine/status/%s",
             }

class Machine(PinshCmd.PinshCmd):
#    '''bomsh# machine localhost test
#       [OK, ['Machine localhost is ready to take commands.']]
#       bomsh# machine localhost dist test
#       [OK, ['localhost updated with test']]
#       bomsh# machine localhost init
#       [OK, []]
#       bomsh# machine localhost reconcile
#       [OK, ['Reconcile complete on localhost']]
#       bomsh# machine localhost status
#       [OK, []]
    '''
        bomsh# machine localhost status fix TestPackageType4
        [OK, ['Package TestPackageType4 fixed on localhost']]
    '''
#       bomsh# machine localhost status purge TestPackageType4 7
#       [OK, []]
#    '''
    def __init__(self):
        """Top-level object has a 'test' child: test the machine
        """
        PinshCmd.PinshCmd.__init__(self, "machine")
        self.help_text = "machine\tcommands that operate on a given machine"
        self.cmd_owner = 1

        self.machine_field = ConfigField(data_type=MACHINE)
        self.children = [self.machine_field]

        test = PinshCmd.PinshCmd("test", "test\ttest a machine connection")
        dist = PinshCmd.PinshCmd("dist", "dist\tdeploy a library to a machine")
        init = PinshCmd.PinshCmd("init", "init\tintialize bombardier on a machine")
        reconcile = PinshCmd.PinshCmd("reconcile", "reconcile\treconcile machine state to bill of materials")
        status = PinshCmd.PinshCmd("status", "status\tstatus reporting or manipulation")

        self.machine_field.children = [test, dist, init,
                                       status, reconcile]

        self.dist_field = ConfigField(data_type=DIST)
        dist.children = [self.dist_field]

        fix = PinshCmd.PinshCmd("fix", "fix\tsets the state of a package to INSTALLED")
        purge = PinshCmd.PinshCmd("purge", "purge\tremoves a package from the status of a machine")

        status.children = [fix, purge]

        fix.children = [ConfigField(data_type=PACKAGE)]
        purge_package = ConfigField(data_type=PACKAGE)
        purge.children = [purge_package]

        purge_package.children = [Integer.Integer()]
        
        self.auth = ENABLE

    def watch_job(self, job_name):
        output = {"alive": 1}
        while output.get("alive", 0):
            time.sleep(0.25)
            url = "json/job/poll/%s" % job_name
            output = system_state.cnm_connector.service_yaml_request(url)
            if "traceback" in output:
                raise MachineTraceback(url, output["traceback"])
            new_output = output["new_output"]
            if new_output:
                libUi.process_cnm(new_output)
        return 

    def analyze_output(self, machine_name, command, output, post_data):
        if command == "test":
            for i in range(1,5):
                if "Testing %d/4" % i not in output["complete_log"]:
                    return FAIL, "Invalid output from server"
                return OK, [ "Machine %s is ready to take commands." % machine_name ]
        elif command == "dist":
            if output["command_status"] == OK:
                return OK, ["%s updated with %s" % (machine_name, post_data["dist"])]
            else:
                return FAIL, ["%s FAILED to install %s" % (machine_name, post_data["dist"])]
        elif command == "reconcile":
            if output["command_status"] == OK:
                return OK, ["Reconcile complete on %s" % machine_name]
            else:
                return FAIL, ["Reconcile FAILED on %s" % machine_name]

        return output["command_status"], output["command_output"]

    def cmd(self, tokens, no_flag):
        """
        tokens -- all of the keywords passed in the command string, parsed
        no_flag -- whether the 'no' keyword was used in the command string
        """

        if no_flag:
            return FAIL, []
        if len(tokens) < 3:
            return FAIL, ["Incomplete command."]
        possible_machine_names = self.machine_field.preferred_names(tokens, 1)
        if len(possible_machine_names) == 0:
            return FAIL, ["Unknown machine name: %s" % tokens[2]]
        if len(possible_machine_names) > 1:
            return FAIL, ["Ambiguous machine name: %s" % tokens[2]]

        machine_name = possible_machine_names[0]

        command = tokens[2].lower()
        if command not in URL_LOOKUP:
            return FAIL, ["Unknown command: %s" % command]

        post_data = {}
        url = URL_LOOKUP[command] % machine_name
        if command == "dist":
            post_data = {"dist": tokens[-1]}

        # FIXME: Left off here with status fix command

        try:
            job_name = system_state.cnm_connector.get_job(url, post_data)
            libUi.info("Job name: %s" % job_name)
            self.watch_job(job_name)

            libUi.info("Joining...")
            output = system_state.cnm_connector.join_job(job_name)
            print "OUTPUT::::", output
            return self.analyze_output(machine_name, command, output, post_data)
            #return output["command_status"], output["command_output"]
        except MachineTraceback, m_err:
            libUi.process_traceback(m_err)
