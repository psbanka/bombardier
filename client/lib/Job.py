#!/usr/bin/python

'Runs a package job in an isolated thread'

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

import StringIO, traceback, time
from bombardier_core.Logger import Logger
from bombardier_core.static_data import OK, FAIL
from threading import Thread

class JobThread(Thread):
    'Runs an action on a machine in a separate thread'

    def __init__(self, import_string, cmd, config):
        '''
        import_string -- python code which will be exec'd to import the
                         package class
        cmd -- python code which will be exec'd which will run the actual
               action on this machine.
        config -- the configuration of this machine
        '''
        Thread.__init__(self)
        self.import_string = import_string
        self.cmd = cmd
        self.config = config
        self.cmd_status = None

    def run(self):
        'Thread interface'
        Logger.debug("Running %s..." % self.cmd)
        try:
            exec(self.import_string)
            exec("self.cmd_status = %s" % self.cmd)
        except StandardError, err:
            Logger.error("Failed to run %s (%s)" % (self.cmd, err))
            sio = StringIO.StringIO()
            traceback.print_exc(file=sio)
            sio.seek(0)
            data = sio.read()
            ermsg = ''
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            Logger.error(ermsg)
            self.cmd_status = FAIL

class Job:
    'Controls the execution of a thread'

    def __init__(self, import_string, cmd, config):
        '''
        import_string -- python code which will be exec'd to import the
                         package class
        cmd -- python code which will be exec'd which will run the actual
               action on this machine.
        config -- the configuration of this machine
        '''
        self.import_string = import_string
        self.cmd = cmd
        self.config = config
        self.start_time = None
        self.job_thread = None
        self.job_status = None

    def is_running(self):
        'ability to check if the job is finished'
        if self.job_thread:
            if self.job_thread.isAlive():
                return True
            if type(self.job_thread.cmd_status) == type(0) \
               or type(self.job_thread.cmd_status) == type('str'):
                Logger.debug("-- status: %s" % (self.job_thread.cmd_status))
                self.job_status = self.job_thread.cmd_status
            else:
                msg = "Invalid return status (type: %s)"
                Logger.error(msg % (type(self.job_thread.cmd_status)))
                self.job_status = FAIL
        return False

    def execute(self):
        'starts a job and waits for it to finish'
        self.start_time = time.time()
        status = FAIL
        if self.is_running():
            msg = "Refusing to run %s; it is already running" % self.cmd
            Logger.error(msg)
            return FAIL
        self.job_thread = JobThread(self.import_string, self.cmd, self.config)
        self.job_thread.start()
        Logger.info("Started job...")
        counter = 1
        while self.is_running():
            time.sleep(1)
            counter += 1
            if not counter % 100:
                msg = "Waiting for completion (%s)..."
                Logger.info(msg % time.strftime(time.ctime()))
                counter = 1
        status = self.job_thread.cmd_status
        return status

    def kill_thread(self, timeout=5):
        'Aborts a job'
        if not self.is_running():
            return OK
        self.job_thread.join(timeout)
        return OK


