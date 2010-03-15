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

'''A [PinshCmd] command which allows a user to comment on actions that they
have taken within the bdr shell'''

import tempfile, os, libUi, re
import PinshCmd
import Expression
from bombardier_core.static_data import FAIL
from SystemStateSingleton import SystemState
system_state = SystemState()

#IGNORE_LINE = '--This line, and those below, will be ignored--'
PUBLISH_LINE    = "--PUBLISH------------------------------------------------"
PUBLISH_TEXT    = "PUBLISH THIS CHANGE [y/n]: y"
JOB_NOTICE_LINE = "--CHECK THE JOB NAMES THAT THIS COMMENT REFERS TO--------"

FORM_TEXT_HEADER = '\n\n\n%s\n%s\n\n%s'
FORM_TEXT_HEADER = FORM_TEXT_HEADER % (PUBLISH_LINE, PUBLISH_TEXT, JOB_NOTICE_LINE)

def process_form_data(form_data):
    "After the user has entered the form data, examine it."
    publish = True
    job_names = []
    comments = []
    state = None
    for line in form_data.split('\n'):
        line = line.strip()
        if not line:
            continue
        elif line == PUBLISH_LINE:
            state = PUBLISH_TEXT
            continue
        elif line == JOB_NOTICE_LINE:
            state = JOB_NOTICE_LINE
            continue
        if state == None:
            comments.append(line)
        elif state == PUBLISH_TEXT:
            publish_flag = line.split(':')[-1].strip().lower()
            if publish_flag.startswith('n'):
                publish = False
        elif state == JOB_NOTICE_LINE:
            match = re.compile('\[(.)\]\s+(\S+@\S+\-\d+)\:').findall(line)[0]
            if len(match) != 2:
                continue
            flag, job_name = match
            if flag == 'X':
                job_names.append(job_name)
    return publish, job_names, comments

def get_comments(form_text):
    "Create a form that the user can edit and pull out relevant data"
    _fd, file_name = tempfile.mkstemp(suffix=".txt", text=True)
    file_handle = os.fdopen(_fd, 'w+b')
    file_handle.write(form_text)
    file_handle.close()
    os.system("%s %s" % (system_state.editor, file_name))
    form_data = open(file_name).read()
    publish, job_names, comments = process_form_data(form_data)
    delete_temp_file = False
    submit = False
    if job_names and comments:
        submit = libUi.ask_yes_no("Submit comments to server", libUi.YES)
        if submit:
            delete_temp_file = True
        else:
            msg = "%%%% Discarded changes. Edits can be found here: %s\n"
            print msg % file_name
    else:
        delete_temp_file = True

    if delete_temp_file:
        os.unlink(file_name)
    return submit, publish, job_names, '\n'.join(comments)

class Comment(PinshCmd.PinshCmd):
    '''Comment on some jobs that have been run'''
    def __init__(self):
        PinshCmd.PinshCmd.__init__(self, "comment")
        self.help_text = "comment\tcomment on jobs that have been run"
        comment_line = Expression.Expression("<comment>\tComments to be applied to all uncommented jobs")
        self.children = [comment_line]
        self.cmd_owner = 1

    def cmd(self, tokens, no_flag):
        '''Comment on some jobs that have been run'''
        if no_flag:
            return FAIL, []

        job_info = system_state.cnm_connector.get_uncommented_jobs()
        job_names = []
        form_text = FORM_TEXT_HEADER
        for job_name, job_desc in job_info:
            form_text += "\n[X] %20s: %50s" % (job_name, job_desc)
            job_names.append(job_name)

        if len(job_names) == 0:
            return FAIL, ["No jobs require comments."]

        if len(tokens) > 1:
            comments = ' '.join(tokens[1:])
            response = system_state.cnm_connector.post_comments(True, job_names, comments)
            system_state.set_comment_counter()
            return response["command_status"], response

        submit, publish, job_names, comments = get_comments(form_text)
        if not job_names:
            return FAIL, ["Comment does not apply to any jobs."]
        if not comments:
            return FAIL, ["No comments made."]
        if not submit:
            return FAIL, []

        response = system_state.cnm_connector.post_comments(publish, job_names, comments)
        system_state.set_comment_counter()
        system_state.set_comment_counter()
        return response["command_status"], response

