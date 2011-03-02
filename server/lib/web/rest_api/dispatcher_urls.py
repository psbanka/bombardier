"Package urls module"
from django.conf.urls.defaults import patterns, url
from django.contrib.auth.decorators import login_required
from django_restapi.responder import JsonDictResponder
from CnmResource import CnmResource
from bombardier_server.cnm.Exceptions import InvalidInput, InvalidDispatcherAction
from bombardier_server.cnm.Exceptions import DispatcherAlreadyStarted, DispatcherOffline
from bombardier_server.cnm.Exceptions import CnmServerException
from bombardier_core.static_data import OK, FAIL, ABORTED_JOB_NAME
from bombardier_core.static_data import LOCAL_TYPE, BDR_CLIENT_TYPE

import os, yaml
from configs.models import Comment, CommentedJob

VALID_FILE_CHARS  = [ chr(x) for x in range(ord('a'), ord('z')+1) ]
VALID_FILE_CHARS += [ chr(x) for x in range(ord('A'), ord('Z')+1) ]
VALID_FILE_CHARS += [ chr(x) for x in range(ord('0'), ord('9')+1) ]
VALID_FILE_CHARS += [ '_', '-', '.' ]
VALID_FILE_CHARS  = set(VALID_FILE_CHARS)

def check_string(value):
    "Check string to make sure it only contains valid characters"
    if not set(value).issubset(VALID_FILE_CHARS):
        bad_characters = set(value) - VALID_FILE_CHARS
        raise InvalidInput(bad_characters)

def safe_get(request, option):
    "User check_string to validate input from a POST"
    value = request.POST.get(option, "")
    check_string(value)
    return str(value)

class PackageBuildEntry(CnmResource):
    "Create a new package based on the information it contains"
    @login_required
    def create(self, request, package_name):
        "Create a package"
        output = {"command_status": OK}
        try:
            svn_user = safe_get(request, "svn_user")
            svn_password = request.POST.get("svn_password", "")
            debug = safe_get(request, "debug")
            prepare = safe_get(request, "prepare")
            dispatcher = self.get_dispatcher()
            job_name = dispatcher.package_build_job(request.user.username, package_name,
                                                    svn_user, svn_password, debug,
                                                    prepare) 
            dispatcher.queue_job(job_name)
            output = dispatcher.get_job_status(job_name)
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class PackageActionEntry(CnmResource):
    "Run a package action job on a remote machine "
    @login_required
    def create(self, request, package_name):
        "Dispatch a package action"
        output = {"command_status": OK}
        try:
            post_data = yaml.load(request.POST.get("yaml", ''))
            arguments = []
            if post_data:
                action = post_data["action"]
                machine_name = post_data["machine"]
                arguments = post_data.get("arguments", [])
            else:
                action = safe_get(request, "action")
                machine_name = safe_get(request, "machine")
            dispatcher = self.get_dispatcher()
            job_name = dispatcher.package_action_job(request.user.username, package_name,
                                                     action, machine_name, arguments) 
            dispatcher.queue_job(job_name)
            output = dispatcher.get_job_status(job_name)
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineEnableEntry(CnmResource):
# FIXME: need to pass machine_type
    "MachineEnableEntry is used to share ssh keys to a machine"
    @login_required
    def create(self, request, machine_name):
        "Dispatch key sharing"
        output = {"command_status": OK}
        try:
            post_dict = yaml.load(request.POST.get("yaml"))
            password = post_dict.get('password')
            if not password:
                raise InvalidInput("Password needed")
            machine_type = post_dict.get('machine_type')
            if not machine_type:
                raise InvalidInput("Machine type needed")
            dispatcher = self.get_dispatcher()
            job_name = dispatcher.enable_job(request.user.username,
                                             machine_name, machine_type,
                                             password)
            dispatcher.queue_job(job_name)
            output = dispatcher.get_job_status(job_name)
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineDisableEntry(CnmResource):
# FIXME: Need to pass yaml
    "MachineDisableEntry is remove ssh keys from a machine"
    @login_required
    def create(self, request, machine_name):
        "Dispatch key removal"
        output = {"command_status": OK}
        try:
            post_dict = yaml.load(request.POST.get("yaml"))
            machine_type = post_dict.get('machine_type')
            if not machine_type:
                raise InvalidInput("Machine type needed")
            dispatcher = self.get_dispatcher()
            job_name = dispatcher.disable_job(request.user.username,
                                              machine_name, machine_type)
            dispatcher.queue_job(job_name)
            output = dispatcher.get_job_status(job_name)
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachinePackageRestoreEntry(CnmResource):
    "Check restore data that can be loaded, and load it"
    @login_required
    def read(self, request, machine_name, package_name):
        "Show restore data that can be restored"
        server_home = self.get_server_home()
        check_string(machine_name)
        check_string(package_name)
        output = {"command_status": OK}
        try:
            dispatcher = self.get_dispatcher()
            output = dispatcher.check_restore_data(request.user.username, machine_name, package_name)
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineStatusEntry(CnmResource):
    "Status check class for machines"
    @login_required
    def create(self, request, machine_name):
        "Check status of on a remote machine"
        output = {"command_status": OK}
        try:
            dispatcher = self.get_dispatcher()
            job_name = dispatcher.check_status_job(request.user.username, machine_name)
            dispatcher.queue_job(job_name)
            output = dispatcher.get_job_status(job_name)
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

    def read(self, request, machine_name):
        "Show cached status for a machine"
        server_home = self.get_server_home()
        check_string(machine_name)
        status_path = os.path.join(server_home, "status", machine_name)
        machine_status = {}
        if os.path.isfile(status_path):
            machine_status = yaml.load(status_path)
        responder = JsonDictResponder(machine_status)
        return responder.element(request)


class MachineStartReconcileEntry(CnmResource):
    "Machine reconcile class"
    @login_required
    def create(self, request, machine_name):
        "Start a reconcile job on a machine"
        output = {"command_status": OK}
        if 1 == 1:
        #try:
            dispatcher = self.get_dispatcher()
            data = dispatcher.check_status()
            job_name = dispatcher.reconcile_job(request.user.username, machine_name)
            dispatcher.queue_job(job_name)
            output = dispatcher.get_job_status(job_name)
        else:
        #except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineUnPushEntry(CnmResource):
    "Remove cleartext configuration data to a machine for troubleshooting"
    @login_required
    def create(self, request, machine_name):
        "Run a job to initialize bombardier client on a machine"
        output = {"command_status": OK}
        try:
            dispatcher = self.get_dispatcher()
            job_name = dispatcher.unpush_job(request.user.username, machine_name)
            dispatcher.queue_job(job_name)
            output = dispatcher.get_job_status(job_name)
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachinePushEntry(CnmResource):
    "Push cleartext configuration data to a machine for troubleshooting"
    @login_required
    def create(self, request, machine_name):
        "Run a job to initialize bombardier client on a machine"
        output = {"command_status": OK}
        try:
            dispatcher = self.get_dispatcher()
            job_name = dispatcher.push_job(request.user.username, machine_name)
            dispatcher.queue_job(job_name)
            output = dispatcher.get_job_status(job_name)
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineStartSetupEntry(CnmResource):
    "Initialize bombardier client class"
    @login_required
    def create(self, request, machine_name):
        "Run a job to initialize bombardier client on a machine"
        output = {"command_status": OK}
        try:
            post_dict = yaml.load(request.POST.get("yaml"))
            password = post_dict.get('password')
            if not password:
                raise InvalidInput("Password needed")
            dispatcher = self.get_dispatcher()
            init_job_name = dispatcher.setup_machine(request.user.username, machine_name, password)
            if init_job_name == ABORTED_JOB_NAME:
                output = {"command_status": FAIL,
                          "job_name": ABORTED_JOB_NAME,
                          "command output": "Could not find dist files for setup."}
            else:
                output = dispatcher.get_job_status(init_job_name)
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineStartInitEntry(CnmResource):
    "Initialize bombardier client class"
    @login_required
    def create(self, request, machine_name):
        "Run a job to initialize bombardier client on a machine"
        output = {"command_status": OK}
        try:
            dispatcher = self.get_dispatcher()
            job_name = dispatcher.init_job(request.user.username, machine_name)
            dispatcher.queue_job(job_name)
            output = dispatcher.get_job_status(job_name)
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineStartDistEntry(CnmResource):
    "Distutils package install class"
    @login_required
    def create(self, request, machine_name):
        "Run a job that installs a python distutils package on a machine"
        output = {"command_status": OK}
        try:
            dist_name = safe_get(request, "dist")
            dispatcher = self.get_dispatcher()
            job_name = dispatcher.dist_job(request.user.username, machine_name, dist_name)
            dispatcher.queue_job(job_name)
            output = dispatcher.get_job_status(job_name)
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class VersionCollection(CnmResource):
    "Status config collection"
    @login_required
    def read(self, request):
        "Default read method"
        from bombardier_server._version import version_info as srv
        from bombardier_core._version import version_info as crv
        output = ["Server: %s-%s" % (srv.get("branch_nick"), srv.get("revno")),
                  "Server-core: %s-%s" % (crv.get("branch_nick"), crv.get("revno")),
                 ]
        dispatcher = self.get_dispatcher()
        client_version_info = dispatcher.get_client_versions()
        for key in client_version_info:
            output.append("%s: %s" % (key, client_version_info[key]))
        responder = JsonDictResponder(output)
        open("/tmp/FOOP.txt", 'a').write("output: %s\n" % output)
        return responder.element(request)

class MachineStartTestEntry(CnmResource):
    "Test entry class"
    @login_required
    def create(self, request, machine_name):
        "Run a test job on a machine"
        output = {"command_status": OK}
        try:
            dispatcher = self.get_dispatcher()
            data = dispatcher.check_status()
            job_name = dispatcher.test_job(request.user.username,
                        machine_name, BDR_CLIENT_TYPE)
            dispatcher.queue_job(job_name)
            output = dispatcher.get_job_status(job_name)
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineCleanupEntry(CnmResource):
    "Machine connection cleanup"
    @login_required
    def create(self, request):
        "Clean up all machine connections in the dispatcher"
        output = {"command_status": OK}
        try:
            dispatcher = self.get_dispatcher()
            # FIXME: HOW CAN THIS WORK? NO METHOD WITH THIS NAME.
            output = dispatcher.cleanup_connections(request.user.username)
        except DispatcherOffline:
            output = {}
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineClearBrokenEntry(CnmResource):
    "Clear out the broken job dictionary for a machine"
    @login_required
    def create(self, request):
        "Clean up all machine connections in the dispatcher"
        machine_name = request.POST.get("machine", None)
        output = {"command_status": OK}
        try:
            dispatcher = self.get_dispatcher()
            output = dispatcher.clear_broken(machine_name)
        except DispatcherOffline:
            output = {}
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)


class MachineStopJobsEntry(CnmResource):
    "Stop all running jobs and anything pending"
    @login_required
    def create(self, request):
        "Clean up all machine connections in the dispatcher"
        machine_name = request.POST.get("machine", None)
        output = {"command_status": OK}
        try:
            dispatcher = self.get_dispatcher()
            output = dispatcher.stop_all_jobs(request.user.username, machine_name)
        except DispatcherOffline:
            output = {}
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineShowJobsEntry(CnmResource):
    "Show all the job information for a machine"
    @login_required
    def read(self, request, machine_name):
        "Get machine job info"
        output = {"command_status": OK}
        try:
            dispatcher = self.get_dispatcher()
            output = dispatcher.show_jobs(machine_name)
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class JobCommentCollection(CnmResource):
    "Returns comments made"
    @login_required
    def read(self, request):
        "Return all comments"
        output = {"command_status": OK}
        comment_lines = []
        try:
            comments = Comment.objects.all()
            for comment in comments:
                commented_jobs = CommentedJob.objects.filter(comment_id = comment)
                job_name_list = []
                [ job_name_list.append(commented_job.job_name) for commented_job in commented_jobs ]
                str = "%s | %s | %s | %s"
                str = str % (comment.time, comment.username, comment.text, ','.join(job_name_list))
                comment_lines.append(str)
            output["comments"] = comment_lines
        except Exception, x:
            output["command_status"] = FAIL
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class JobCommentPendingEntry(CnmResource):
    "Finds jobs that needs comments and allows users to comment"
    @login_required
    def read(self, request):
        "Give me a list of jobs that needs commenting for this user"
        output = {"command_status": OK}
        try:
            dispatcher = self.get_dispatcher()
            output = dispatcher.get_jobs_pending_comments(request.user.username)
        except Exception, x:
            output["command_status"] = FAIL
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

    @login_required
    def create(self, request):
        "Add some comments about a job that has run"
        output = {"command_status": OK}
        try:
            dispatcher = self.get_dispatcher()
            post_dict = yaml.load(request.POST.get("yaml"))
            job_names = post_dict.get("job_names", [])
            comment_text = post_dict.get("comment", None)
            publish_flag = post_dict.get("publish", False)
            comment = Comment(text=comment_text, username=str(request.user.username),
                              publish=publish_flag)
            comment.save()
            for job_name in job_names:
                machine_name = dispatcher.get_machine_name(job_name)
                commented_job = CommentedJob(comment_id=comment,
                                             job_name=job_name,
                                             username=str(request.user.username),
                                             machine_name=machine_name,
                                            )
                commented_job.save()
                output[job_name] = "comment applied."
                dispatcher.note_comment(job_name)
        except Exception, x:
            output["command_status"] = FAIL
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class JobJoinEntry(CnmResource):
    "Job join class"
    @login_required
    def read(self, request, job_name):
        "Tell dispatcher to wait a job thread to return"
        output = {"command_status": OK}
        try:
            dispatcher = self.get_dispatcher()
            output = dispatcher.job_join(request.user.username, job_name, 10)
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class JobPollEntry(CnmResource):
    "Job polling class"
    @login_required
    def create(self, request):
        "Ask dispatcher for output from a job"
        output = {"command_status": OK}
        try:
            dispatcher = self.get_dispatcher()
            post_dict = yaml.load(request.POST.get("yaml"))
            job_names = post_dict.get("job_names", [])
            output = dispatcher.job_poll(job_names)
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class JobKillEntry(CnmResource):
    "Job kill class"
    @login_required
    def create(self, request, job_name):
        "Tell dispatcher to terminate a job"
        output = {"command_status": OK}
        try:
            dispatcher = self.get_dispatcher()
            output = dispatcher.job_kill(request.user.username, job_name)
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

class DispatcherControlEntry(CnmResource):
    "Dispatcher control handler"
    @login_required
    def read(self, request):
        "Check dispatcher status, time and running jobs"
        output = {"command_status" : OK,
                  "command_output" : '',
                 }
        try:
            dispatcher = self.get_dispatcher()
            output.update( dispatcher.check_status() )
        except DispatcherOffline:
            output["command_status"] = FAIL
            output["command_output"] = "Dispatcher is offline."
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

    @login_required
    def create(self, request):
        "Set password on the dispatcher from request"
        output = {"command_status" : OK}
        try:
            try:
                uri = request.POST.get("uri", "")
                if not uri:
                    server_home = self.get_server_home()
                    uri = self.get_dispatcher_info().get("dispatcher_uri", '')
                status = self.attach_dispatcher(uri)
                msg = "Attached to dispatcher %s" % uri
                output["command_output"] = [msg]
            except DispatcherOffline:
                msg = "Dispatcher %s is offline / cannot attach" % uri
                output["command_output"] = [msg]
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

urlpatterns = patterns('',
   url(r'^json/machine/restore/(?P<machine_name>.*)/(?P<package_name>.*)',
       MachinePackageRestoreEntry(permitted_methods = ['GET'])),
   url(r'^json/package_action/(?P<package_name>.*)',
       PackageActionEntry(permitted_methods = ['POST'])),
   url(r'^json/package_build/(?P<package_name>.*)',
       PackageBuildEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/enable/(?P<machine_name>.*)$',
       MachineEnableEntry(permitted_methods=['POST'])),
   url(r'^json/machine/disable/(?P<machine_name>.*)$',
       MachineDisableEntry(permitted_methods=['POST'])),
   url(r'^json/machine/status/(?P<machine_name>.*)',
       MachineStatusEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/check_status/(?P<machine_name>.*)',
       MachineStatusEntry(permitted_methods = ['GET','POST'])),
   url(r'^json/machine/reconcile/(?P<machine_name>.*)',
       MachineStartReconcileEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/unpush/(?P<machine_name>.*)',
       MachineUnPushEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/push/(?P<machine_name>.*)',
       MachinePushEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/setup/(?P<machine_name>.*)',
       MachineStartSetupEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/init/(?P<machine_name>.*)',
       MachineStartInitEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/dist/(?P<machine_name>.*)',
       MachineStartDistEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/start_test/(?P<machine_name>.*)',
       MachineStartTestEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/cleanup_connections',
       MachineCleanupEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/stop-jobs/', 
        MachineStopJobsEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/clear-broken-jobs/', 
        MachineClearBrokenEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/show-jobs/(?P<machine_name>.*)', 
        MachineShowJobsEntry(permitted_methods = ['GET'])),
   url(r'^json/job/comment/pending/', 
        JobCommentPendingEntry(permitted_methods = ['GET', 'POST'])),
   url(r'^json/comments/', 
        JobCommentCollection(permitted_methods = ['GET'])),
   url(r'^json/job/join/(?P<job_name>.*)', JobJoinEntry()),
   url(r'^json/job/poll/',
        JobPollEntry(permitted_methods = ['POST'])),
   url(r'^json/job/kill/(?P<job_name>.*)', 
        JobKillEntry(permitted_methods = ['POST'])),
   url(r'^json/dispatcher/', 
        DispatcherControlEntry(permitted_methods = ['GET', 'POST'])),
   url(r'^json/version', VersionCollection()),
)

