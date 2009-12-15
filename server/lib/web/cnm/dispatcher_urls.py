"Package urls module"
from django.conf.urls.defaults import patterns, url
from django.contrib.auth.decorators import login_required
from django_restapi.responder import JsonDictResponder
from CnmResource import CnmResource
from Exceptions import InvalidInput, InvalidDispatcherAction
from Exceptions import DispatcherAlreadyStarted, DispatcherOffline
from bombardier_core.static_data import OK, FAIL
import os, yaml

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
    return value

class PackageBuildEntry(CnmResource):
    "Create a new package based on the information it contains"
    @login_required
    def create(self, request, package_name):
        "Create a package"
        output = {"status": OK}
        try:
            svn_user = safe_get(request, "svn_user")
            svn_password = safe_get(request, "svn_password")
            dispatcher = self.get_dispatcher()
            server_home = CnmResource.get_server_home()
            dispatcher.set_server_home(request.user, server_home)
            output = dispatcher.package_build_job(request.user, package_name,
                                                  svn_user, svn_password) 
        except Exception:
            output.update(self.dump_exception(request))
        responder = JsonDictResponder(output)
        return responder.element(request)

class PackageActionEntry(CnmResource):
    "Run a package action job on a remote machine "
    @login_required
    def create(self, request, package_name):
        "Dispatch a package action"
        output = {"status": OK}
        try:
            action = safe_get(request, "action")
            machine_name = safe_get(request, "machine")
            dispatcher = self.get_dispatcher()
            server_home = CnmResource.get_server_home()
            dispatcher.set_server_home(request.user, server_home)
            output = dispatcher.package_action_job(request.user, package_name,
                                                   action, machine_name) 
        except Exception:
            output.update(self.dump_exception(request))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineEnableEntry(CnmResource):
    "MachineEnableEntry is used to share ssh keys to a machine"
    @login_required
    def create(self, request, machine_name):
        "Dispatch key sharing"
        output = {"status": OK}
        try:
            post_dict = yaml.load(request.POST.get("yaml"))
            password = post_dict.get('password')
            if not password:
                raise InvalidInput("Password needed")
            dispatcher = self.get_dispatcher()
            server_home = CnmResource.get_server_home()
            dispatcher.set_server_home(request.user, server_home)
            output = dispatcher.enable_job(request.user, machine_name, password)
        except Exception:
            output.update(self.dump_exception(request))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineDisableEntry(CnmResource):
    "MachineDisableEntry is remove ssh keys from a machine"
    @login_required
    def create(self, request, machine_name):
        "Dispatch key removal"
        output = {"status": OK}
        try:
            dispatcher = self.get_dispatcher()
            server_home = CnmResource.get_server_home()
            dispatcher.set_server_home(request.user, server_home)
            output = dispatcher.disable_job(request.user, machine_name)
        except Exception:
            output.update(self.dump_exception(request))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineStatusEntry(CnmResource):
    "Status check class for machines"
    @login_required
    def create(self, request, machine_name):
        "Check status of on a remote machine"
        output = {"status": OK}
        try:
            dispatcher = self.get_dispatcher()
            server_home = CnmResource.get_server_home()
            dispatcher.set_server_home(request.user, server_home)
            output = dispatcher.check_status_job(request.user, machine_name)
        except Exception:
            output.update(self.dump_exception(request))
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
        output = {"status": OK}
        try:
            dispatcher = self.get_dispatcher()
            server_home = CnmResource.get_server_home()
            dispatcher.set_server_home(request.user, server_home)
            output = dispatcher.reconcile_job(request.user, machine_name)
        except Exception:
            output.update(self.dump_exception(request))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineUnPushEntry(CnmResource):
    "Remove cleartext configuration data to a machine for troubleshooting"
    @login_required
    def create(self, request, machine_name):
        "Run a job to initialize bombardier client on a machine"
        output = {"status": OK}
        try:
            dispatcher = self.get_dispatcher()
            server_home = CnmResource.get_server_home()
            dispatcher.set_server_home(request.user, server_home)
            output = dispatcher.unpush_job(request.user, machine_name)
        except Exception:
            output.update(self.dump_exception(request))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachinePushEntry(CnmResource):
    "Push cleartext configuration data to a machine for troubleshooting"
    @login_required
    def create(self, request, machine_name):
        "Run a job to initialize bombardier client on a machine"
        output = {"status": OK}
        try:
            dispatcher = self.get_dispatcher()
            server_home = CnmResource.get_server_home()
            dispatcher.set_server_home(request.user, server_home)
            output = dispatcher.push_job(request.user, machine_name)
        except Exception:
            output.update(self.dump_exception(request))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineStartInitEntry(CnmResource):
    "Initialize bombardier client class"
    @login_required
    def create(self, request, machine_name):
        "Run a job to initialize bombardier client on a machine"
        output = {"status": OK}
        try:
            dispatcher = self.get_dispatcher()
            server_home = CnmResource.get_server_home()
            dispatcher.set_server_home(request.user, server_home)
            output = dispatcher.init_job(request.user, machine_name)
        except Exception:
            output.update(self.dump_exception(request))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineStartDistEntry(CnmResource):
    "Distutils package install class"
    @login_required
    def create(self, request, machine_name):
        "Run a job that installs a python distutils package on a machine"
        output = {"status": OK}
        try:
            dist_name = safe_get(request, "dist")
            dispatcher = self.get_dispatcher()
            server_home = CnmResource.get_server_home()
            dispatcher.set_server_home(request.user, server_home)
            output = dispatcher.dist_job(request.user, machine_name, dist_name)
        except Exception:
            output.update(self.dump_exception(request))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineStartTestEntry(CnmResource):
    "Test entry class"
    @login_required
    def create(self, request, machine_name):
        "Run a test job on a machine"
        output = {"status": OK}
        try:
            dispatcher = self.get_dispatcher()
            server_home = CnmResource.get_server_home()
            dispatcher.set_server_home(request.user, server_home)
            output = dispatcher.test_job(request.user, machine_name)
        except Exception:
            output.update(self.dump_exception(request))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineCleanupEntry(CnmResource):
    "Machine connection cleanup"
    @login_required
    def create(self, request):
        "Clean up all machine connections in the dispatcher"
        output = {"status": OK}
        try:
            dispatcher = self.get_dispatcher()
            output = dispatcher.cleanup_connections(request.user)
        except DispatcherOffline:
            output = {}
        except Exception:
            output.update(self.dump_exception(request))
        responder = JsonDictResponder(output)
        return responder.element(request)

class JobJoinEntry(CnmResource):
    "Job join class"
    @login_required
    def read(self, request, job_name):
        "Tell dispatcher to wait a job thread to return"
        output = {"status": OK}
        try:
            dispatcher = self.get_dispatcher()
            output = dispatcher.job_join(request.user, job_name, 10)
        except Exception:
            output.update(self.dump_exception(request))
        responder = JsonDictResponder(output)
        return responder.element(request)

class JobPollEntry(CnmResource):
    "Job polling class"
    @login_required
    def read(self, request, job_name):
        "Ask dispatcher for output from a job"
        output = {"status": OK}
        try:
            dispatcher = self.get_dispatcher()
            output = dispatcher.job_poll(request.user, job_name)
        except Exception:
            output.update(self.dump_exception(request))
        responder = JsonDictResponder(output)
        return responder.element(request)

class DispatcherControlEntry(CnmResource):
    "Dispatcher control handler"
    @login_required
    def read(self, request, action):
        "Check dispatcher status, time and running jobs"
        output = {"command_status" : OK,
                  "command_output" : '',
                 }
        try:
            dispatcher = self.get_dispatcher()
            if action == "status":
                output.update( dispatcher.check_status() )
            else:
                raise InvalidDispatcherAction(action)
        except DispatcherOffline:
            output["command_status"] = FAIL
            output["command_output"] = "Dispatcher is offline."
        except Exception:
            output.update(self.dump_exception(request))
        responder = JsonDictResponder(output)
        return responder.element(request)

    @login_required
    def create(self, request, action):
        "Set password on the dispatcher from request"
        output = {"command_status" : OK}
        try:
            if action == "set-password":
                dispatcher = self.get_dispatcher()
                server_home = CnmResource.get_server_home()
                dispatcher.set_server_home(request.user, server_home)
                password = request.POST.get("password", "")
                output = dispatcher.set_password(password)

            elif action == "start":
                try:
                    status = self.start_dispatcher()
                    output["command_status"] = status
                    output["command_output"] = ["Dispatcher started"]
                except DispatcherAlreadyStarted:
                    output["command_output"] = ["Dispatcher already started"]

            elif action == "stop":
                status = self.stop_dispatcher(request.user)
                output["command_status"] = status
                output["command_output"] = ["Dispatcher stopped"]

            else:
                print "INVALID ACTION %s" % action
                raise InvalidDispatcherAction(action)
        except Exception:
            output.update(self.dump_exception(request))
        responder = JsonDictResponder(output)
        return responder.element(request)

urlpatterns = patterns('',
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
   url(r'^json/machine/init/(?P<machine_name>.*)',
       MachineStartInitEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/dist/(?P<machine_name>.*)',
       MachineStartDistEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/start_test/(?P<machine_name>.*)',
       MachineStartTestEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/cleanup_connections',
       MachineCleanupEntry(permitted_methods = ['POST'])),
   url(r'^json/job/join/(?P<job_name>.*)', JobJoinEntry()),
   url(r'^json/job/poll/(?P<job_name>.*)', JobPollEntry()),
   url(r'^json/dispatcher/(?P<action>.*)', 
       DispatcherControlEntry(permitted_methods = ['GET', 'POST'])),
)

