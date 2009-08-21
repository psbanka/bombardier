from django.conf.urls.defaults import patterns, url
from django.contrib.auth.decorators import login_required
from django_restapi.responder import JsonDictResponder
from CnmResource import CnmResource
from Exceptions import InvalidJobName
import StringIO, traceback
from bombardier_core.static_data import OK, FAIL

VALID_FILE_CHARS  = [ chr(x) for x in range(ord('a'), ord('z')+1) ]
VALID_FILE_CHARS += [ chr(x) for x in range(ord('A'), ord('Z')+1) ]
VALID_FILE_CHARS += [ chr(x) for x in range(ord('0'), ord('9')+1) ]
VALID_FILE_CHARS += [ '_', '-', '.' ]
VALID_FILE_CHARS  = set(VALID_FILE_CHARS)

def safe_get(request, option):
    value = request.POST.get(option, "")
    if not set(value).issubset(VALID_FILE_CHARS):
        bad_characters = set(value) - VALID_FILE_CHARS
        raise InvalidInput(bad_characters)
    return value

class PackageActionEntry(CnmResource):
    @login_required
    def create(self, request, package_name):
        output = {"status": OK}
        try:
            action = safe_get(request, "action")
            machine_name = safe_get(request, "machine")
            dispatcher = self.get_dispatcher()
            server_home = CnmResource.get_server_home()
            dispatcher.set_server_home(request.user, server_home)
            output = dispatcher.package_action_job(request.user, package_name,
                                                   action, machine_name)
        except Exception, err:
            output.update(self.dump_exception(request, err))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineStartReconcileEntry(CnmResource):
    @login_required
    def create(self, request, machine_name):
        output = {"status": OK}
        try:
            dispatcher = self.get_dispatcher()
            server_home = CnmResource.get_server_home()
            dispatcher.set_server_home(request.user, server_home)
            output = dispatcher.reconcile_job(request.user, machine_name)
        except Exception, err:
            output.update(self.dump_exception(request, err))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineStartInitEntry(CnmResource):
    @login_required
    def create(self, request, machine_name):
        output = {"status": OK}
        try:
            dispatcher = self.get_dispatcher()
            server_home = CnmResource.get_server_home()
            dispatcher.set_server_home(request.user, server_home)
            output = dispatcher.init_job(request.user, machine_name)
        except Exception, err:
            output.update(self.dump_exception(request, err))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineStartDistEntry(CnmResource):
    @login_required
    def create(self, request, machine_name):
        output = {"status": OK}
        try:
            dist_name = request.POST.get("dist", "")
            if not set(dist_name).issubset(VALID_FILE_CHARS):
                bad_characters = set(dist_name) - VALID_FILE_CHARS
                raise InvalidInput(bad_characters)
            dispatcher = self.get_dispatcher()
            server_home = CnmResource.get_server_home()
            dispatcher.set_server_home(request.user, server_home)
            output = dispatcher.dist_job(request.user, machine_name, dist_name)
        except Exception, err:
            output.update(self.dump_exception(request, err))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineStartTestEntry(CnmResource):
    @login_required
    def create(self, request, machine_name):
        output = {"status": OK}
        try:
            dispatcher = self.get_dispatcher()
            server_home = CnmResource.get_server_home()
            dispatcher.set_server_home(request.user, server_home)
            output = dispatcher.test_job(request.user, machine_name)
        except Exception, err:
            output.update(self.dump_exception(request, err))
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineCleanupEntry(CnmResource):
    @login_required
    def create(self, request):
        output = {"status": OK}
        try:
            dispatcher = self.get_dispatcher()
            output = dispatcher.cleanup(request.user)
        except Exception, err:
            output.update(self.dump_exception(request, err))
        responder = JsonDictResponder(output)
        return responder.element(request)

class JobJoinEntry(CnmResource):
    @login_required
    def read(self, request, job_name):
        output = {"status": OK}
        try:
            dispatcher = self.get_dispatcher()
            output = dispatcher.job_join(request.user, job_name, 10)
        except Exception, err:
            output.update(self.dump_exception(request, err))
        responder = JsonDictResponder(output)
        return responder.element(request)

class JobPollEntry(CnmResource):
    @login_required
    def read(self, request, job_name):
        output = {"status": OK}
        try:
            dispatcher = self.get_dispatcher()
            output = dispatcher.job_poll(request.user, job_name)
        except Exception, err:
            output.update(self.dump_exception(request, err))
        responder = JsonDictResponder(output)
        return responder.element(request)

urlpatterns = patterns('',
   url(r'^json/package/(?P<package_name>.*)', PackageActionEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/reconcile/(?P<machine_name>.*)', MachineStartReconcileEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/init/(?P<machine_name>.*)', MachineStartInitEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/dist/(?P<machine_name>.*)', MachineStartDistEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/start_test/(?P<machine_name>.*)', MachineStartTestEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/cleanup', MachineCleanupEntry(permitted_methods = ['POST'])),
   url(r'^json/job/join/(?P<job_name>.*)', JobJoinEntry()),
   url(r'^json/job/poll/(?P<job_name>.*)', JobPollEntry()),
)

