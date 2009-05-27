from django.conf.urls.defaults import patterns, url
from django.contrib.auth.decorators import login_required
from django_restapi.responder import JsonDictResponder
from CnmResource import CnmResource
from Exceptions import InvalidJobName
import StringIO, traceback
from bombardier_core.static_data import OK, FAIL

class MachineStartTestEntry(CnmResource):
    @login_required
    def create(self, request, machine_name):
        output = {"status": OK}
        try:
            dispatcher = self.get_dispatcher()
            server_home = CnmResource.get_server_home()
            dispatcher.set_server_home(request.user, server_home)
            output = dispatcher.start_job(request.user, machine_name)
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
   url(r'^json/machine/start_test/(?P<machine_name>.*)', MachineStartTestEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/cleanup', MachineCleanupEntry(permitted_methods = ['POST'])),
   url(r'^json/job/join/(?P<job_name>.*)', JobJoinEntry()),
   url(r'^json/job/poll/(?P<job_name>.*)', JobPollEntry()),
)

