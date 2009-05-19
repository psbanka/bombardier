from django.conf.urls.defaults import patterns, url
from django.contrib.auth.decorators import login_required
from django_restapi.responder import JsonDictResponder
from CnmResource import CnmResource
import Pyro.core
from Exceptions import InvalidJobName

class MachineStartTestEntry(CnmResource):
    @login_required
    def create(self, request, machine_name):
        dispatcher = Pyro.core.getProxyForURI("PYRONAME://dispatcher")
        server_home = CnmResource.get_server_home()
        dispatcher.set_server_home(request.user, server_home)
        job = dispatcher.start_job(request.user, machine_name)
        responder = JsonDictResponder(job)
        return responder.element(request)

class MachineCleanupEntry(CnmResource):
    @login_required
    def create(self, request):
        dispatcher = Pyro.core.getProxyForURI("PYRONAME://dispatcher")
        status = dispatcher.cleanup(request.user)
        responder = JsonDictResponder(status)
        return responder.element(request)

class JobJoinEntry(CnmResource):
    @login_required
    def read(self, request, job_name):
        dispatcher = Pyro.core.getProxyForURI("PYRONAME://dispatcher")
        status = dispatcher.job_join(request.user, job_name, 10)
        responder = JsonDictResponder(status)
        return responder.element(request)

class JobPollEntry(CnmResource):
    @login_required
    def read(self, request, job_name):
        dispatcher = Pyro.core.getProxyForURI("PYRONAME://dispatcher")
        status = dispatcher.job_poll(request.user, job_name)
        responder = JsonDictResponder(status)
        return responder.element(request)

urlpatterns = patterns('',
   url(r'^json/machine/start_test/(?P<machine_name>.*)', MachineStartTestEntry(permitted_methods = ['POST'])),
   url(r'^json/machine/cleanup', MachineCleanupEntry(permitted_methods = ['POST'])),
   url(r'^json/job/join/(?P<job_name>.*)', JobJoinEntry()),
   url(r'^json/job/poll/(?P<job_name>.*)', JobPollEntry()),
)

