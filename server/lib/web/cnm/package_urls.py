from django.conf.urls.defaults import patterns, url
from django_restapi.model_resource import Collection, Entry, reverse
from django_restapi.responder import JsonDictResponder, JSONResponder, YamlFileResponder
from django_restapi.resource import Resource
from configs.models import ServerConfig
import syck, glob
import os, sys
from django.contrib.auth.decorators import login_required
from django.http import HttpResponse
from Exceptions import InvalidServerHome
from MachineConfig import MachineConfig
from MachineInterface import MachineInterface

def dump_json(data):
    response = HttpResponse(mimetype = "application/json")
    response_dict = syck.load(data)
    simplejson.dump(response_dict, response)

def get_server_home():
    config_entry = ServerConfig.objects.get(name="server_home")
    server_home =config_entry.value
    if not os.path.isdir(server_home):
        raise InvalidServerHome(server_home)
    return server_home


#==================================
import Pyro.core
import time


class MachineTestEntry(Resource):
    @login_required
    def read(self, request, machine_name):
        dispatcher = Pyro.core.getProxyForURI("PYRONAME://dispatcher")
        server_home = get_server_home()
        dispatcher.set_server_home(server_home)
        job = dispatcher.start_job(request.user, machine_name)
        return dump_json(job)

# FIXME: TEST SHOULD BE A POST
urlpatterns = patterns('',
   url(r'^json/machine/test/(?P<machine_name>.*)', MachineTestEntry(permitted_methods = ['GET'])),
)

