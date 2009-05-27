from django.conf.urls.defaults import patterns, url
from django_restapi.model_resource import Collection, Entry, reverse
from django_restapi.responder import JsonDictResponder, JSONResponder, YamlFileResponder
from django_restapi.resource import Resource
from configs.models import Machine, Include, Bom, ServerConfig, Package
import ServerConfigFile
import syck, glob
import os
import MachineConfig
from django.contrib.auth.decorators import login_required
from django.http import HttpResponseForbidden, HttpResponse
from Exceptions import InvalidServerHome
from django.utils import simplejson
from bombardier_core.static_data import OK, FAIL

import Pyro.core
from django_restapi.resource import Resource
import StringIO
import traceback

class CnmResource(Resource):
    def __init__(self, authentication=None, permitted_methods=None,
                 mimetype=None):
        Resource.__init__(self, authentication, permitted_methods, mimetype)

    @classmethod
    def dump_json(cls, data):
        response = HttpResponse(mimetype = "application/json")
        response_dict = syck.load(data)
        return simplejson.dump(response_dict, response)

    @classmethod
    def get_server_home(cls):
        config_entry = ServerConfig.objects.get(name="server_home")
        server_home = config_entry.value
        if not os.path.isdir(server_home):
            raise InvalidServerHome(server_home)
        return server_home

    @classmethod
    def get_dispatcher(cls):
        dispatcher = Pyro.core.getProxyForURI("PYRONAME://dispatcher")
        return dispatcher

    def dump_exception(self, request, err):
        exc = StringIO.StringIO()
        traceback.print_exc(file=exc)
        exc.seek(0)
        data = exc.read()
        traceback_data = []
        for line in data.split('\n'):
            traceback_data.append(line)
            ermsg = "%% %s" % line
            print "%s : %s " % (request.user, ermsg)
        return {"status": FAIL, "traceback": traceback_data}


