"CnmResource module"
from django_restapi.resource import Resource
from configs.models import ServerConfig
import syck
import os
from django.http import HttpResponse
from Exceptions import InvalidServerHome
from django.utils import simplejson
from bombardier_core.static_data import FAIL

import Pyro.core
import StringIO
import traceback

class CnmResource(Resource):
    "Base resource class"
    def __init__(self, authentication=None, permitted_methods=None,
                 mimetype=None):
        Resource.__init__(self, authentication, permitted_methods, mimetype)

    @classmethod
    def get_server_home(cls):
        "Return server_home"
        config_entry = ServerConfig.objects.get(name="server_home")
        server_home = config_entry.value
        if not os.path.isdir(server_home):
            raise InvalidServerHome(server_home)
        return server_home

    @classmethod
    def get_dispatcher(cls):
        "Create and return a dispatcher"
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


