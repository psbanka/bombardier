"CnmResource module"
from django_restapi.resource import Resource
from configs.models import ServerConfig
import os
from Exceptions import InvalidServerHome, DispatcherOffline
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
        try:
            dispatcher = Pyro.core.getProxyForURI("PYRONAME://dispatcher")
            dispatcher.check_status()
        except Pyro.core.ProtocolError:
            raise DispatcherOffline()

        return dispatcher

    @classmethod
    def dump_exception(cls, request):
        "Pretty print an exception"
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


