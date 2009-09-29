"CnmResource module"
from django_restapi.resource import Resource
from configs.models import ServerConfig
import os, time
import yaml
from Exceptions import InvalidServerHome, DispatcherOffline
from Exceptions import DispatcherAlreadyStarted, DispatcherError
from daemonize import daemonize

from bombardier_core.static_data import OK, FAIL
import Pyro.core
import StringIO
import traceback
from commands import getstatusoutput as gso
from signal import SIGTERM

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
            config_entry = ServerConfig.objects.get(name="dispatcher_uri")
            dispatcher_uri = config_entry.value
            dispatcher = Pyro.core.getProxyForURI(dispatcher_uri)
            dispatcher.check_status()
        except ServerConfig.DoesNotExist:
            raise DispatcherOffline()
        except Pyro.core.ProtocolError:
            raise DispatcherOffline()

        return dispatcher

    @classmethod
    def _check_dispatcher_running(cls, uri):
        "Check for tcp port listener"
        tcp_port = uri.split(':')[-1].split('/')[0]
        cmd = 'lsof -i tcp:%s | grep python | grep -v grep' % tcp_port
        _status, output = gso(cmd)
        if output:
            return True
        return False

    @classmethod
    def _check_dispatcher_stopped(cls, pid):
        "Check for running pid"
        cmd = "lsof -p %s | wc -l" % pid
        _status, output = gso(cmd)
        if output.strip() == "0":
            return True
        return False

    @classmethod
    def _wait_for_dispatcher(cls, check_func, arg, timeout=10.0):
        "Use check_func to check for desired dispatcher state"
        start_time = time.time()
        while not check_func(arg):
            if (time.time() - start_time) > timeout:
                return FAIL
            time.sleep(0.25)
        time.sleep(1)
        return OK

    def stop_dispatcher(self, username):
        "Stop dispatcher and cleanup ServerConfig items"
        try:
            dispatcher = self.get_dispatcher()
            dispatcher.cleanup_connections(username)
        except DispatcherOffline:
            pass
        try:
            pid = ServerConfig.objects.get(name="dispatcher_pid")
            status = os.kill(int(pid.value), SIGTERM)
            dispatcher_pid = ServerConfig.objects.get(name="dispatcher_pid")
            status = self._wait_for_dispatcher(self._check_dispatcher_stopped,
                                                    dispatcher_pid.value)
            dispatcher_pid.delete()
        except ServerConfig.DoesNotExist:
            pass
        try:
            dispatcher_uri = ServerConfig.objects.get(name="dispatcher_uri")
            dispatcher_uri.delete()
            if status == FAIL:
                return FAIL
        except ServerConfig.DoesNotExist:
            pass
        return OK

    def start_dispatcher(self):
        "Start dispatcher if it's not running"
        try:
            self.get_dispatcher()
            raise DispatcherAlreadyStarted()
        except DispatcherOffline:
            pass
        status = daemonize()
        if status == OK:
            dispatcher_dict = None
            while not dispatcher_dict:
                dispatcher_info = open("dispatcher_info.yml").read()
                dispatcher_dict = yaml.load(dispatcher_info)
            self._standard_update(dispatcher_dict)
            server_co = ServerConfig.objects.get(name="dispatcher_uri")
            return self._wait_for_dispatcher(self._check_dispatcher_running,
                                                  server_co.value)
        raise DispatcherError("Cannot be started")

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

    @classmethod
    def _form_update(cls, query_dict):
        "Update or create values based on form POST."
        [ obj.delete() for obj in ServerConfig.objects.all() ]
        names = [ i for i in query_dict if i.endswith('name') ]
        for form_entry in names:
            id_num = form_entry.split('-')[1]
            name = query_dict[form_entry]
            value = query_dict['form-%s-value' % id_num]
            if name:
                server_co = ServerConfig.objects.get_or_create(id=id_num)[0]
                server_co.value = value
                server_co.name = name
                server_co.save()

    @classmethod
    def _standard_update(cls, query_dict):
        "Update or create on standard POST"
        for name in query_dict:
            value = query_dict[name]
            try:
                server_co = ServerConfig.objects.get(name=name)
            except ServerConfig.DoesNotExist:
                server_co = ServerConfig.objects.create()
                server_co.name = name
            server_co.value = value
            server_co.save()


