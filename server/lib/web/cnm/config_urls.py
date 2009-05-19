from django.conf.urls.defaults import patterns, url
from django_restapi.responder import JsonDictResponder, JSONResponder, YamlFileResponder
from CnmResource import CnmResource
from configs.models import Client, Include, Bom, ServerConfig, Package
import ServerConfigFile
import syck, glob
import os
import MachineConfig
from django.contrib.auth.decorators import login_required
from django.http import HttpResponseForbidden

MAPPER = {"merged": Client, "client": Client, "include": Include,
          "bom": Bom, "package": Package}

class ConfigEntry(CnmResource):
    @login_required
    def read(self, request, config_type, config_name):
        server_home = self.get_server_home()
        if config_type.lower() == "merged":
            machine_config = MachineConfig.MachineConfig(config_name, "", server_home)
            machine_config.merge()
            responder = JsonDictResponder(machine_config.data)
            return responder.element(request)
        else:
            config_file = os.path.join(server_home, config_type)
            responder = YamlFileResponder(config_file)
            return responder.element(request, config_name)

class ConfigCollection(CnmResource):
    @login_required
    def read(self, request, config_type, config_name):
        objects = MAPPER[config_type.lower()].objects.filter(name__startswith=config_name)
        responder = JSONResponder()
        responder.expose_fields = ["name"]
        return responder.list(request, objects)

#==================================

class ServerConfigCollection(CnmResource):
    @login_required
    def read(self, request):
        server_config_objects = ServerConfig.objects.all()
        output = {}
        for server_co in server_config_objects:
            output[server_co.name] = server_co.value
        responder = JsonDictResponder(output)
        return responder.element(request)

    @login_required
    def create(self, request):
        if not request.user.is_superuser:
            return HttpResponseForbidden()
        query_dict = request.POST
        for entry in query_dict:
            server_co = ServerConfig.objects.get(name=entry)
            server_co.value = query_dict.get(entry)
            server_co.save()
        server_config_objects = ServerConfig.objects.all()
        output = {}
        for server_co in server_config_objects:
            output[server_co.name] = server_co.value
        responder = JsonDictResponder(output)
        return responder.element(request)

class ServerConfigSyncCollection(CnmResource):
    @login_required
    def read(self, request):
        server_config_objects = ServerConfig.objects.all()
        for server_co in server_config_objects:
            server_co.delete()
        server_config_file = ServerConfigFile.ServerConfigFile()
        config_data = server_config_file.global_config
        for element in config_data:
            sc = ServerConfig(name=element, value=config_data[element])
            sc.save()
        responder = JsonDictResponder(config_data)
        return responder.element(request)

class DbSyncCollection(CnmResource):
    @login_required
    def create(self, request):
        config_data = self._server_sync()
        config_data.update(self._server_home_sync())
        config_data["status"] = "OK"
        responder = JsonDictResponder(config_data)
        return responder.element(request)

    def _server_sync(self):
        server_config_objects = ServerConfig.objects.all()
        for server_co in server_config_objects:
            server_co.delete()
        server_config_file = ServerConfigFile.ServerConfigFile()
        config_data = server_config_file.global_config
        for element in config_data:
            sc = ServerConfig(name=element, value=config_data[element])
            sc.save()
        return config_data

    def _server_home_sync(self):
        new_config_objects = []
        server_home = self.get_server_home()
        config_data = {}
        for config_type in MAPPER:
            if config_type == "merged":
                continue
            config_data[config_type] = []
            config_objects = MAPPER[config_type.lower()].objects.all()
            for config_object in config_objects:
                config_object.delete()
            config_wildcard = os.path.join(server_home, config_type, "*.yml")
            config_files = glob.glob(config_wildcard)
            for config_file in config_files:
                base_name = config_file.split(os.path.sep)[-1]
                config_name = base_name.split('.yml')[0]
                config_object = MAPPER[config_type.lower()](name=config_name)
                config_object.save()
                config_data[config_type].append(base_name)
            new_config_objects += MAPPER[config_type.lower()].objects.all()
        return config_data

urlpatterns = patterns('',
   url(r'^json/server/config', ServerConfigCollection(permitted_methods = ['POST', "GET"])),
   url(r'^json/dbsync', DbSyncCollection(permitted_methods = ['POST'])),
   url(r'^json/(?P<config_type>.*)/search/(?P<config_name>.*)', ConfigCollection()),
   url(r'^json/(?P<config_type>.*)/name/(?P<config_name>.*)$', ConfigEntry(permitted_methods=['GET'])),
)

