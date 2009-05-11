from django.conf.urls.defaults import *
from django_restapi.model_resource import Collection, Entry, reverse
from django_restapi.responder import *
from django_restapi.resource import Resource
from configs.models import Client, Include, Bom, ServerConfig
import ServerConfigFile
import syck, glob, yaml
import os
import MachineConfig

MAPPER = {"merged": Client, "client": Client, "include": Include, "bom": Bom}


def dump_json(data):
    response = HttpResponse(mimetype = "application/json")
    response_dict = syck.load(data)
    simplejson.dump(response_dict, response)

def get_server_home():
    config_entry = ServerConfig.objects.get(name="server_home")
    server_home =config_entry.value
    return server_home

class ConfigEntry(Resource):
    def read(self, request, config_type, config_name):
        server_home = get_server_home()
        if config_type.lower() == "merged":
            machine_config = MachineConfig.MachineConfig(config_name, "", server_home)
            machine_config.merge()
            responder = JsonDictResponder(machine_config.data)
            return responder.element(request)
        else:
            config_file = os.path.join(server_home, config_type)
            responder = YamlFileResponder(config_file)
            return responder.element(request, config_name)

class ConfigCollection(Resource):
    def read(self, request, config_type, config_name):
        objects = MAPPER[config_type.lower()].objects.filter(name__startswith=config_name)
        responder = JSONResponder()
        responder.expose_fields = ["name"]
        return responder.list(request, objects)

class ConfigSyncCollection(Resource):
    def read(self, request):
        new_config_objects = []
        server_home = get_server_home()

        for config_type in MAPPER:
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

            new_config_objects += MAPPER[config_type.lower()].objects.all()
        responder = JSONResponder()
        responder.expose_fields = ["name"]
        return responder.list(request, new_config_objects)

class MergedEntry(Resource):
    def read(self, request, client_name):
        server_home = get_server_home()
        machine_config = MachineConfig.MachineConfig(client_name, "", server_home)
        machine_config.merge()
        responder = JsonDictResponder(machine_config.data)
        return responder.element(request)

#==================================

class ServerConfigCollection(Resource):
    def read(self, request):
        server_config_objects = ServerConfig.objects.all()
        output = {}
        for server_co in server_config_objects:
            output[server_co.name] = server_co.value
        responder = JsonDictResponder(output)
        return responder.element(request)
    def create(self, request):
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

class ServerConfigSyncCollection(Resource):
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

urlpatterns = patterns('',
   url(r'^json/server/config', ServerConfigCollection(permitted_methods = ['POST', "GET"])),
   #url(r'^json/server/set_config', ServerConfigModifyCollection(permitted_methods=['POST'])),
   url(r'^json/server/dbsync', ServerConfigSyncCollection()),
   url(r'^json/dbsync', ConfigSyncCollection()),
   url(r'^json/dbsync/(?P<server_home>)', ConfigSyncCollection()),
   url(r'^json/(?P<config_type>.*)/search/(?P<config_name>.*)', ConfigCollection()),
   url(r'^json/(?P<config_type>.*)/name/(?P<config_name>.*)$', ConfigEntry(permitted_methods=['GET'])),
   #url(r'^json/merged/(?P<client_name>.*)$', MergedEntry()),
)

