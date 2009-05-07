from django.conf.urls.defaults import *
from django_restapi.model_resource import Collection, Entry, reverse
from django_restapi.responder import *
from django_restapi.resource import Resource
from configs.models import Client, Include, Bom
import ServerConfig
import syck, glob

MAPPER = {"client": Client, "include": Include, "bom": Bom}

def path_to_config_file(config_type, config_name):
    config_path = os.path.join(ServerConfig.ServerConfig().server_home, config_type)
    config_file = os.path.join(config_path)
    return config_file

class ConfigEntry(Resource):
    def read(self, request, config_type, config_name):
        config_file = path_to_config_file(config_type, config_name)
        responder = YamlResponder(config_file)
        return responder.element(request, config_name)

class ConfigCollection(Resource):
    def read(self, request, config_type, config_name):
        objects = MAPPER[config_type].objects.filter(name__startswith=config_name)
        responder = JSONResponder()
        responder.expose_fields = ["name"]
        return responder.list(request, objects)

class ConfigSyncCollection(Resource):
    def read(self, request):
        new_config_objects = []
        for config_type in MAPPER:
            server_config = ServerConfig.ServerConfig()
            config_objects = MAPPER[config_type].objects.all()
            for config_object in config_objects:
                config_object.delete()
            config_wildcard = os.path.join(server_config.server_home, config_type, "*.yml")
            config_files = glob.glob(config_wildcard)
            for config_file in config_files:
                base_name = config_file.split(os.path.sep)[-1]
                config_name = base_name.split('.yml')[0]
                config_object = MAPPER[config_type](name=config_name)
                config_object.save()

            new_config_objects += MAPPER[config_type].objects.all()
        responder = JSONResponder()
        responder.expose_fields = ["name"]
        return responder.list(request, new_config_objects)

urlpatterns = patterns('',
   url(r'^json/dbsync', ConfigSyncCollection()),
   url(r'^json/(?P<config_type>.*)/search/(?P<config_name>.*)', ConfigCollection()),
   url(r'^json/(?P<config_type>.*)/name/(?P<config_name>.*)$', ConfigEntry(permitted_methods=['GET'])),
)

