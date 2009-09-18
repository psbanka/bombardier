"Config urls module"
from django.conf.urls.defaults import patterns, url
from django_restapi.responder import JsonDictResponder, JSONResponder
from django_restapi.responder import YamlFileResponder

from django import forms

from django.shortcuts import render_to_response
from CnmResource import CnmResource
from configs.models import Machine, Include, Bom, ServerConfig, Package, Status
from configs.models import Dist, MachineModelFactory, IncludeModelFactory
from configs.models import BomModelFactory, PackageModelFactory
from configs.models import DistModelFactory, StatusModelFactory
from bombardier_core.static_data import OK, FAIL
import os
import MachineConfig
from django.contrib.auth.decorators import login_required
from django.http import HttpResponseForbidden

MAPPER = {"merged": Machine, "machine": Machine, "include": Include,
          "bom": Bom, "package": Package, "status": Status }
MAPPER["dist"] = Dist

FACTORIES = [MachineModelFactory, IncludeModelFactory, BomModelFactory,
             PackageModelFactory, DistModelFactory, StatusModelFactory]

class ConfigEntry(CnmResource):
    "ConfigEntry base class"
    @login_required
    def read(self, request, config_type, config_name):
        "Default read method for ConfigEntry"
        server_home = self.get_server_home()
        config_file = os.path.join(server_home, config_type)
        responder = YamlFileResponder(config_file)
        return responder.element(request, config_name)

    @login_required
    def update(self, request, config_type, config_name):
        "Default PUT method for ConfigEntry"
        yaml_string = request.PUT["yaml"]
        server_home = self.get_server_home()
        file_path = os.path.join(server_home, config_type,
                                 "%s.yml" % config_name)
        output = {"status": OK}
        if os.path.isfile(file_path):
            output["message"] = "update"
        else:
            output["message"] = "create" 
        try:
            open(file_path, 'w').write(yaml_string)
        except IOError, ioe:
            output["status"] = FAIL
            output["message"] = "IO Error: %s" % str(ioe)
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineEntry(ConfigEntry):
    "Machine config entry"
    @login_required
    def read(self, request, machine_name):
        "Call superclass read method with config type"
        return super(MachineEntry, self).read(request, "machine", machine_name)

    @login_required
    def update(self, request, machine_name):
        "Call superclass read method with config type"
        return super(MachineEntry, self).update(request,
                                                "machine", machine_name)

class PackageEntry(ConfigEntry):
    "Package config entry"
    @login_required
    def read(self, request, package_name):
        "Call superclass read method with config type"
        return super(PackageEntry, self).read(request, "package", package_name)

    @login_required
    def update(self, request, package_name):
        "Call superclass read method with config type"
        return super(PackageEntry, self).update(request,
                                                "package", package_name)

class BomEntry(ConfigEntry):
    "Bom config entry"
    @login_required
    def read(self, request, bom_name):
        "Call superclass read method with config type"
        return super(BomEntry, self).read(request, "bom", bom_name)

    @login_required
    def update(self, request, bom_name):
        "Call superclass read method with config type"
        return super(BomEntry, self).update(request, "bom", bom_name)

class IncludeEntry(ConfigEntry):
    "Include config entry"
    @login_required
    def read(self, request, include_name):
        "Call superclass read method with config type"
        return super(IncludeEntry, self).read(request, "include", include_name)

    @login_required
    def update(self, request, include_name):
        "Call superclass read method with config type"
        return super(IncludeEntry, self).update(request,
                                                "include", include_name)

class StatusEntry(ConfigEntry):
    "Status config entry"
    @login_required
    def read(self, request, machine_name):
        "Call superclass read method with config type"
        return super(StatusEntry, self).read(request, "status", machine_name)

class DistEntry(ConfigEntry):
    "Config entry for dist type (used for python distutils packages)."
    @login_required
    def read(self, request, dist_name):
        "Create dictionary from Dist object and return as json"
        dist_object = Dist.objects.get(name__startswith=dist_name)
        output = {"name": dist_object.name,
                  "version": dist_object.version,
                  "description": dist_object.desc,
                  "dist_name": dist_object.dist_name
                 }
        responder = JsonDictResponder(output)
        return responder.element(request)

class MergedEntry(CnmResource):
    """Merged Entry is a merged configuration based on machine and including
       based on the client configuration"""
    @login_required
    def read(self, request, machine_name):
        "Create machine config object, merge and return data as json"
        server_home = self.get_server_home()
        machine_config = MachineConfig.MachineConfig(machine_name, 
                                                     "", server_home)
        machine_config.merge()
        responder = JsonDictResponder(machine_config.data)
        return responder.element(request)

#==================================

class ConfigCollection(CnmResource):
    "Base config collection"
    @login_required
    def read(self, request, config_type, config_name):
        "Default read method for ConfigCollection"
        mapped = MAPPER[config_type.lower()]
        objects = mapped.objects.filter(name__startswith=config_name)
        responder = JSONResponder()
        responder.expose_fields = ["name"]
        return responder.list(request, objects)

class StatusCollection(ConfigCollection):
    "Status config collection"
    @login_required
    def read(self, request, machine_name):
        "Call superclass read method with collection type"
        sup = super(StatusCollection, self)
        return sup.read(request, "status", machine_name)

class MergedCollection(ConfigCollection):
    "Merged config collection"
    @login_required
    def read(self, request, machine_name):
        "Call superclass read method with collection type"
        sup = super(MergedCollection, self)
        return sup.read(request, "merged", machine_name)

class MachineCollection(ConfigCollection):
    "Machine config collection"
    @login_required
    def read(self, request, machine_name):
        "Call superclass read method with collection type"
        sup = super(MachineCollection, self)
        return sup.read(request, "machine", machine_name)

class IncludeCollection(ConfigCollection):
    "Include config collection"
    @login_required
    def read(self, request, include_name):
        "Call superclass read method with collection type"
        sup = super(IncludeCollection, self)
        return sup.read(request, "include", include_name)

class BomCollection(ConfigCollection):
    "Bom config collection"
    @login_required
    def read(self, request, bom_name):
        "Call superclass read method with collection type"
        sup = super(BomCollection, self)
        return sup.read(request, "bom", bom_name)

class PackageCollection(ConfigCollection):
    "Package config collection"
    @login_required
    def read(self, request, package_name):
        "Call superclass read method with collection type"
        sup = super(PackageCollection, self)
        return sup.read(request, "package", package_name)

class DistCollection(ConfigCollection):
    "Dist config collection"
    @login_required
    def read(self, request, dist_name):
        "Expose custom fields for Dist type collection and return as json"
        objects = Dist.objects.filter(name__startswith=dist_name)
        responder = JSONResponder()
        responder.expose_fields = ["name", "version", "desc", "dist_name"]
        return responder.list(request, objects)

#==================================

class ServerConfigCollection(CnmResource):
    "Server config collection"
    @login_required
    def read(self, request):
        "Return json representation of server configuration"
        output = {}
        server_config = {}

        server_config_objects = ServerConfig.objects.all()
        for server_co in server_config_objects:
            server_config[server_co.name] = server_co.value
        output["server configuration"] = server_config
        object_config = {}
        for factory_name in FACTORIES:
            factory = factory_name()
            object_config[factory.subdir] = factory.summarize()

        output["object lists"] = object_config

        responder = JsonDictResponder(output)
        return responder.element(request)

    @login_required
    def create(self, request):
        "Create or update configuration items on the server config collection."
        if not request.user.is_superuser:
            return HttpResponseForbidden()
        query_dict = request.POST

        if "form-0-name" in query_dict:
            self._form_update(query_dict)
        else:
            self._standard_update(query_dict)

        server_config_objects = ServerConfig.objects.all()
        output = {}
        for server_co in server_config_objects:
            output[server_co.name] = server_co.value
        responder = JsonDictResponder(output)
        return responder.element(request)

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

    def _standard_update(self, query_dict):
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


class DbSyncCollection(CnmResource):
    "Class for populating the database from yaml files present in server home."
    @login_required
    def create(self, request):
        "Sync db from server home"
        self._server_home_sync()
        config_data = {"status": "OK"}
        responder = JsonDictResponder(config_data)
        return responder.element(request)

    def _server_home_sync(self):
        "Use each Factory to sync itself from its corresponding yaml file"
        for factory_constructor in FACTORIES:
            factory = factory_constructor()
            factory.clean()
            factory.create()

def config_setting_form(request):
    "Form for setting configuration items. Mostly a debugging tool."
    from django.forms.models import modelformset_factory
    ServerConfigFormSet = modelformset_factory(ServerConfig)
    server_config_objects = ServerConfig.objects.all()
    formset = ServerConfigFormSet(queryset=server_config_objects)

    return render_to_response('config_setting_form.html', {
        'formset': formset,
    })

class ServerConfigForm(forms.ModelForm):
    "Server config form from ServerConfig model"
    class Meta:
        "Meta class for use by django to create form"
        model = ServerConfig


urlpatterns = patterns('',
   url(r'^json/server/config',
       ServerConfigCollection(permitted_methods = ['POST', "GET"])),
   url(r'^json/dbsync',
       DbSyncCollection(permitted_methods = ['POST'])),
   url(r'^json/merged/search/(?P<machine_name>.*)', MergedCollection()),
   url(r'^json/merged/name/(?P<machine_name>.*)$',
       MergedEntry(permitted_methods=['GET'])),
   url(r'^json/machine/search/(?P<machine_name>.*)',
       MachineCollection()),
   url(r'^json/machine/name/(?P<machine_name>.*)$',
       MachineEntry(permitted_methods=['GET', 'PUT'])),
   url(r'^json/include/search/(?P<include_name>.*)', IncludeCollection()),
   url(r'^json/include/name/(?P<include_name>.*)$',
       IncludeEntry(permitted_methods=['GET', 'PUT'])),
   url(r'^json/bom/search/(?P<bom_name>.*)', BomCollection()),
   url(r'^json/bom/name/(?P<bom_name>.*)$',
       BomEntry(permitted_methods=['GET', 'PUT'])),
   url(r'^json/package/search/(?P<package_name>.*)', PackageCollection()),
   url(r'^json/package/name/(?P<package_name>.*)$',
       PackageEntry(permitted_methods=['GET', 'PUT'])),
   url(r'^json/dist/search/(?P<dist_name>.*)', DistCollection()),
   url(r'^json/dist/name/(?P<dist_name>.*)', DistEntry()),
   url(r'^json/status/search/(?P<machine_name>.*)', StatusCollection()),
   url(r'^json/status/name/(?P<machine_name>.*)$',
       StatusEntry(permitted_methods=['GET'])),
   url(r'^server/config', config_setting_form),
)
