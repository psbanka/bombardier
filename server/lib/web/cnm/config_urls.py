from django.conf.urls.defaults import patterns, url
from django_restapi.responder import JsonDictResponder, JSONResponder, YamlFileResponder

from django import forms

from django.shortcuts import render_to_response
from CnmResource import CnmResource
from configs.models import Machine, Include, Bom, ServerConfig, Package
from configs.models import Dist
from configs.models import MachineModelFactory, IncludeModelFactory, BomModelFactory, PackageModelFactory
from configs.models import DistModelFactory
import syck, glob
import os
import MachineConfig
from django.contrib.auth.decorators import login_required
from django.http import HttpResponseForbidden

MAPPER = {"merged": Machine, "machine": Machine, "include": Include,
          "bom": Bom, "package": Package }
MAPPER["dist"] = Dist

FACTORIES = [MachineModelFactory, IncludeModelFactory, BomModelFactory,
             PackageModelFactory, DistModelFactory]

class ConfigEntry(CnmResource):
    @login_required
    def read(self, request, config_type, config_name):
        server_home = self.get_server_home()
        config_file = os.path.join(server_home, config_type)
        responder = YamlFileResponder(config_file)
        return responder.element(request, config_name)

class MachineEntry(ConfigEntry):
    @login_required
    def read(self, request, machine_name):
        return super(MachineEntry, self).read(request, "machine", machine_name)

class PackageEntry(ConfigEntry):
    @login_required
    def read(self, request, package_name):
        return super(PackageEntry, self).read(request, "package", package_name)

class BomEntry(ConfigEntry):
    @login_required
    def read(self, request, bom_name):
        return super(BomEntry, self).read(request, "bom", bom_name)

class IncludeEntry(ConfigEntry):
    @login_required
    def read(self, request, include_name):
        return super(IncludeEntry, self).read(request, "include", include_name)

class DistEntry(ConfigEntry):
    @login_required
    def read(self, request, dist_name):
        object = Dist.objects.get(name__startswith=dist_name)
        output = {"name": object.name,
                  "version": object.version,
                  "description": object.desc,
                  "dist_name": object.dist_name
                 }
        responder = JsonDictResponder(output)
        return responder.element(request)

class MergedEntry(CnmResource):
    @login_required
    def read(self, request, machine_name):
        server_home = self.get_server_home()
        machine_config = MachineConfig.MachineConfig(machine_name, "", server_home)
        machine_config.merge()
        responder = JsonDictResponder(machine_config.data)
        return responder.element(request)

#==================================

class ConfigCollection(CnmResource):
    @login_required
    def read(self, request, config_type, config_name):
        objects = MAPPER[config_type.lower()].objects.filter(name__startswith=config_name)
        responder = JSONResponder()
        responder.expose_fields = ["name"]
        return responder.list(request, objects)

class MergedCollection(ConfigCollection):
    @login_required
    def read(self, request, machine_name):
        return super(MachineCollection, self).read(request, "merged", machine_name)

class MachineCollection(ConfigCollection):
    @login_required
    def read(self, request, machine_name):
        return super(MachineCollection, self).read(request, "machine", machine_name)

class IncludeCollection(ConfigCollection):
    @login_required
    def read(self, request, include_name):
        return super(IncludeCollection, self).read(request, "include", include_name)

class BomCollection(ConfigCollection):
    @login_required
    def read(self, request, bom_name):
        return super(BomCollection, self).read(request, "bom", bom_name)

class PackageCollection(ConfigCollection):
    @login_required
    def read(self, request, package_name):
        return super(PackageCollection, self).read(request, "package", package_name)

class DistCollection(ConfigCollection):
    @login_required
    def read(self, request, dist_name):
        objects = Dist.objects.filter(name__startswith=dist_name)
        responder = JSONResponder()
        responder.expose_fields = ["name", "version", "desc", "dist_name"]
        return responder.list(request, objects)

#==================================

class ServerConfigCollection(CnmResource):
    @login_required
    def read(self, request):
        output = {}
        server_config = {}

        server_config_objects = ServerConfig.objects.all()
        for server_co in server_config_objects:
            server_config[server_co.name] = server_co.value
        output["server configuration"] = server_config
        object_config = {}
        for Factory in FACTORIES:
            factory = Factory()
            object_config[factory.subdir] = factory.summarize()

        output["object lists"] = object_config

        responder = JsonDictResponder(output)
        return responder.element(request)

    @login_required
    def create(self, request):
        if not request.user.is_superuser:
            return HttpResponseForbidden()
        query_dict = request.POST

        if "form-0-name" in query_dict:
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

        else:
            for name in query_dict:
                value = query_dict[name]
                try:
                    server_co = ServerConfig.objects.get(name=name)
                except ServerConfig.DoesNotExist:
                    server_co = ServerConfig.objects.create()
                    server_co.name = name
                server_co.value = value
                server_co.save()

        server_config_objects = ServerConfig.objects.all()
        output = {}
        for server_co in server_config_objects:
            output[server_co.name] = server_co.value
        responder = JsonDictResponder(output)
        return responder.element(request)

class DbSyncCollection(CnmResource):
    @login_required
    def create(self, request):
        self._server_home_sync()
        config_data = {"status": "OK"}
        responder = JsonDictResponder(config_data)
        return responder.element(request)

    def _server_home_sync(self):
        server_home = self.get_server_home()
        config_data = {}
        for Factory in FACTORIES:
            factory = Factory()
            factory.clean()
            factory.create()

def config_setting_form(request):
    from django.forms.models import modelformset_factory
    ServerConfigFormSet = modelformset_factory(ServerConfig)
    server_config_objects = ServerConfig.objects.all()
    formset = ServerConfigFormSet(queryset=server_config_objects)

    return render_to_response('config_setting_form.html', {
        'formset': formset,
    })

class ServerConfigForm(forms.ModelForm):
    class Meta:
        model = ServerConfig


urlpatterns = patterns('',
   url(r'^json/server/config', ServerConfigCollection(permitted_methods = ['POST', "GET"])),
   url(r'^json/dbsync', DbSyncCollection(permitted_methods = ['POST'])),
   url(r'^json/merged/search/(?P<machine_name>.*)', MergedCollection()),
   url(r'^json/merged/name/(?P<machine_name>.*)$', MergedEntry(permitted_methods=['GET'])),
   url(r'^json/machine/search/(?P<machine_name>.*)', MachineCollection()),
   url(r'^json/machine/name/(?P<machine_name>.*)$', MachineEntry(permitted_methods=['GET'])),
   url(r'^json/include/search/(?P<include_name>.*)', IncludeCollection()),
   url(r'^json/include/name/(?P<include_name>.*)$', IncludeEntry(permitted_methods=['GET'])),
   url(r'^json/bom/search/(?P<bom_name>.*)', BomCollection()),
   url(r'^json/bom/name/(?P<bom_name>.*)$', BomEntry(permitted_methods=['GET'])),
   url(r'^json/package/search/(?P<package_name>.*)', PackageCollection()),
   url(r'^json/package/name/(?P<package_name>.*)$', PackageEntry(permitted_methods=['GET'])),
   url(r'^json/dist/search/(?P<dist_name>.*)', DistCollection()),
   url(r'^json/dist/name/(?P<dist_name>.*)', DistEntry()),
   url(r'^server/config', config_setting_form),
)
