"Config urls module"
from django.conf.urls.defaults import patterns, url
from django_restapi.responder import JsonDictResponder
from django_restapi.responder import YamlFileResponder

from django import forms

from django.shortcuts import render_to_response
from CnmResource import CnmResource
from configs.models import ServerConfig
from bombardier_core.static_data import OK, FAIL
from bombardier_core.static_data import LOCAL_TYPE, BDR_CLIENT_TYPE
import os, glob
from bombardier_server.cnm.MachineStatus import MachineStatus
from django.contrib.auth.decorators import login_required
from django.http import HttpResponseForbidden
import yaml
from bombardier_server.cnm.Exceptions import MachineConfigurationException
from bombardier_server.cnm.Exceptions import MachineStatusException

bad_characters = ['/', '%', '\\', ' ', '$', '(', ')', '!', '#', '@',
                  '&', '*', '[', ';', '?', '|', '<', '>']

def sanitize(input_data):
    output = []
    for character in input_data:
        if character not in bad_characters:
            output.append(character)
    return ''.join(output)

class ConfigEntry(CnmResource):
    "ConfigEntry base class"
    @login_required
    def read(self, request, config_type, config_name):
        "Default read method for ConfigEntry"
        dispatcher = self.get_dispatcher()
        config_data = dispatcher.read_config_data(config_type, config_name)
        responder = JsonDictResponder(config_data)
        return responder.element(request)

    @login_required
    def create(self, request, config_type, config_name):
        "Default POST method for ConfigEntry"
        yaml_string = request.POST["yaml"]
        dispatcher = self.get_dispatcher()
        output = dispatcher.make_config_data(config_type, sanitize(config_name), yaml_string)
        responder = JsonDictResponder(output)
        return responder.element(request)

class MachineEntry(ConfigEntry):
    "Machine config entry"
    @login_required
    def read(self, request, machine_name):
        "Call superclass read method with config type"
        return super(MachineEntry, self).read(request, "machine", machine_name)

    @login_required
    def create(self, request, machine_name):
        "Call superclass read method with config type"
        return super(MachineEntry, self).create(request,
                                                "machine", machine_name)

class PackageEntry(ConfigEntry):
    "Package config entry"
    @login_required
    def read(self, request, package_name):
        "Call superclass read method with config type"
        return super(PackageEntry, self).read(request, "package", package_name)

    @login_required
    def create(self, request, package_name):
        "Call superclass read method with config type"
        return super(PackageEntry, self).create(request,
                                                "package", package_name)

class BomEntry(ConfigEntry):
    "Bom config entry"
    @login_required
    def read(self, request, bom_name):
        "Call superclass read method with config type"
        return super(BomEntry, self).read(request, "bom", bom_name)

    @login_required
    def create(self, request, bom_name):
        "Call superclass read method with config type"
        return super(BomEntry, self).create(request, "bom", bom_name)

class IncludeEntry(ConfigEntry):
    "Include config entry"
    @login_required
    def read(self, request, include_name):
        "Call superclass read method with config type"
        return super(IncludeEntry, self).read(request, "include", include_name)

    @login_required
    def create(self, request, include_name):
        "Call superclass read method with config type"
        return super(IncludeEntry, self).create(request,
                                                "include", include_name)
class SummaryEntry(CnmResource):
    "Config entry for digested summary information"
    @login_required
    def read(self, request, machine_name):
        "Digest useful information from the status of the server"
        output = {"command_status": OK}
        try:
            dispatcher = self.get_dispatcher()
            output = dispatcher.get_machine_status(machine_name)
        except Exception, x:
            output.update(self.dump_exception(request, x))
        responder = JsonDictResponder(output)
        return responder.element(request)

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
        dispatcher = self.get_dispatcher()
        output = dispatcher.get_dist_name_info(dist_name)
        responder = JsonDictResponder(output)
        return responder.element(request)

class MergedEntry(CnmResource):
    """Merged Entry is a merged configuration based on machine and including
       based on the client configuration"""

    @login_required
    def read(self, request, machine_name):
        "Create machine config object, merge and return data as json"
        try:
            username = request.user.username
            dispatcher = self.get_dispatcher()
            machine_config = dispatcher.get_machine_config(username, machine_name, BDR_CLIENT_TYPE)
            machine_config.merge()
            responder = JsonDictResponder(machine_config.data)
        except Exception, x:
            output = self.dump_exception(request, x)
            responder = JsonDictResponder(output)
        return responder.element(request)

#==================================

class ConfigCollection(CnmResource):
    "Base config collection"
    @login_required
    def read(self, request, config_type, config_name):
        "Default read method for ConfigCollection"
        dispatcher = self.get_dispatcher()
        output = dispatcher.get_config_info(config_type, sanitize(config_name))
        responder = JsonDictResponder(output)
        return responder.element(request)

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
        return sup.read(request, "machine", machine_name)

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
        dispatcher = self.get_dispatcher()
        output = dispatcher.get_dist_info(dist_name)
        responder = JsonDictResponder(output)
        return responder.element(request)
        #return responder.list(request, objects)

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
   url(r'^json/merged/search/(?P<machine_name>.*)', MergedCollection()),
   url(r'^json/merged/name/(?P<machine_name>.*)$',
       MergedEntry(permitted_methods=['GET'])),
   url(r'^json/machine/search/(?P<machine_name>.*)',
       MachineCollection()),
   url(r'^json/machine/name/(?P<machine_name>.*)$',
       MachineEntry(permitted_methods=['GET', 'POST'])),
   url(r'^json/include/search/(?P<include_name>.*)', IncludeCollection()),
   url(r'^json/include/name/(?P<include_name>.*)$',
       IncludeEntry(permitted_methods=['GET', 'POST'])),
   url(r'^json/bom/search/(?P<bom_name>.*)', BomCollection()),
   url(r'^json/bom/name/(?P<bom_name>.*)$',
       BomEntry(permitted_methods=['GET', 'POST'])),
   url(r'^json/package/search/(?P<package_name>.*)', PackageCollection()),
   url(r'^json/package/name/(?P<package_name>.*)$',
       PackageEntry(permitted_methods=['GET', 'POST'])),
   url(r'^json/dist/search/(?P<dist_name>.*)', DistCollection()),
   url(r'^json/dist/name/(?P<dist_name>.*)', DistEntry()),
   url(r'^json/status/search/(?P<machine_name>.*)', StatusCollection()),
   url(r'^json/status/name/(?P<machine_name>.*)$',
       StatusEntry(permitted_methods=['GET'])),
   url(r'^json/summary/search/(?P<machine_name>.*)', StatusCollection()),
   url(r'^json/summary/name/(?P<machine_name>.*)$',
       SummaryEntry(permitted_methods=['GET'])),
   url(r'^server/config', config_setting_form),
)
