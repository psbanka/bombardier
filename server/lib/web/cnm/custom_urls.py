from django.conf.urls.defaults import *
from django_restapi.model_resource import Collection, Entry, reverse
from django_restapi.responder import *
from django_restapi.resource import Resource
from clients.models import Client

# JSON Test API URLs
#
# Clients are available at /json/clients/ and
# /json/clients/[client_id]/.
#
class ClientCollection(Resource):
    def read(self, request, client_name):
        clients = Client.objects.filter(name__startswith=client_name)
        responder = JSONResponder()
        responder.expose_fields = ["name"]
        return responder.list(request, clients)

class ClientEntry(Resource):

    def read(self, request, client_name):
        responder = YamlResponder("/cygdrive/c/deploy/client")
        return responder.element(request, client_name)

#    def delete(self, request, person_id, friend_id):
#        friendship = get_friendship(person_id, friend_id)
#        friendship[0].friends.remove(friendship[1])
#        return HttpResponseRedirect('/friends/')

urlpatterns = patterns('',
   url(r'^json/client/search/(?P<client_name>.*)', ClientCollection()),
   url(r'^json/client/name/(?P<client_name>.*)$', ClientEntry(permitted_methods=['GET'])),
)

