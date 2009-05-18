from django.conf.urls.defaults import *
from django_restapi.responder import *
from django_restapi.resource import Resource
from django.contrib.auth.views import login, logout

class AuthEntry(Resource):
    def read(self, request):
        if not request.user.is_authenticated():
            status = "NOT_AUTHENTICATED"
            status_code = 403
        else:
            status = "OK"
            status_code = 200
        responder = JsonDictResponder({"status": status})
        response = responder.element(request)
        response.status_code = status_code
        return response

class UserProfileEntry(Resource):
    def read(self, request):
        output = {"username": request.user.username,
                  "super_user": request.user.is_superuser}

        responder = JsonDictResponder(output)
        return responder.element(request)

urlpatterns = patterns('',
   url(r'^json/check_authentication', AuthEntry(permitted_methods=['GET'])),
   url(r'^accounts/login/$',  login),
   url(r'^accounts/logout/$', logout),
   url(r'^accounts/profile/$', UserProfileEntry()),
)

