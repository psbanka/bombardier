"Authentication urls module"
from django.conf.urls.defaults import patterns, url
from django.contrib.auth.decorators import login_required
from django.contrib.auth.models import User
from CnmResource import CnmResource
from django.contrib.auth.views import login, logout
from django_restapi.responder import JsonDictResponder
from django_restapi.responder import JSONResponder

from bombardier_core.static_data import OK, FAIL
from django.http import Http404
import yaml

class AuthEntry(CnmResource):
    "Authentication entry"
    def read(self, request):
        "Reject read if user is not authenticated"
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

class UserProfileEntry(CnmResource):
    "User profile entry"
    def read(self, request):
        "Return user name and superuser status as json"
        output = {"username": request.user.username,
                  "super_user": request.user.is_superuser}

        responder = JsonDictResponder(output)
        return responder.element(request)

class UsersCollection(CnmResource):
    "Users collection"
    @login_required
    def read(self, request, username):
        "Return list of possible users as json"
        users = User.objects.filter(username__startswith=username)
        responder = JSONResponder()
        responder.expose_fields = ["username"]
        return responder.list(request, users)

class UsersEntry(CnmResource):
    "Users entry"
    @login_required
    def read(self, request, username):
        "Return user info as json if user exists"
        try:
            user = User.objects.get(username__exact=username)
        except User.DoesNotExist:
            raise Http404
        output = {"username": user.username,
                  "is_superuser": user.is_superuser,
                  "is_active": user.is_active,
                  "first_name": user.first_name,
                  "last_name": user.last_name,
                  "email": user.email,
                 }
        responder = JsonDictResponder(output)
        response = responder.element(request)
        response.status_code = 200
        return response

    @login_required
    def delete(self, request, username):
        "Choose to delete a user"

        command_output = []
        user = User.objects.get(username__exact=username)
        user.delete()
        output = "%s deleted" % username
        responder = JsonDictResponder({"command_status": OK,
                                       "command_output": output})
        response = responder.element(request)
        response.status_code = 200
        return response

    @login_required
    def create(self, request, username):
        "Create or update a user"

        query_dict = {}
        command_output = []
        if "yaml" in request.POST:
            yaml_string = request.POST["yaml"]
            try:
                query_dict = yaml.load(yaml_string)
            except Exception, x:
                output = {"command_status": FAIL, "command_output": "Bad YAML"}
                responder = JsonDictResponder(output)
                return responder.element(request)
        else:
            query_dict = request.POST

        try:
            user = User.objects.get(username__exact=username)
        except User.DoesNotExist:
            user = User.objects.create_user(username,
                                            query_dict.get("email", ''))
            command_output.append("User %s created" % username)

        password = query_dict.get("password")
        if password:
            user.set_password(password)
            command_output.append("Password set for user %s" % username)

        if query_dict.has_key('admin'):
            user.is_superuser = query_dict.get('admin')
            command_output.append("User %s made an administrator" % username)
        if query_dict.has_key('active'):
            user.is_active = query_dict.get('active')
            command_output.append("User %s made active" % username)

        first_name = query_dict.get("first_name")
        if first_name:
            user.first_name = first_name
            command_output.append("First name set for user %s" % username)

        last_name = query_dict.get("last_name")
        if last_name:
            user.last_name = last_name
            command_output.append("Last name set for user %s" % username)

        email = query_dict.get("email")
        if email:
            user.email = email
            command_output.append("Email set for user %s" % username)

        user.save()
        output_string = '|'.join(command_output)
        responder = JsonDictResponder({"command_status": OK,
                                       "command_output": output_string})
        response = responder.element(request)
        response.status_code = 200
        return response

urlpatterns = patterns('',
   url(r'^json/check_authentication', AuthEntry(permitted_methods=['GET'])),
   url(r'^json/user/search/(?P<username>.*)', UsersCollection()),
   url(r'^json/user/name/(?P<username>.*)',
       UsersEntry(permitted_methods=['POST', 'GET', 'DELETE'])),
   url(r'^accounts/login/$',  login),
   url(r'^accounts/logout/$', logout),
   url(r'^accounts/profile/$', UserProfileEntry()),
)

