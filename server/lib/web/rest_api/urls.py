from django.conf.urls.defaults import url, patterns, include, handler500, handler404
#from django.conf.urls.defaults import *
from django.contrib import admin

urlpatterns = patterns('',
   url(r'', include('bombardier_server.web.rest_api.config_urls')),
   url(r'', include('bombardier_server.web.rest_api.auth_urls')),
   url(r'', include('bombardier_server.web.rest_api.dispatcher_urls')),
   url(r'^admin/(.*)', admin.site.root)
)
