from django.conf.urls.defaults import url, patterns, include, handler500, handler404
#from django.conf.urls.defaults import *
from django.contrib import admin

urlpatterns = patterns('',
   url(r'', include('config_urls')),
   url(r'', include('auth_urls')),
   url(r'', include('dispatcher_urls')),
   url(r'^admin/(.*)', admin.site.root)
)
