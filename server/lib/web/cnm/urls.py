from django.conf.urls.defaults import url, patterns, include, handler500
#from django.conf.urls.defaults import *
from django.contrib import admin

urlpatterns = patterns('',
   url(r'', include('config_urls')),
   url(r'', include('auth_urls')),
   url(r'', include('package_urls')),
   url(r'^admin/(.*)', admin.site.root)
)
