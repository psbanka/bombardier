import os
from bombardier_core.static_data import SERVER_CONFIG_FILE
SERVER_CONFIG = {}
if os.path.isfile(SERVER_CONFIG_FILE):
    import yaml
    SERVER_CONFIG = yaml.load(open(SERVER_CONFIG_FILE).read())

DEBUG = SERVER_CONFIG.get("debug", True)
TEMPLATE_DEBUG = SERVER_CONFIG.get("debug", True)

SERVER_HOME = SERVER_CONFIG.get("server_home", "/var/deploy")
SERVER_ROOT = SERVER_CONFIG.get("server_root", '')
LOGIN_URL="%s/accounts/login" % SERVER_ROOT
LOGIN_REDIRECT_URL="%s/accounts/profile" % SERVER_ROOT

INSTALLED_APPS = (
    'django.contrib.contenttypes',
    'django.contrib.admin',
    'django.contrib.auth',
    'django.contrib.sessions',
    'django.contrib.sites',
    'bombardier_server.web.rest_api.configs',
)
SITE_ID=1
ROOT_URLCONF = 'bombardier_server.web.rest_api.urls'

DATABASE_NAME = os.path.join(SERVER_HOME, 'admin', 'cnmdb')
DATABASE_ENGINE = 'sqlite3'
TEMPLATE_DIRS = ["/usr/local/share/bdr-templates"]
TEST_DATABASE_NAME = 'testdb'
DATABASE_SUPPORTS_TRANSACTIONS = False
DATABASE_OPTIONS = {'timeout': 30}

MIDDLEWARE_CLASSES = (
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
#    'django.middleware.common.CommonMiddleware',
#    'django.middleware.doc.XViewMiddleware',
)
