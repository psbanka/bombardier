import os

DEBUG = True
TEMPLATE_DEBUG = True
SERVER_HOME = "/var/deploy"
LOGIN_URL="/bdr/accounts/login"
LOGIN_REDIRECT_URL="/bdr/accounts/profile"

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
TEMPLATE_DIRS = [os.path.join(SERVER_HOME, 'rest_api', 'templates')]
TEST_DATABASE_NAME = 'testdb'
DATABASE_SUPPORTS_TRANSACTIONS = False
DATABASE_OPTIONS = {'timeout': 30}

MIDDLEWARE_CLASSES = (
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
#    'django.middleware.common.CommonMiddleware',
#    'django.middleware.doc.XViewMiddleware',
)
