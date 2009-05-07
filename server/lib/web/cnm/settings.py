from os.path import realpath

DEBUG = True
TEMPLATE_DEBUG = True

INSTALLED_APPS = (
    'django.contrib.contenttypes',
    'django.contrib.admin',
    'django.contrib.auth',
    'django.contrib.sessions',
    'django.contrib.sites',
    'configs',
)
SITE_ID=1
ROOT_URLCONF = 'urls'

DATABASE_NAME = realpath('cnmdb')
DATABASE_ENGINE = 'sqlite3'
TEMPLATE_DIRS = 'templates'

MIDDLEWARE_CLASSES = (
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
#    'django.middleware.common.CommonMiddleware',
#    'django.middleware.doc.XViewMiddleware',
)
