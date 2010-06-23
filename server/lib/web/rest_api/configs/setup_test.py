import bombardier_server.web.cnm.ServerConfig as ServerConfig
import django.utils.simplejson as json
import os


def create_server_dir(s):
    print "creating"
    os.system("mkdir -p %s" %s.server_home)
    for dir in ["include", "client", "bom"]:
        os.system("mkdir %s/%s" % ( s.server_home, dir ))

def clean_server_dir(s):
    os.system("mkdir -p %s" %s.server_home)

def setup(self):
    new_initial = [{"pk": 1, "model": "configs.include", "fields": {"name": "otherapp"}}, {"pk": 2, "model": "configs.include", "fields": {"name": "app1"}}, {"pk": 1, "model": "configs.client", "fields": {"name": "tester1"}}, {"pk": 2, "model": "configs.client", "fields": {"name": "other1"}}, {"pk": 3, "model": "configs.client", "fields": {"name": "tester2"}}, {"pk": 1, "model": "configs.bom", "fields": {"name": "foo"}}, {"pk": 2, "model": "configs.bom", "fields": {"name": "bomp"}}]
    s = ServerConfig.ServerConfig()
    create_server_dir(s)
    home = s.server_home
    for i in new_initial:
        config_file = "%s/%s/%s.yml" % ( s.server_home, i['model'].split('.')[1], i['fields']['name'] )
        print config_file
        open( config_file, 'w').write( json.dumps(i["fields"]) )

setup(1)

