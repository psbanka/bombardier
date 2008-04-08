import glob
try:
    import syck as yaml
except:
    import yaml

hostsYamlFile = 'deploy/include/hosts.yml'
hosts = yaml.load(open( hostsYamlFile ).read())
hosts['hostFile']['hosts'] = {}

for clientFile in glob.glob( 'deploy/client/*.yml' ):
    y = yaml.load( open( clientFile ).read() )
    client = clientFile[clientFile.rfind('/')+1:clientFile.rfind('.')]
    ipAddress = y.get('ipAddress')
    if ipAddress:
        hosts['hostFile']['hosts'][client] = ipAddress 

open(hostsYamlFile, 'w').write( yaml.dump(hosts) )
