import webUtil, os, ConfigParser, random, string, StringIO
import yaml
from static import *

"""This thing allows queries and posts to client configuration
files. It should also allow puts, but doesn't yet. These routines
don't require authentication, and should. All posts are basically
added to the configuration for the client, where PUTs are meant to
overwrite."""

def post(request):
    clientName = request.args.get("client")
    if not clientName:
        return webUtil.err400(request, "No client specified",
                              "Please specify a client name in the query string")
    clientName = clientName[0]
    config = ConfigParser.ConfigParser()
    configPath = os.path.join("deploy", "config", clientName.upper()+".ini")
    if os.path.isfile(configPath):
        config.read(configPath)

    configData = request.args.get("config")
    if not configData:
        return "OK"
    configData = configData[0]
    scratchFile = "%s-tmp-%s.ini" % (clientName, random.randint(1,1000))
    scratchPath = os.path.join("clientconfig", scratchFile)
    open(scratchPath, 'w').write(configData)
    newConfig = ConfigParser.ConfigParser()
    newConfig.read(scratchPath)
    os.unlink(scratchPath)

    for section in newConfig.sections():
        if not config.has_section(section):
            config.add_section(section)
        for option in newConfig.options(section):
            config.set(section, option, newConfig.get(section, option))
    cf = open(configPath, 'w')
    config.write(cf)
    return "--- OK\n"

def getSystemNames():
    baseDir = os.path.join('deploy','config')
    inodes = os.listdir(baseDir)
    systemNames = ['']
    for inode in inodes:
        if os.path.isfile(os.path.join(baseDir,inode)) and inode.endswith('.ini'):
            systemName = inode[:inode.rfind('.ini')]
            if systemName != '':
                systemNames.append(systemName)
    systemNames.sort()
    return systemNames

def get(request):
    clientName = request.args.get("client")
    if not clientName:
        return string.join(getSystemNames(), '\n')
    clientName = clientName[0]
    if clientName.rfind('..') != -1:
        return webUtil.err500(request)
    configPath = os.path.join("deploy", "config", clientName+".ini")
    if os.path.isfile(configPath):
        if request.args.get("type"):
            return open(configPath, 'r').read()
        else: # convert yaml to ini
            data = yaml.loadFile(configPath)
            try:
                config = webUtil.makeConfigObject(data.next())
            except:
                print "BAD YAML", data
            output = StringIO.StringIO()
            config.write(output)
            output.seek(0)
            return output.read()
    elif configPath.lower().endswith('ini'):
        if request.args.get("type"): # convert ini to yaml
            return webUtil.configToYaml(configPath)
        else:
            request.setHeader("Content-Type", "text/plain")
            return open(configPath, 'r').read() #FIXME: should be threaded.
    else:
        return webUtil.err404(request)

def put(request):
    clientName = request.args.get("client")
    if not clientName:
        return webUtil.err400(request, "Bad put", "Provide the client name")
    clientName = clientName[0]
    if clientName.rfind('..') != -1:
        return webUtil.err500(request)

    # need to trick ConfigParser
    configData = request.content.read()
    if request.args.get("type"):
        ymlPath = os.path.join("deploy", "config", "%s.yml" % clientName)
        webUtil.verifyAndWriteYaml(configData, ymlPath)
        return "--- OK\n"
    else:
        iniFile  = os.path.join("deploy", "config", "%s.ini" % clientName)
        webUtil.verifyAndWriteConfig(configData, iniFile)
        return "--- OK\n"

