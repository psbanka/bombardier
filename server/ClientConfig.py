import cherrypy
import yaml, os, random, StringIO
import Root
from static import *
import webUtil

"""This thing allows queries and posts to client configuration
files. It should also allow puts, but doesn't yet. These routines
don't require authentication, and should. All posts are basically
added to the configuration for the client, where PUTs are meant to
overwrite."""

class ClientConfig(Root.Root):

    known_methods = ["GET", "POST", "PUT"]

    def GET(self, client=None, type="yml"):
        cherrypy.response.headerMap["Content-type"] = "text/plain"
        if not client:
            return '\n'.join(webUtil.getClientNames())
        if client.rfind('..') != -1:
            cherrypy.response.status = 500
            return "Forbidden\n"
        configData = webUtil.readClientData(client)
        configData = webUtil.nicifyForLegacyClients(configData)
        if configData == {}:
            errmsg = "Bad configuration request: %s\n" % client
            cherrypy.log(errmsg)
            cherrypy.response.status = 404
            return errmsg
        if type.lower() == "ini":
            config = webUtil.makeConfigObject(configData)
            output = StringIO.StringIO()
            config.write(output)
            output.seek(0)
            return output.read()
        else:
            return yaml.dump(configData)

    def POST(self, client=None, config=None):
        cherrypy.response.headerMap["Content-type"] = "text/plain"
        if not client:
            cherrypy.response.status = 400
            return "No client specified "\
                   "Please specify a client name in the query string"
        configObj = ConfigParser.ConfigParser()
        configPath = webUtil.getConfigFile(client.upper()+".ini")
        if os.path.isfile(configPath):
            configObj.read(configPath)

        if not config:
            return "OK"
        scratchFile = "%s-tmp-%s.ini" % (client, random.randint(1,1000))
        scratchPath = os.path.join("clientconfig", scratchFile)
        open(scratchPath, 'w').write(config)
        newConfig = ConfigParser.ConfigParser()
        newConfig.read(scratchPath)
        os.unlink(scratchPath)

        for section in newConfig.sections():
            if not configObj.has_section(section):
                configObj.add_section(section)
            for option in newConfig.options(section):
                configObj.set(section, option, newConfig.get(section, option))
        cf = open(configPath, 'w')
        configObj.write(cf)
        return "--- OK\n"

    def PUT(self, client, type="ini"):
        if client.rfind('..') != -1:
            cherrypy.response.status = 500
            return "Invalid client name"

        # need to trick ConfigParser
        configData = cherrypy.request.body.read()
        if type != "yaml":
            ymlPath = webUtil.getConfigFile("%s.yml" % client)
            webUtil.verifyAndWriteYaml(configData, ymlPath)
            return "--- OK\n"
        else:
            iniFile  = webUtil.getConfigFile("%s.ini" % client)
            webUtil.verifyAndWriteConfig(configData, iniFile)
            return "--- OK\n"

