import cherrypy
import yaml, os, time
import Root
import webUtil

from static import *

BLOCK_SIZE = 10000

class PutFile(Root.Root):

    known_methods = ["PUT"]

    def PUT(self, path1=None, path2=None, path3=None, filename=None):
        cherrypy.response.headerMap["Content-type"] = "text/plain"
        path = [path1, path2, path3, filename]
        manipPath = []
        for item in path:
            if item:
                manipPath.append(item)

        assert '/' not in manipPath[-1]
        for item in manipPath[:-1]:
            assert '.' not in item

        filepath = DEPLOY_DIR
        for item in manipPath:
            filepath = os.path.join(filepath, item)
        try:
            fh = open(filepath, 'wb')
            data = '0'
            while data:
                data = cherrypy.request.body.read(BLOCK_SIZE)
                fh.write(data)
            return "--- OK\n" 
        except:
            return "--- FAIL\n"

