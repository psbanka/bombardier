import webUtil, os
from static import *

CACA_HOME ="."
BASE      = "/website/service/caca/"
DATA      = "/var/lib/caca/"
from WebSvc import CACA

caca = CACA(CACA_HOME, DATA, BASE)
def do(request):
    os.environ["REQUEST_URI"]    = "/"+request.path
    return caca.handle_request()

def get(request, logger, errlog):
    os.environ["REQUEST_METHOD"] = "GET"
    return do(request)

def put(request, logger, errlog):
    caca.infile = request.content
    os.environ["REQUEST_METHOD"] = "PUT"
    return do(request)
