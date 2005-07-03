import webUtil, os, random, string, shutil, yaml, time

from static import *

"""This thing allows clients to add to their logging entries. This
provides a timeline function for the administrator to know the history
of a given machine."""

def doc():
    return """
get:
    [client] <section> : Returns all log entries for the given
             client name and optionally filtering for section.

put: 
    [client] [severity] <section>: adds an entry to the log for this client,
             indicating severity and message details. Section is optional and only 

    """

def post(request, logger, errlog):
    status, config = webUtil.processOptions(request, errlog,
                                            ["client", "severity", "message"],
                                            ["section"])
    if status == FAIL:
        errmsg = "Specify the client name in the query string, the severity and the message\n"
        return webUtil.err400(request, errlog, "Error in variables", errmsg)
    clientPath = os.path.join(webUtil.getClientPath(), config["client"])
    statusPath = os.path.join(clientPath, STATUS_FILE)
    lastPath   = os.path.join(clientPath, LAST_STATUS)
    if not os.path.isdir(clientPath):
        os.makedirs(clientPath)
    if not os.path.isfile(statusPath):
        fh = open(statusPath, 'w')
    else:
        fh = open(statusPath, 'a')
    fh2 = open(lastPath, 'w')
    config["time"]= time.ctime()
    logEntry = yaml.dump(config)
    fh.write(logEntry)
    fh2.write(logEntry)
    fh.close()
    fh2.close()
    return "OK\n"

def writeLast(config):
    repeat    = False
    lastPath  = os.path.join(clientPath, LAST_STATUS)
    fh        = open(lastPath, 'r')
    if 1 == 1:
    #try:
        lastData  = yaml.load(fh).next()
    else:
    #except:
        lastData = {"severity": "none", "section":"none", "message":"none"}
    fh.close()
    if lastData["severity"] == config["severity"]:
        if lastData["section"] == config["section"]:
            if lastData["message"] == lastData["message"]:
                repeat = True
    fh        = open(lastPath, 'w')
    logEntry  = yaml.dump(config)
    fh.write(logEntry)
    fh.close()
    return repeat

def writeStatus(config):
    config["time"]= time.ctime()
    clientPath = os.path.join(webUtil.getClientPath(), config["client"])
    statusPath = os.path.join(clientPath, STATUS_FILE)
    if not os.path.isdir(clientPath):
        logger.info("Making directory %s" % clientPath)
        os.makedirs(clientPath)
    if not os.path.isfile(statusPath):
        fh = open(statusPath, 'w')
    else:
        fh = open(statusPath, 'a')
    repeat = writeLast(config)
    if not repeat:
        fh.write(logEntry)
    return "OK\n"

def put(request, logger, errlog):
    status, config = webUtil.processOptions(request, errlog,
                                            ["client", "severity"],
                                            ["section"])
    if status == FAIL:
        errmsg = "Specify the client name and the severity in the query string\n"
        return webUtil.err400(request, errlog, "Error in variables", errmsg)
    if not config.get("section"):
        config["section"] = GENERAL
    config["message"] = request.content.read()
    return writeStatus(config)


def get(request, logger, errlog):
    status, config = webUtil.processOptions(request, errlog, ["client"],
                                            ["section", "__doc__"])
    if status == FAIL:
        return webUtil.err500(request, errlog)
    if config.get("__doc__"):
        return doc()
    output = []
    statusPath = os.path.join(webUtil.getClientPath(), config["client"], STATUS_FILE)
    viewFilter = []
    if os.path.isfile(statusPath):
        for entry in yaml.loadFile(statusPath):
            if config["section"] and entry["section"].upper() != config["section"].upper():
                continue
            output.append(yaml.dump(entry))
        return string.join(output, '\n')
    else:
        print "NO ENTRIES", statusPath
        return ""

