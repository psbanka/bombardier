import webUtil, os, random, string, shutil, yaml, time

from static import *

"""This thing allows clients to add to their logging entries."""

def doc():
    return """
get:
    [client] <section> <viewed> <acknowledged>: Returns all log entries for the given
             client name and optionally filtering for section, viewed status or
             acknowledge status

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
    clientPath = os.path.join(ROOT_DIR, "client", config["client"])
    statusPath = os.path.join(clientPath, STATUS_FILE)
    lastPath   = os.path.join(clientPath, LAST_STATUS)
    if not os.path.isdir(clientPath):
        os.makedirs(clientPath)
    if not os.path.isfile(statusPath):
        fh = open(statusPath, 'w')
    else:
        fh = open(statusPath, 'a')
    fh2 = open(lastPath, 'w')
    config["acknowledged"] = "False"
    config["viewed"] = "False"
    config["time"]= time.ctime()
    logEntry = yaml.dump(config)
    fh.write(logEntry)
    fh2.write(logEntry)
    fh.close()
    fh2.close()
    return "OK\n"

def put(request, logger, errlog):
    status, config = webUtil.processOptions(request, errlog,
                                            ["client", "severity"],
                                            ["section"])
    if status == FAIL:
        errmsg = "Specify the client name and the severity in the query string\n"
        return webUtil.err400(request, errlog, "Error in variables", errmsg)
    clientPath = os.path.join(ROOT_DIR, "client", config["client"])
    statusPath = os.path.join(clientPath, STATUS_FILE)
    lastPath   = os.path.join(clientPath, LAST_STATUS)
    if not os.path.isdir(clientPath):
        logger.info("Making directory %s" % clientPath)
        os.makedirs(clientPath)
    if not os.path.isfile(statusPath):
        fh = open(statusPath, 'w')
    else:
        fh = open(statusPath, 'a')
    fh2 = open(lastPath, 'w')
    config["acknowledged"] = "False"
    config["viewed"] = "False"
    config["time"]= time.ctime()
    config["message"] = request.content.read()
    logEntry = yaml.dump(config)
    fh.write(logEntry)
    fh2.write(logEntry)
    fh.close()
    fh2.close()
    return "OK\n"


def get(request, logger, errlog):
    status, config = webUtil.processOptions(request, errlog, ["client"],
                                            ["section", "viewed", "acknowledged", "__doc__"])
    if status == FAIL:
        return webUtil.err500(request, errlog)
    if config.get("__doc__"):
        return doc()
    output = []
    statusPath = os.path.join(ROOT_DIR, "client", config["client"], STATUS_FILE)
    viewFilter = []
    if config["viewed"]:
        if config["viewed"].lower().startswith('t'):
            viewFilter.append(VIEWED)
        else:
            viewFilter.append(NOT_VIEWED)
    if config["acknowledged"]:
        if config["acknowledged"].lower().startswith('t'):
            viewFilter.append(ACKNOWLEDGED)
        else:
            viewFilter.append(NOT_ACKNOWLEDGED)
    if os.path.isfile(statusPath):
        for entry in yaml.loadFile(statusPath):
            if entry["acknowledged"] == "True" and NOT_ACKNOWLEDGED in viewFilter:
                continue
            elif entry["acknowledged"] == "False" and ACKNOWLEDGED in viewFilter:
                continue
            if entry["viewed"] == "True" and NOT_VIEWED in viewFilter:
                continue
            if entry["viewed"] == "False" and VIEWED in viewFilter:
                continue
            if config["section"] and entry["section"].upper() != config["section"].upper():
                continue
            output.append(yaml.dump(entry))
        return string.join(output, '\n')
    else:
        print "NO ENTRIES", statusPath
        return ""

