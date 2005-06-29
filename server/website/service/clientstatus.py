import webUtil, os, ConfigParser, string, yaml, time, webUtil

from static import *

"""This thing allows queries and posts to client configuration
files. It should also allow puts, but doesn't yet. These routines
don't require authentication, and should. All posts are basically
added to the configuration for the client, where PUTs are meant to
overwrite."""

def doc():
    return """client status web service
    
get: <client> 
    (no arguments):           Will return a list of all system names that
                              you can obtain status about
    client:                   will return the current overall system install
                              status for this client, including details of every
                              package they supposedly should have installed,
                              when they last contacted the server, etc.

put: <client> (yaml data of install progress)
    client:                   name of the client that the install progress
                              pertains to.
   
"""

def getInstalledPackages(clientName, errlog):
    inodes = os.listdir(os.path.join(ROOT_DIR, 'client'))
    filename = ''
    for inode in inodes:
        if inode.lower() == clientName.lower():
            filename1 = os.path.join(ROOT_DIR, "client", inode, "install-progress.txt")
            filename2 = os.path.join(ROOT_DIR, "client", inode, "install-progress.yml")
            break
    if not os.path.isfile(filename1) and not os.path.isfile(filename2):
        errlog.error( "ERROR: Install-progress file %s can't be read for client" % clientName )
        return []
    elif os.path.isfile(filename1):
        installedPackages = open(filename1).readlines()
    else:
        try:
            packageData = yaml.loadFile(filename2).next()
            installedPackages1 = [pn for pn in packageData if packageData[pn][0] == 'INSTALLED']
            installedPackages2 = [pn for pn in packageData if packageData[pn] == 'INSTALLED']
            installedPackages = installedPackages1 + installedPackages2
        except:
            installedPackages = []
    outputNames = []
    for packageName in installedPackages:
        outputNames.append(packageName[:packageName.rfind('-')]) # remove the version
    return outputNames
        

def installStatus(clientName, errlog):

    """ Determine the install progress of every package in the BOM for
    this client"""

    output = {}
    filename, type = webUtil.findFile(clientName)
    dataFile = open(os.path.join(filename))
    if type == INI:
        config = ConfigParser.ConfigParser()
        config.readfp(dataFile)
        dataFile.close()
        packageGroups = webUtil.getPackageGroups(config)
    else:
        data = dataFile.read()
        packageGroups = webUtil.getPackageGroups(data, type=YML)

    installedPackages = getInstalledPackages(clientName, errlog)
    for packageGroup in packageGroups:
        packageNames = []
        filename = webUtil.getConfigFile(packageGroup+".BOM")
        output[packageGroup] = {"status":"OK", # overall status of this packagegroup
                                "packages":{}, # dictionary of packages for this group
                                "installedStatus": "OK"}
        if os.path.isfile(filename):
            packageNames = open(webUtil.getConfigFile(packageGroup+".BOM")).readlines()
        else:
            output[packageGroup]["status"] = "FAIL"
            continue
        for packageName in packageNames:
            packageName = packageName.strip()
            if not packageName:
                continue
            if packageName.strip() in installedPackages:
                output[packageGroup]["packages"][packageName] = "OK"
            else:
                output[packageGroup]["packages"][packageName] = "FAIL"
                output[packageGroup]["installedStatus"] = "FAIL"
    return output

def updateStatus(clientName):
    """ Determine whether the client is up-to-date or not """

    output = {}
    clientPath = os.path.join(ROOT_DIR, "client", clientName)
    ymlPath = os.path.join(clientPath, LAST_STATUS)
    output = {"elapsed":0,
              "lastStatus":"",
              "lastMessage":""}
    if not os.path.isfile(ymlPath):
        return output
    y = yaml.loadFile(ymlPath)
    ydata = y.next()
    output["lastMessage"] = ydata["message"]
    output["lastStatus"]  = ydata["severity"]
    checkinTime  = time.mktime(time.strptime(ydata["time"]))
    now = time.time()
    output["elapsed"] = now - checkinTime 
    return output

def clientDetail(clientName, errlog):
    output = {"installation":{},
              "verification":{},
              "status":{},
              "owner":""}
    output["installation"] = installStatus(clientName, errlog)
    output["status"] = updateStatus(clientName)
    return yaml.dump(output)

def get(request, logger, errlog):
    clientName = request.args.get("client")
    if not clientName:
        return string.join(webUtil.getSystemNames(), '\n')
    clientName = clientName[0]
    if clientName.rfind('..') != -1:
        return webUtil.err500(request, errlog)
    return clientDetail(clientName, errlog)

def post(request, logger, errlog):
    """ This would be a good place for clients to put their install-progress or verify"""
    return "NOT IMPLEMENTED"

def put(request, logger, errlog):
    status, config = webUtil.processOptions(request, errlog, ["client", "message"], [])
    if status == FAIL:
        return
    data = request.content.read()
    if config["message"].upper() == "INSTALL":
        installProgressDir = os.path.join(ROOT_DIR, "client", config["client"])
        if not os.path.isdir(installProgressDir):
            try:
                os.mkdir(installProgressDir)
            except OSError:
                return "Unable to create directory\n"
        status = webUtil.verifyAndWriteYaml(data, os.path.join(installProgressDir, "install-progress.yml"))
    else:
        "Good Place to upload verify data"
        return "Unknown status message\n"
    if status == OK:
        return "--- OK\n"
    return "--- FAIL\n"
