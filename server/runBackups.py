import commands, os, sys, yaml, glob
from bombardier.staticData import *
from bombardier.miniUtility import cygpath, addDictionaries
import Client

PYTHON = os.path.join(sys.prefix, "bin", "python")

import logging
import logging.handlers

logger = logging.getLogger("backupServer")
fileHandler = logging.FileHandler("output/backup.log")
formatter = logging.Formatter('%(asctime)s|%(levelname)s|%(message)s|')
fileHandler.setFormatter(formatter)
logger.addHandler(fileHandler)
logger.setLevel(logging.DEBUG)
stdErrHandler = logging.StreamHandler(sys.stderr)
stdErrHandler.setFormatter(formatter)
logger.addHandler(stdErrHandler)

def archiveMaint(clientConfig, databases, localArchive):
    daysToKeep = int(clientConfig["sql"]["backupDays"])

    for database in databases:
        archiveDict = {}
        deleteList  = []
        directories = glob.glob("%s/%s/archive*" % ( localArchive, database ) )
        for directory in directories:
            name, db, day, time = directory.split('-')
            if day in archiveDict:
                archiveDict[day].append(time)
            else:
                archiveDict[day] = [time]
        sortedDays = archiveDict.keys()
        sortedDays.sort()
        for day in sortedDays[:-daysToKeep]:
            for time in archiveDict[day]:
                deleteList.append("archive-%s-%s-%s" % (database, day, time) )

        for day in sortedDays:
            times = archiveDict[day]
            if len(times) > 1:
                times.sort()
                for time in times[:-1]:
                    deleteList.append("archive-%s-%s-%s" % (database, day, time) )

        for directory in deleteList:
            logger.info("Removing old archive %s" % directory)
            path = "%s/%s/%s" % (localArchive, database, directory)
            cmd = "rm -rf %s" % path
            logger.debug(cmd)
            os.system(cmd)

def pullFiles(backupServer, clientConfig, localDir):
    localReplica  = "%s/%s" % (localDir, backupServer) 
    backupPath = clientConfig["sql"]["backupPath"]
    ipAddress  = clientConfig["ipAddress"]
    username   = clientConfig["defaultUser"]
    status = OK
    if 1 == 1:
    #try: 
        logger.info("Transferring files from %s..." % (backupServer))
        backupCygPath = cygpath(backupPath)
        cmd = "rsync -a %s@%s:%s/* %s/" % (username, ipAddress, backupCygPath, localReplica)
        logger.debug(cmd)
        status, output = commands.getstatusoutput(cmd)
        if status != OK:
            logger.error( "%s failed." % cmd)
    else:
    #except:
        logger.error("Exception caught in trying to rsync.")
        status = FAIL
    return status

def pushFiles(restoreServers, backupServer, localDir):
    localReplica = "%s/%s" % (localDir, backupServer) 
    
    status = OK
    for restoreServer in restoreServers:
        clientConfig = Client.Client(restoreServer, '')
        clientConfig.downloadClient()
        ipAddress    = clientConfig["ipAddress"]
        username     = clientConfig["defaultUser"]
        restorePath  = clientConfig["sql"]["restorePath"]
        if 1 == 1:
        #try: 
            logger.info("Transferring files to %s..." % (restoreServer))
            restoreCygPath = cygpath(restorePath)
            cmd = "rsync -a %s/* %s@%s:%s" % (localReplica, username, ipAddress, restoreCygPath)
            logger.debug(cmd)
            status, output = commands.getstatusoutput(cmd)
            if status != OK:
                logger.error( "%s failed." % cmd)
        else:
        #except:
            logger.error("Exception caught in trying to rsync.")
            status = FAIL
    return status

def dumpOutput(output):
    for line in output.split('\n'):
        print "   ",line

def runBackup(backupServer, full, debug):
    if full:
        logger.info("Running full backup on %s" % backupServer)
        cmd = "%s bcs.py %s -k SqlBackup -x backupFull" % (PYTHON, backupServer)
    else:
        logger.info("Running log backup on %s" % backupServer)
        cmd = "%s bcs.py %s -k SqlBackup -x backupLog" % (PYTHON, backupServer)
    status, output = commands.getstatusoutput(cmd)
    if debug:
        dumpOutput(output)
    if status == FAIL:
        logger.error( "Backup failed on %s" % backupServer )
        dumpOutput(output)
        sys.exit(FAIL)
    return status

def runRestore(restoreServers, full, debug):
    status = OK
    for server in restoreServers:
        cmd = "%s bcs.py %s -k SqlBackup -x restore" % (PYTHON, server)
        cmdstatus, output = commands.getstatusoutput(cmd)
        logger.info( "Running restore on %s" % server )
        if debug:
            dumpOutput(output)
        if cmdstatus == FAIL:
            status = FAIL
            logger.error( "Restore failed on %s" % server )
            dumpOutput(output)
            sys.exit(FAIL)
    return status

def dbReport(restoreServers, backupServer, databases, full):
    report = {}
    if full:
        backupData = yaml.load(open("output/%s-backupFull.yml" % backupServer, 'r').read())
    else:
        backupData = yaml.load(open("output/%s-backupLog.yml" % backupServer, 'r').read())
    startTime = backupData["startTime"]
    report["backupTime"] = startTime
    for database in databases:
        report[database] = {}
        if full:
            report[database]["stats"] = backupData[database]["stats"]
        for key in ["backup", "compress", "verify", "rrd"]:
            report[database][key] = backupData[database][key]
    for restoreServer in restoreServers:
        report[restoreServer] = {}
        restoreData = yaml.load(open("output/%s-restore.yml" % restoreServer, 'r').read())
        status = OK
        for database in databases:
            report[restoreServer][database] = {}
            dbInfo = restoreData.get(database)
            if dbInfo:
                dbTimeCheck = dbInfo.get("timestamp")
                report[restoreServer][database]["timestamp"] = dbTimeCheck
                if dbTimeCheck != startTime:
                    msg = "Database %s did not restore properly on %s (should be %s, found %s)"
                    logger.error( msg % (database, restoreServer, startTime, dbTimeCheck))
                    status = FAIL
                    report[restoreServer][database]["status"] = "FAIL"
                else:
                    logger.info("Database %s on %s has correct timestamp %s" % (database, restoreServer, dbTimeCheck))
                    report[restoreServer][database]["status"] = "OK"
            else:
                logger.error( "Database %s on %s has no restore history." % (database, restoreServer) )
                report[restoreServer][database]["timestamp"] = "NONE"
                report[restoreServer][database]["status"] = "FAIL"
    if status == OK:
        logger.info("All databases replicated properly." )
        report["status"] = "OK"
    else:
        logger.info("Some databases did not verify." )
        report["status"] = "FAIL"
    return report

def pickOutNastyBits(report):
    message = ''
    for key in report:
        thing = report[key]
        if type(thing) == type("string"):
            if thing == "FAIL":
                message += "%s: FAILED;" % (key)
        if type(thing) == type({}):
            moreNastyBits = pickOutNastyBits(thing)
            if moreNastyBits:
                message += "%s[ %s ];" % (key, moreNastyBits)
    return message

def susanUpdate(clientConfig, report):
    environment = clientConfig["sql"]["environment"]
    susanUrl    = clientConfig["system"]["susanUrl"]
    proxyData   = clientConfig["system"]["proxy"]

    if report["status"] == "OK":
        updateType = 2
    else:
        updateType = 3

    message  = pickOutNastyBits(report)
    if message == '':
        message = "%s backup OK" % report["backupType"]
    base     = 'curl -s -k'
    pem      = '-E susan.pem'
    postVars = '-d record=%s_backup -d type=%s -d comments="%s"' % (environment, updateType, message)
    dest     = '%s/susan/post_event.jsp' % susanUrl
    proxy    = ''
    if proxyData:
        proxy = '-x %s' % proxyData
    cmd = ' '.join([base, pem, postVars, proxy, dest])
    #cmd = ' '.join([base, postVars, proxy, dest])
    logger.debug("Running this command to post status to susan: %s" % cmd)
    status = os.system(cmd)
    logger.info("Posted status to susan (%s)" % `status`)
    return status

def statusEmail(clientConfig, subject, report):
    from email.MIMEText import MIMEText
    import smtplib
    dest          = clientConfig["sql"]["administrators"]
    status,source = commands.getstatusoutput('hostname')
    mailServer    = clientConfig["mail"]["relay"]

    if clientConfig["system"]["debug"]:
        logger.warning("*********SKIPPING EMAIL due to debugging")
        return
    body = yaml.dump(report, default_flow_style=False)
    msg = MIMEText(body)
    msg['Subject'] = subject
    msg['From'] = source
    msg['To'] = ','.join(dest)
    s = smtplib.SMTP()
    #s.set_debuglevel(1)
    logger.info("Sending informational message to %s..." % `dest`)
    s.connect(host=mailServer)
    s.sendmail(source, dest, msg.as_string())
    s.quit()

def wrapup(report, backupServer, clientConfig):
    if report["status"] == "OK":
        if report["backupType"] == "FULL":
            subject = "Full Backup and Restore successful on %s" % backupServer
            statusEmail(clientConfig, subject, report)
    else:
        subject = "Errors encoundered in %s backup on %s" % (report["backupType"].lower(), backupServer)
        statusEmail(clientConfig, subject, report)
    susanUpdate(clientConfig, report)

def prettyReport(report):
    print "\n",yaml.dump(report, default_flow_style=False)

if __name__ == "__main__":
    import optparse
    LOCAL_ARCHIVE = "/var/blob"
    parser = optparse.OptionParser("usage: %prog server-name [options] <backupServer>")
    parser.add_option("-f", "--full", dest="full",
                      action="store_true", default=False,
                      help="Turn on debugging")
    parser.add_option("-d", "--debug", dest="debug",
                      action="store_true", default=False,
                      help="Perform a full backup instead of an incremental")

    (options, args) = parser.parse_args()
    if not args:
        print "ERROR: Must specify the name of a backup server"
        parser.print_help()
        sys.exit(FAIL)
    backupServer = args[0]
    clientConfig = Client.Client(backupServer, '')
    clientConfig.downloadClient()
    databases = []
    for databaseName in clientConfig["sql"]["backupDatabases"]:
        if clientConfig[databaseName]["primary"].lower() == backupServer.lower():
            databases.append(databaseName)
    servers = clientConfig["sql"]["servers"]
    restoreServers = list(set(servers) - set([backupServer]))
    report = {"backupServer": backupServer, "databases": databases, "restoreServers": restoreServers}
    if options.full:
        report["backupType"] = "FULL"
    else:
        report["backupType"] = "LOG"
        
    report['backup']    = runBackup(backupServer, options.full, options.debug)
    report['pullFiles'] = pullFiles(backupServer, clientConfig, LOCAL_ARCHIVE)
    archiveMaint(clientConfig, databases, "%s/%s" % (LOCAL_ARCHIVE, backupServer))
    report['pushFiles'] = pushFiles(restoreServers, backupServer, LOCAL_ARCHIVE)
    report['restore']   = runRestore(restoreServers, options.full, options.debug)
    addDictionaries(report, dbReport(restoreServers, backupServer, databases, options.full) )
    wrapup(report, backupServer, clientConfig)

    prettyReport(report)
