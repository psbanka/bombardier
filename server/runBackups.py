import commands, os, sys, yaml, glob, time
from bombardier.staticData import *
from bombardier.miniUtility import cygpath, addDictionaries
from bcs import BombardierRemoteClient, EXECUTE
import Client
import logging
import logging.handlers

SYSTEM_LOCK_TIMEOUT = 900
BACKUP_FULL = 0
BACKUP_LOG  = 1
RESTORE     = 3

SCRIPT = {BACKUP_FULL: "backupFull", BACKUP_LOG: "backupLog", RESTORE:"restore"}

logger = logging.getLogger("backupServer")
fileHandler = logging.FileHandler("output/backup.log")
formatter = logging.Formatter('%(asctime)s|%(levelname)s|%(message)s|')
fileHandler.setFormatter(formatter)
logger.addHandler(fileHandler)
logger.setLevel(logging.DEBUG)
stdErrHandler = logging.StreamHandler(sys.stderr)
stdErrHandler.setFormatter(formatter)
logger.addHandler(stdErrHandler)

class BombardierBackup(BombardierRemoteClient):
    def __init__(self, hostname, action):
        BombardierRemoteClient.__init__(self, hostname, '')
        self.action = EXECUTE
        self.packageList = ["SqlBackup"]

    def restore(self):
        self.script = SCRIPT[RESTORE]
        return self.process(EXECUTE, self.packageList, self.script)

    def backupFull(self):
        self.script = SCRIPT[BACKUP_FULL]
        return self.process(EXECUTE, self.packageList, self.script)

    def backupLog(self):
        self.script = SCRIPT[BACKUP_LOG]
        return self.process(EXECUTE, self.packageList, self.script)

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
    if setLock("%s-pull-lock" % backupServer) == FAIL:
        logger.error("Unable to proceed with backup. Lock is set.")
        return FAIL
    localReplica  = "%s/%s" % (localDir, backupServer) 
    backupPath = clientConfig["sql"]["servers"][backupServer]["backupPath"]
    ipAddress  = clientConfig["ipAddress"]
    username   = clientConfig["defaultUser"]
    status = OK
    #if 1 == 1:
    try: 
        logger.info("Transferring files from %s..." % (backupServer))
        backupCygPath = cygpath(backupPath)
        r = BombardierBackup(backupServer, BACKUP_FULL)
        status = r.rsync(localReplica, backupCygPath, "PULL")
    #else:
    except:
        logger.error("Exception caught in trying to rsync.")
        status = FAIL
    logger.info("Removing '.bak' and '.log' files..")
    os.system("find %s -name \"*.bak\" -exec rm -f '{}' \\;" % localDir)
    os.system("find %s -name \"*.log\" -exec rm -f '{}' \\;" % localDir)
    clearLock("%s-pull-lock" % backupServer)
    return status

def restore(restoreServer):
    if setLock(restoreServer+"-push-lock") == FAIL:
        logger.info("%s busy. Try again later." % restoreServer)
        return FAIL
    logger.info("Instructing server %s to restore..." % restoreServer)
    r = BombardierBackup(restoreServer, RESTORE)
    cmdstatus = r.restore()
    if cmdstatus == FAIL:
        logger.error( "Restore failed on %s" % ( restoreServer) )
    clearLock(restoreServer+"-push-lock")
    return cmdstatus

def syncAndRestore(restoreServers, backupServer, localNetwork, localDir):
    localReplica = "%s/%s" % (localDir, backupServer) 
    status = OK
    for restoreServer in restoreServers:
        clientConfig   = Client.Client(restoreServer, '')
        clientConfig.downloadClient()
        ipAddress      = clientConfig["ipAddress"]
        username       = clientConfig["defaultUser"]
        restorePath    = clientConfig["sql"]["servers"][restoreServer]["restorePath"]
        restoreNetwork = clientConfig["sql"]["servers"][restoreServer]["network"]
        if restoreNetwork == localNetwork:
            cmdstatus = restore(restoreServer)
            if cmdstatus == FAIL:
                status = FAIL
        else:
            logger.info("Pushing to %s on network %s" % (restoreServer, restoreNetwork))
            #if 1 == 1:
            try: 
                if setLock(restoreServer+"sync-push-lock") == FAIL:
                    logger.info("%s busy. Try again later." % restoreServer)
                    continue
                logger.info("Transferring files to %s..." % (restoreServer))
                if "\\" in restorePath or ':' in restorePath:
                    restorePath = cygpath(restorePath)
                r = BombardierBackup(restoreServer, RESTORE)
                status = r.rsync(localReplica, restorePath, "PUSH")
                if status != OK:
                    logger.error( "failed.")
                    continue
                clearLock(restoreServer+"sync-push-lock")
                cmdstatus = restore(restoreServer)
                if cmdstatus == FAIL:
                    status = FAIL
            #else:
            except:
                logger.error("Exception caught in trying to rsync.")
                clearLock(restoreServer+"-push-lock")
                status = FAIL
                continue
    return status

def runBackup(backupServer, full, debug):
    if setLock("%s-backup-lock" % backupServer) == FAIL:
        logger.error("Unable to proceed with backup. Lock is set.")
        return FAIL
    if full:
        r = BombardierBackup(backupServer, BACKUP_FULL)
        status = r.backupFull()
    else:
        r = BombardierBackup(backupServer, BACKUP_LOG)
        status = r.backupLog()
    status = OK
    if status != OK:
        logger.error( "Backup failed on %s" % ( backupServer ) )
    clearLock("%s-backup-lock" % backupServer)
    return status

def dbReport(restoreServers, backupServer, databases, full, clientConfig):
    report = {}
    if full:
        backupData = yaml.load(open("output/%s-backupFull.yml" % backupServer, 'r').read())
    else:
        backupData = yaml.load(open("output/%s-backupLog.yml" % backupServer, 'r').read())
    startTime = backupData["startTime"]
    report["backupTime"] = startTime
    report["databases"] = {}
    for database in databases:
        report["databases"][database] = {}
        if full:
            report["databases"][database]["stats"] = backupData[database]["stats"]
        for key in ["backup", "compress", "verify", "rrd"]:
            data = backupData.get(database, {}).get(key)
            if data:
                report["databases"][database][key] = data
    status = OK
    report["servers"] = {}
    for restoreServer in restoreServers:
        report["servers"][restoreServer] = {}
        fileName = "output/%s-restore.yml" % restoreServer
        if not os.path.isfile(fileName):
            report["servers"][restoreServer] = "NO-OUTPUT"
            continue
        restoreData = yaml.load(open(fileName, 'r').read())
        role = clientConfig["sql"]["servers"][restoreServer]["role"]
        if role != "secondary" and not full:
            continue
        for database in databases:
            report["servers"][restoreServer][database] = {}
            dbInfo = restoreData.get(database)
            if dbInfo:
                dbTimeCheck = dbInfo.get("timestamp")
                report["servers"][restoreServer][database]["timestamp"] = dbTimeCheck
                if dbTimeCheck != startTime:
                    msg = "BAD TIMESTAMP: %s/%s (should be %s, found %s)"
                    logger.error( msg % (database, restoreServer, startTime, dbTimeCheck))
                    status = FAIL
                    report["servers"][restoreServer][database]["status"] = "FAIL"
                else:
                    logger.info("Correct timestamp: %s/%s: %s" % (database, restoreServer, dbTimeCheck))
                    report["servers"][restoreServer][database]["status"] = "OK"
            else:
                logger.error( "Database %s on %s has no restore history." % (database, restoreServer) )
                report["servers"][restoreServer][database]["timestamp"] = "NONE"
                report["servers"][restoreServer][database]["status"] = "FAIL"
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
    logger.debug("Running this command to post status to susan: %s" % cmd)
    status = os.system(cmd)
    logger.info("Posted status to susan (%s)" % `status`)
    return status

def statusEmail(clientConfig, subject, report):
    from email.MIMEText import MIMEText
    import smtplib
    dest          = clientConfig["sql"]["administrators"]
    status,source = commands.getstatusoutput('hostname')
    mailServer    = clientConfig["cg"]["relay"]

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

class DeadLockException(Exception):
    def __init__(self, filename, elapsedMin):
        Exception.__init__(self)
        self.filename  = filename
        self.elapsedMin = elapsedMin
    def __str__(self):
        return "Lock file %s has been locked for too long (%s)" % (self.filename, `self.elapsedMin`)
    def __repr__(self):
        return "Lock file %s has been locked for too long (%s)" % (self.filename, `self.elapsedMin`)

def setLock(lockFilename, sleepTime=0):
    time.sleep(sleepTime)
    maxTime    = SYSTEM_LOCK_TIMEOUT
    lockPath   = "output/" + lockFilename

    if os.path.isfile(lockPath):
        timestr = open(lockPath).read().strip()
        try:
            elapsedMin = (time.time() - float(timestr)) / 60.0
        except ValueError:
            elapsedMin = maxTime
        logger.warning("Lock file %s is busy (%3.0f min old)" % (lockPath, elapsedMin))
        if elapsedMin > maxTime:
            raise DeadLockException(lockFilename, elapsedMin)
        return FAIL
    open(lockPath, 'w').write(`time.time()`)
    logger.info("Set lock %s" % lockFilename)
    return OK

def clearLock(lockFilename):
    lockPath = "output/" + lockFilename
    if not os.path.isfile(lockPath):
        logger.warning("Lock was not previously set. Could be some tom foolery going on...")
        return OK
    logger.info("Unlocking %s..." % lockFilename)
    try:
        os.unlink(lockPath)
    except:
        logger.error("Could not delete %s. Unable to unlock" % lockPath)
        return FAIL
    return OK


if __name__ == "__main__":
    import optparse
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
    databases = clientConfig["sql"]["databases"]
    servers = clientConfig["sql"]["servers"].keys()
    localNetwork = clientConfig["sql"]["servers"][backupServer]["network"]
    localArchive = clientConfig["sql"]["mgmtBackupDirectory"]
    restoreServers = list(set(servers) - set([backupServer]))
    report = {"backupServer": backupServer, "databases": databases, "restoreServers": restoreServers}
    if options.full:
        report["backupType"] = "FULL"
    else:
        report["backupType"] = "LOG"
    report['status'] = 'OK'
        
    try:
        report['backup']    = runBackup(backupServer, options.full, options.debug)
        report['pullFiles'] = pullFiles(backupServer, clientConfig, localArchive)
        archiveMaint(clientConfig, databases, "%s/%s" % (localArchive, backupServer))
        report['syncAndRestore'] = syncAndRestore(restoreServers, backupServer, localNetwork, localArchive)
        addDictionaries(report, dbReport(restoreServers, backupServer, databases, options.full, clientConfig) )
        wrapup(report, backupServer, clientConfig)
        open("output/backupReport.yml", 'w').write(yaml.dump(report))
    except DeadLockException, e:
        report["EXCEPTION"] = str(e)
        report["status"] = "FAIL"
        wrapup(report, backupServer, clientConfig)
    prettyReport(report)
