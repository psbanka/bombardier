import commands, os, sys, yaml, glob, time
from bombardier.staticData import *
from bombardier.miniUtility import cygpath, addDictionaries
from bcs import BombardierRemoteClient, EXECUTE
import Client
import logging
import logging.handlers

## TODOS: 
# 1. Centralize configs
# 2. Implement PARTIAL_DB
# 3. Factor text


####################################################
SYSTEM_LOCK_TIMEOUT = 900

# OPTIONS
CLEAR_LOCKS     = -1
PERFORM_BACKUP  = 1
RSYNC_PULL      = 2
ARCHIVE_MAINT   = 4
RSYNC_PUSH      = 8
PERFORM_RESTORE = 16
PARTIAL_DB      = 32
RESTORE_USERS   = 64
REPORT_OUT      = 132
#OPTIONS = [PERFORM_BACKUP, RSYNC_PULL, ARCHIVE_MAINT, RSYNC_PUSH, PERFORM_RESTORE, RESTORE_USERS, REPORT_OUT]
OPTIONS = [PERFORM_BACKUP, RSYNC_PULL, ARCHIVE_MAINT, RSYNC_PUSH, PERFORM_RESTORE, RESTORE_USERS, REPORT_OUT]
#OPTIONS = [CLEAR_LOCKS, RESTORE_USERS, PERFORM_RESTORE]
####################################################


logger = logging.getLogger("backupServer")
fileHandler = logging.FileHandler("output/backup.log")
formatter = logging.Formatter('%(asctime)s|%(levelname)s|%(message)s|')
fileHandler.setFormatter(formatter)
logger.addHandler(fileHandler)
logger.setLevel(logging.DEBUG)
stdErrHandler = logging.StreamHandler(sys.stderr)
stdErrHandler.setFormatter(formatter)
logger.addHandler(stdErrHandler)


######################
### Locking Components
######################

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

#######################
## Reporting Components
#######################

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
        if role == "manual":
            report["servers"][restoreServer] = "Automated restore disabled"
            continue
        if role != "secondary" and not full:
            report["servers"][restoreServer] = "Skipped for log backup"
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

########################
### Backup Components
########################

class BombardierBackup:
    def __init__(self, backupServerName, restoreServers, fullBackup):
        self.backupServerObject = BombardierRemoteClient(backupServerName, '')
        self.backupServerName   = backupServerName
        self.restoreServers     = restoreServers
        self.fullBackup         = fullBackup

    def runBackup(self):
        if setLock("%s-backup-lock" % self.backupServerName.replace('-','')) == FAIL:
            return FAIL
        status = OK
        if self.fullBackup:
            print "#################################### BACKUP FULL "
            status, output = self.backupServerObject.process(EXECUTE, ["SqlBackup"], "backupFull", True)
        else:
            print "#################################### BACKUP LOG "
            status, output = self.backupServerObject.process(EXECUTE, ["SqlBackup"], "backupLog", True)
        if status != OK:
            logger.error( "Backup failed on %s" % ( backupServerName ) )
        clearLock("%s-backup-lock" % backupServerName.replace('-',''))
        return status

    def sync(self, localNetwork, localDir):
        localReplica  = "%s/%s" % (localDir, self.backupServerName.replace('-', '')) 
        status = OK
        for restoreServerName in self.restoreServers:
            restoreServer  = self.restoreServers[restoreServerName]
            restorePath    = restoreServer["restorePath"]
            restoreNetwork = restoreServer["restoreNetwork"]
            restoreObject  = restoreServer["object"]

            if restoreNetwork != localNetwork:
                if setLock(restoreServerName.replace('-','')+"sync-push-lock") == FAIL:
                    logger.info("%s busy. Try again later." % restoreServerName)
                    continue
                logger.info("Pushing to %s on network %s" % (restoreServerName, restoreNetwork))
                if 1 == 1:
                #try: 
                    logger.info("Transferring files to %s..." % (restoreServerName))
                    if "\\" in restorePath or ':' in restorePath:
                        restorePath = cygpath(restorePath)
                    status = restoreObject.rsync(localReplica, restorePath, "PUSH")
                    clearLock(restoreServerName.replace('-','')+"sync-push-lock")
                    if status != OK:
                        logger.error( "RSYNC failed.")
                        continue
                else:
                #except:
                    logger.error("Exception caught in trying to rsync.")
                    clearLock(restoreServerName.replace('-','')+"-sync-push-lock")
                    status = FAIL
                    continue
        return status

    def restore(self):
        status = OK
        for restoreServerName in self.restoreServers:
            restoreServer  = self.restoreServers[restoreServerName]
            role           = restoreServer["role"]
            restoreObject  = restoreServer["object"]

            if role == "manual": # ^^^ FIXME: RW_SECONDARY
                continue
            if role == "rw_secondary" and not self.fullBackup:
                logger.info("Not restoring to secondary on log backup")
                continue

            if setLock(restoreServerName.replace('-','')+"-restore-lock") == FAIL:
                logger.info("%s busy. Try again later." % restoreServerName)
                status = FAIL
                continue

            logger.info("Instructing server %s to restore..." % restoreServerName)
            print "#################################### RESTORE"
            cmdstatus, output =  restoreObject.process(EXECUTE, ["SqlBackup"], "restore", True)
            clearLock(restoreServerName.replace('-','')+"-restore-lock")

            if cmdstatus == FAIL:
                logger.error( "Restore failed on %s" % ( restoreServerName) )
                status = FAIL
                continue
        return status

    def online(self):
        status = OK
        for restoreServerName in self.restoreServers:
            restoreServer  = self.restoreServers[restoreServerName]
            role           = restoreServer["role"]
            restoreObject  = restoreServer["object"]

            if role == "rw_secondary" and self.fullBackup:
                print "#################################### ONLINE "
                logger.info("Instructing server %s to come online..." % restoreServerName)
                restoreObject.process(EXECUTE, ["SqlBackup"], "online", True)
                # NEED A DIFFERENT ROUTINE
                print "#################################### CLEANUSERS "
                logger.info("Instructing server %s to set proper user permission..." % restoreServerName)
                if restoreObject.process(EXECUTE, ["DbAuthorization"], "recreateUsers", True)[0] == OK:
                    continue
                else:
                    logger.error("Re-creating users failed.")
                status = FAIL
        return status


##############################
### File management components
##############################

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
    if setLock("%s-pull-lock" % backupServer.replace('-','')) == FAIL:
        logger.error("Unable to proceed with backup. Lock is set.")
        return FAIL
    localReplica  = "%s/%s" % (localDir, backupServer.replace('-', '')) 
    backupPath = clientConfig["sql"]["servers"][backupServer]["backupPath"]
    status = OK
    if 1 == 1:
    #try: 
        logger.info("Transferring files from %s..." % (backupServer))
        backupCygPath = cygpath(backupPath)
        r = BombardierRemoteClient(backupServer, '')
        status = r.rsync(localReplica, backupCygPath, "PULL")
    else:
    #except:
        logger.error("Exception caught in trying to rsync.")
        status = FAIL
    logger.info("Removing '.bak' and '.log' files..")
    os.system("find %s -name \"*.bak\" -exec rm -f '{}' \\;" % localDir)
    os.system("find %s -name \"*.log\" -exec rm -f '{}' \\;" % localDir)
    clearLock("%s-pull-lock" % backupServer.replace('-',''))
    return status

#######################
### MAIN
#######################

if __name__ == "__main__":
    import optparse
    parser = optparse.OptionParser("usage: %prog server-name [options] <backupServer>")
    parser.add_option("-f", "--full", dest="full",
                      action="store_true", default=False,
                      help="Turn on debugging")

    (options, args) = parser.parse_args()
    if not args:
        print "ERROR: Must specify the name of a backup server"
        parser.print_help()
        sys.exit(FAIL)
    backupServerName = args[0]

    clientConfig       = Client.Client(backupServerName, '')
    clientConfig.downloadClient()
    databases          = clientConfig["sql"]["databases"]
    servers            = clientConfig["sql"]["servers"].keys()
    localNetwork       = clientConfig["sql"]["servers"][backupServerName]["network"]
    localArchive       = clientConfig["sql"]["mgmtBackupDirectory"]
    restoreServerNames = list(set(servers) - set([backupServerName]))
    restoreServers     = {}
    if PARTIAL_DB in OPTIONS:
        databases = [databases[0]]
        clientConfig["sql"]["databases"] = databases
        logger.info("Restricting databases to %s" % str(databases))
        os.system('mv deploy/client/%s.yml %s.yml.bak' % (backupServerName, backupServerName))
        open("deploy/client/%s.yml" % backupServerName, 'w').write(yaml.dump(clientConfig.data))

    for restoreServerName in restoreServerNames:
        clientConfig   = Client.Client(restoreServerName, '')
        clientConfig.downloadClient()
        ipAddress      = clientConfig["ipAddress"]
        restorePath    = clientConfig["sql"]["servers"][restoreServerName]["restorePath"]
        restoreNetwork = clientConfig["sql"]["servers"][restoreServerName]["network"]
        role           = clientConfig["sql"]["servers"][restoreServerName]["role"]
        restoreObject  = BombardierRemoteClient(restoreServerName, '')
        restoreServers[restoreServerName] = {"object":restoreObject, "ipAddress":ipAddress, "role":role,
                                             "restorePath":restorePath, "restoreNetwork":restoreNetwork}

    report = {"backupServer": backupServerName, "databases": databases, "restoreServers": restoreServerNames}
    if options.full:
        report["backupType"] = "FULL"
    else:
        report["backupType"] = "LOG"
    report['status'] = 'OK'

    if CLEAR_LOCKS in OPTIONS:
        status = os.system('rm -f output/*lock')
        logger.info("Clearing all locks... (%s)" % status)
    
    backup = BombardierBackup(backupServerName, restoreServers, options.full)

    try:
        status = OK
        if PERFORM_BACKUP in OPTIONS:
            status = backup.runBackup()
            report['backup'] = status
        if status != OK:
            sys.exit(0)
        if RSYNC_PULL in OPTIONS:
            report['pull'] = pullFiles(backupServerName, clientConfig, localArchive)
        if ARCHIVE_MAINT in OPTIONS:
            archiveMaint(clientConfig, databases, "%s/%s" % (localArchive, backupServerName.replace('-','')))
        if RSYNC_PUSH in OPTIONS:
            report['push'] = backup.sync(localNetwork, localArchive)
        if PERFORM_RESTORE in OPTIONS :
            report['restore'] = backup.restore()
        if RESTORE_USERS in OPTIONS:
            report['online'] = backup.online()
        if REPORT_OUT in OPTIONS:
            addDictionaries(report, dbReport(restoreServerNames, backupServerName, databases, options.full, clientConfig) )
            wrapup(report, backupServerName, clientConfig)
            open("output/backupReport.yml", 'w').write(yaml.dump(report))
    except DeadLockException, e:
        report["EXCEPTION"] = str(e)
        report["status"] = "FAIL"
        wrapup(report, backupServerName, clientConfig)
    if PARTIAL_DB in OPTIONS:
        logger.info("restoring database configuration...")
        os.system('mv %s.yml.bak deploy/client/%s.yml' % (backupServerName, backupServerName))

    prettyReport(report)
