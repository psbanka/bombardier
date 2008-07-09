"""Coordinates backup, syncronization and restore among bombardier client 
systems that each have the SqlBackup package installed"""

import os, sys, yaml, glob, time
from bombardier.staticData import OK, FAIL
from bombardier.miniUtility import cygpath, addDictionaries
from bombardier.Filesystem import Filesystem
from bcs import BombardierRemoteClient, EXECUTE
from getpass import getpass
import Client
import logging
import logging.handlers

## CONSTANTS ####################################################
DATA_PATH = os.getcwd()
SYSTEM_LOCK_TIMEOUT = 900

FULL = 0
LOG  = 1

OK = 0
FAIL = 1
SKIPPED = 2
DEGRADED = 3

R = {OK:"OK", FAIL:"FAIL", SKIPPED:"SKIPPED", FULL:"FULL", LOG:"LOG"}

# OPTIONS
CLEAR_LOCKS     = -1
PERFORM_BACKUP  = 1
RSYNC_PULL      = 2
ARCHIVE_MAINT   = 4
RSYNC_PUSH      = 8
PERFORM_RESTORE = 16
SINGLE_DB       = 32
RESTORE_USERS   = 64
REPORT_OUT      = 128
PASSWORD        = 256
BASIC_OPTIONS   = [PERFORM_BACKUP, RSYNC_PULL, ARCHIVE_MAINT, RSYNC_PUSH, 
                   PERFORM_RESTORE, RESTORE_USERS, REPORT_OUT]
SECURE_OPTIONS  = [PASSWORD, PERFORM_BACKUP, RSYNC_PULL, ARCHIVE_MAINT, 
                   RSYNC_PUSH, PERFORM_RESTORE, RESTORE_USERS, REPORT_OUT]
TEST_OPTIONS    = [CLEAR_LOCKS, RSYNC_PULL, RSYNC_PUSH, PERFORM_RESTORE, 
                   RESTORE_USERS]
OPTIONS         = BASIC_OPTIONS

## JOBINFO CLASS ################################################

def get_my_databases(config):
    "Returns a dictionary of databases this server is configured for"
    my_databases = {}
    apps = config.dictionary("apps")
    for app in apps:
        database_name = apps[app].get("dbName")
        db_options    = apps[app].get("dbOptions", [])
        if database_name and database_name not in my_databases.keys():
            my_databases[database_name] = db_options
    return my_databases

## LOCKING ROUTINES #############################################

class DeadLockException(Exception):
    """If we are unable to perform a lock, and it's been longer than 
    a timeout period, we throw this exception"""
    def __init__(self, file_name, elapsed_min):
        Exception.__init__(self)
        msg = "Lock file %s has been locked for too long (%s)"
        self.msg = msg % (file_name, str(elapsed_min))
    def __str__(self):
        return self.msg
    def __repr__(self):
        return self.msg

## JOB CLASS ####################################################

class BackupJob:
    "Responsible for keeping track of the backup"
    def __init__(self, options, password, backup_server, restore_servers,
                 filesystem, logger):
        self.filesystem         = filesystem
        self.report             = {"job":{}, "warnings": []}
        self.status             = OK
        self.backup_server_name = backup_server_name
        self.logger             = logger
        self.client             = Client.Client(backup_server_name, 
                                                password, DATA_PATH)
        self.client.downloadClient()
        self.db_dict            = get_my_databases(self.client)
        self.primary_prefix     = "sql.servers.%s" % backup_server_name
        self.sanitized_name     = self.backup_server_name.replace('-','')
        self.local_archive      = self.client.string("sql.mgmtBackupDirectory")
        servers                 = self.client.dictionary("sql.servers").keys()
        restore_server_names    = list(set(servers) - set([backup_server_name]))

        self.start_time_str     = time.asctime()
        self.start_time         = time.time()
        self.backup_server = BombardierRemoteClient(self.backup_server_name, 
                                                    password, DATA_PATH)

        self.backup_type = LOG
        if options.full == True:
            self.backup_type = FULL

        if SINGLE_DB in OPTIONS:
            self.db_dict = self.db_dict[self.db_dict.keys()[0]]

        self.restore_servers = {}
        for restore_server_name in restore_server_names:
            restore_client  = Client.Client(restore_server_name, 
                                            password, DATA_PATH)
            restore_client.downloadClient()
            ip_address      = restore_client.string("ipAddress")
            prefix          = "sql.servers.%s" % backup_server_name
            restore_path    = restore_client.string("%s.restorePath" % prefix)
            restore_network = restore_client.string("%s.network" % prefix)
            role            = restore_client.string("%s.role" % prefix)
            restore_object  = BombardierRemoteClient(restore_server_name, 
                                                     password, DATA_PATH)
            restore_server_dict = {"object":restore_object, 
                                   "ipAddress":ip_address, 
                                   "role":role,
                                   "restorePath":restore_path, 
                                   "restoreNetwork":restore_network}
            self.restore_servers[restore_server_name] = restore_server_dict

    def set_lock(self, lock_file_name, sleep_time=0):
        """If we are running a section of code that we don't want to run 
        concurrently, we lock"""
        time.sleep(sleep_time)
        max_time    = SYSTEM_LOCK_TIMEOUT
        lock_path   = os.path.join(DATA_PATH, "output", lock_file_name)

        if self.filesystem.isfile(lock_path):
            timestr = open(lock_path).read().strip()
            try:
                elapsed_min = (time.time() - float(timestr)) / 60.0
            except ValueError:
                elapsed_min = max_time
            msg = "Lock file %s is busy (%3.0f min old)"
            self.logger.warning(msg % (lock_path, elapsed_min))
            if elapsed_min > max_time:
                raise DeadLockException(lock_file_name, elapsed_min)
            return FAIL
        open(lock_path, 'w').write(`time.time()`)
        self.logger.info("Set lock %s" % lock_file_name)
        return OK

    def clear_lock(self, lock_file_name):
        "We're done with the non-concurrent section"
        lock_path = os.path.join("output", lock_file_name)
        if not self.filesystem.isfile(lock_path):
            msg = "Lock was not previously set?"
            self.logger.warning(msg)
            return OK
        self.logger.info("Unlocking %s..." % lock_file_name)
        try:
            self.filesystem.unlink(lock_path)
        except OSError:
            msg = "Could not delete %s. Unable to unlock" % lock_path
            self.logger.error(msg)
            return FAIL
        return OK

    def run_backup(self):
        "Perform the backup iteself using a BombardierRemoteClient object"
        lock_file = "%s-backup-lock" % self.sanitized_name
        if self.set_lock(lock_file) == FAIL:
            return FAIL
        self.report["type"] = R[self.backup_type]
        if self.backup_type == FULL:
            print "#################################### BACKUP FULL "
            status, output = self.backup_server.process(EXECUTE, 
                                                        ["SqlBackup"],
                                                         "backupFull",
                                                         True)
        else:
            print "#################################### BACKUP LOG "
            status, output = self.backup_server.process(EXECUTE, 
                                                        ["SqlBackup"], 
                                                        "backupLog", 
                                                        True)
        if status != OK:
            msg = "Backup failed on %s" % ( self.backup_server_name ) 
            self.logger.error(msg)
            self.report["warnings"].append(msg)
            for line in output:
                self.report["warnings"].append(line)
            self.status = FAIL

        if self.backup_type == FULL:
            backup_file = "%s-backupFull.yml" % self.backup_server_name
        else:
            backup_file = "%s-backupLog.yml" % self.backup_server_name

        backup_report = os.path.join(DATA_PATH, "output", backup_file)
        backup_data = yaml.load(open(backup_report).read())
        start_time = backup_data["startTime"]
        self.report["backup time"] = start_time
        self.report["databases"] = {}
        for database in self.db_dict:
            self.report["databases"][database] = {}
            if self.backup_type == FULL:
                stat_data = backup_data[database]["stats"]
                self.report["databases"][database]["stats"] = stat_data
            for key in ["backup", "compress", "verify", "rrd"]:
                data = backup_data.get(database, {}).get(key)
                if data:
                    self.report["databases"][database][key] = data

        self.clear_lock("%s-backup-lock" % self.sanitized_name)

    def pull_files(self):
        "Pull backup files from the backup server to this managmeent server"
        if self.status != OK:
            self.report["pullFiles"] = R[SKIPPED]
            return
        backup_path = self.client.string("%s.backupPath" % self.primary_prefix)
        lock_file = "%s-pull-lock" % self.sanitized_name
        if self.set_lock(lock_file) == FAIL:
            msg = "Unable to sync backup files from primary server due to lock."
            self.logger.error(SKIPPED)
            self.report["pullFiles"] = R[SKIPPED]
            self.report["warnings"].append(msg)
            self.status = FAIL
            return
        local_replica = os.path.join(self.local_archive, self.sanitized_name)
        if 1 == 1:
        #try: 
            msg = "Transferring files from %s..." % (self.backup_server_name)
            self.logger.info(msg)
            backup_cygpath = cygpath(backup_path)
            status = self.backup_server.rsync(local_replica, 
                                              backup_cygpath+'/*', "PULL")
            self.report["pullFiles"] = R[status]
            if status != OK:
                self.status = DEGRADED
                msg = "Rsync reported an error pulling files form the primary"
                self.report["warnings"].append(msg)
                self.logger.warning(msg)
        else:
        #except:
            msg = "Exception caught in trying to rsync from the primary."
            self.logger.error(msg)
            self.report["warnings"].append(msg)
            self.status = DEGRADED

        self.logger.info("Removing '.bak' and '.log' files..")
        cmd = "find %s -name \"*.%s\" -exec rm -f '{}' \\;"
        self.filesystem.system(cmd % (self.local_archive, "bak"))
        self.filesystem.system(cmd % (self.local_archive, "log"))
        self.clear_lock(lock_file)

    def archive_maint(self):
        "Keep the archive up to date on the management server"
        if self.status == FAIL:
            self.report["archiveMaint"] = R[SKIPPED]
            return
        archive_path = os.path.join(self.local_archive, self.sanitized_name)
        days_to_keep = self.client.integer("sql.backupDays")

        for database in self.db_dict:
            archive_dict = {}
            delete_list  = []
            search_path = os.path.join(archive_path, database, "archive*")
            directories = glob.glob(search_path)
            for directory in directories:
                name, database, day, time_of_day = directory.split('-')
                if day in archive_dict:
                    archive_dict[day].append(time_of_day)
                else:
                    archive_dict[day] = [time_of_day]
            sorted_days = archive_dict.keys()
            sorted_days.sort()
            for day in sorted_days[:-days_to_keep]:
                for time_of_day in archive_dict[day]:
                    rmdir = "archive-%s-%s-%s" % (database, day, time_of_day)
                    delete_list.append(rmdir)

            for day in sorted_days:
                times = archive_dict[day]
                if len(times) > 1:
                    times.sort()
                    for time_of_day in times[:-1]:
                        rmdir = "archive-%s-%s-%s" % (database, day, time_of_day)
                        delete_list.append(rmdir)

            for directory in delete_list:
                self.logger.info("Removing old archive %s" % directory)
                path = "%s/%s/%s" % (archive_path, database, directory)
                cmd = "rm -rf %s" % path
                self.logger.debug(cmd)
                self.fileystem.system(cmd)
        self.report["directories removed on server"] = delete_list

    def push_files(self):
        "Perform data synchronization"
        if self.status == FAIL:
            self.report["job"]["synchronized-to"] = R[SKIPPED]
            return
        self.report["job"]["synchronized-to"] = []
        local_replica  = os.path.join(self.local_archive, self.sanitized_name) 
        local_network  = self.client.string("%s.network" % self.primary_prefix)

        for restore_server_name in self.restore_servers:
            restore_server  = self.restore_servers[restore_server_name]
            restore_path    = restore_server["restorePath"]
            restore_network = restore_server["restoreNetwork"]
            restore_object  = restore_server["object"]

            if restore_network != local_network:
                lock_file = restore_server_name.replace('-','')+"sync-push-lock"
                if self.set_lock(lock_file) == FAIL:
                    msg = "%s busy. Try again later." % restore_server_name
                    self.logger.info(msg)
                    continue
                msg = "Pushing to %s on network %s"
                self.logger.info(msg % (restore_server_name, restore_network))
                if 1 == 1:
                #try: 
                    msg = "Transferring files to %s..."
                    self.logger.info(msg % (restore_server_name))
                    if "\\" in restore_path or ':' in restore_path:
                        restore_path = cygpath(restore_path)
                    status = restore_object.rsync(local_replica+"/*", 
                                                 restore_path, "PUSH")
                    self.clear_lock(lock_file)
                    if status != OK:
                        msg = "Push to secondary %s failed."
                        self.logger.error(msg % restore_server_name)
                        self.report["warnings"].append(msg)
                        self.status = DEGRADED
                        continue
                else:
                #except:
                    msg = "Exception caught in trying to rsync to %s."
                    self.logger.error(msg % restore_server_name)
                    self.report["warnings"].append(msg)
                    self.status = DEGRADED
                    self.clear_lock(lock_file)
                    continue
            self.report["job"]["synchronized-to"].append(restore_server_name)

    def collect_restore_info(self, restore_server_name):
        "Pulls information from the restore report for our final report"
        self.report["restore"][restore_server_name] = {}
        report_file_name = "%s-restore.yml" % restore_server_name
        report_path = os.path.join(DATA_PATH, "output", report_file_name)
        if not self.filesystem.isfile(report_path):
            self.report["restore"][restore_server_name] = "NO-OUTPUT"
            self.status = DEGRADED
            return
        restore_data = yaml.load(open(report_path, 'r').read())

        for database in self.db_dict:
            self.report["restore"][restore_server_name][database] = {}
            db_info = restore_data.get(database)
            if db_info:
                db_time_check = db_info.get("timestamp")
                self.report["restore"][restore_server_name][database]["timestamp"] = db_time_check
                if db_time_check != self.start_time:
                    msg = "BAD TIMESTAMP: %s/%s (should be %s, found %s)"
                    self.logger.error( msg % (database, restore_server_name,
                                         self.start_time, db_time_check))
                    self.report["warnings"].append(msg) 
                    self.status = DEGRADED
                    self.report["servers"][restore_server_name][database]["status"] = "FAIL"
                else:
                    msg = "Correct timestamp: %s/%s: %s"
                    self.logger.info(msg % (database, restore_server_name, db_time_check))
                    self.report["servers"][restore_server_name][database]["status"] = "VERIFIED"
            else:
                msg =  "Database %s on %s has no restore history."
                self.logger.error( msg % (database, restore_server_name) )
                self.report["warnings"].append(msg)
                self.report["servers"][restore_server_name][database]["timestamp"] = "NONE"
                self.report["servers"][restore_server_name][database]["status"] = "FAIL"
                self.status = DEGRADED

    def restore(self):
        "Perform the restore operation on all secondary servers"
        if self.status == FAIL:
            self.report["restore"] = R[SKIPPED]
            return

        self.report["restore"] = {}

        for restore_server_name in self.restore_servers:
            restore_server = self.restore_servers[restore_server_name]
            role           = restore_server["role"]
            restore_object = restore_server["object"]

            if role == "manual":
                self.report["servers"][restore_server_name] = "Automated restore disabled"
                continue
            if role == "rw_secondary" and not self.backup_type == FULL:
                self.report["servers"][restore_server_name] = "Not configured to restore log backups"
                self.logger.info("Not restoring to secondary on log backup")
                continue

            lock_file = restore_server_name.replace('-','')+"-restore-lock"
            if self.set_lock(lock_file) == FAIL:
                msg = "Restore server %s is locked. Cannot restore."
                self.logger.warning(msg % restore_server_name)
                self.report["warnings"].append(msg)
                self.status = DEGRADED
                continue

            msg = "Instructing server %s to restore..."
            self.logger.info(msg % restore_server_name)
            print "#################################### RESTORE"
            cmdstatus, output =  restore_object.process(EXECUTE, 
                                                       ["SqlBackup"], 
                                                       "restore", True)
            self.clear_lock(lock_file)
            if cmdstatus == FAIL:
                msg = "Restore failed on %s" % ( restore_server_name ) 
                self.logger.error(msg)
                self.report["warnings"].append(msg)
                self.status = DEGRADED

            self.collect_restore_info(restore_server_name)
            if RESTORE_USERS in OPTIONS:
                self.online(restore_server_name)

    def online(self, restore_server_name):
        "Bring secondary servers online and activate user accounts"
        if self.status == FAIL:
            self.report["online"] = R[SKIPPED]
            return
        restore_server = self.restore_servers[restore_server_name]
        restore_object = restore_server["object"]

        msg = "Instructing server %s to come online..."
        self.logger.info(msg % restore_server_name)
        status, output = restore_object.process(EXECUTE, ["SqlBackup"], 
                                                "online", True)
        self.report["restore"][restore_server_name]["online"] = R[status]
        msg = "Instructing server %s to set proper user permission..."
        self.logger.info(msg % restore_server_name)
        status, output = restore_object.process(EXECUTE, ["DbAuthorization"],
                                       "setUsers", True)
        self.report["restore"][restore_server_name]["user auth"] = R[status]
        if status != OK:
            msg = "Re-creating users failed on %s." % restore_server_name
            self.logger.error(msg)
            self.report["warnings"].append(msg)
            self.status = DEGRADED
            return

        msg = "Instructing server %s to run restore scripts..."
        self.logger.info(msg % restore_server_name)
        status, output = restore_object.process(EXECUTE, ["SqlBackup"], 
                                       "restoreScripts", True)
        self.report["restore"][restore_server_name]["run restore scripts"] = R[status]
        if status != OK:
            msg = "Running restore scripts on %s failed." % restore_server_name
            self.logger.error(msg)
            self.report["warnings"].append(msg)
            self.status = DEGRADED

    def status_email(self, subject):
        "Prepare an email that will be sent to administrators"
        from email.MIMEText import MIMEText
        import smtplib

        host_name = self.filesystem.environ.get("HOSTNAME")
        if not host_name:
            self.filesystem.system("hostname > hostname.txt")
            host_name = self.filesystem.open("hostname.txt").read().strip()

        administrators = self.client.string("sql.administrators")
        body = yaml.dump(self.report, default_flow_style=False)
        msg = "Sending informational message to %s..."
        self.logger.info(msg % str(administrators))
        msg = MIMEText(body)
        msg['Subject'] = subject
        msg['From'] = host_name
        msg['To'] = ','.join(administrators)
        smtp = smtplib.SMTP()
        #smtp.set_debuglevel(1)
        mail_server = self.client.string("network.mailRelay")
        smtp.connect(host=mail_server)
        smtp.sendmail(host_name, administrators, msg.as_string())
        smtp.quit()

    def wrapup(self):
        "resposible for coordinating all final reporting actions"
        subject = ''
        if self.status == OK:
            if self.backup_type == FULL:
                subject = "Full Backup and Restore successful on %s"
                subject = subject % self.backup_server_name
        elif self.status == DEGRADED:
            subject = "Some errors were encoundered in %s backup on %s"
            subject = subject % (R[self.backup_type], self.backup_server_name)
        elif self.status == FAIL:
            subject = "FAILURE in %s backup on %s"
            subject = subject % (R[self.backup_type], self.backup_server_name)
        if subject:
            self.status_email(subject)

## MAIN #########################################################

def get_logger():
    "produce an object for logging"
    logger = logging.getLogger("backupServer")
    file_handler = logging.FileHandler("output/backup.log")
    formatter = logging.Formatter('%(asctime)s|%(levelname)s|%(message)s|')
    file_handler.setFormatter(formatter)
    logger.addHandler(file_handler)
    logger.setLevel(logging.DEBUG)
    std_err_handler = logging.StreamHandler(sys.stderr)
    std_err_handler.setFormatter(formatter)
    logger.addHandler(std_err_handler)
    return logger

def get_options():
    "Parse the command line and return options"
    import optparse
    usage = "usage: %prog server-name [options] <backup_server>"
    parser = optparse.OptionParser(usage)
    parser.add_option("-f", "--full", dest="full",
                      action="store_true", default=False,
                      help="Turn on debugging")

    password = ''
    if PASSWORD in OPTIONS:
        password = getpass("Please provide root password: ")

    (options, args) = parser.parse_args()
    if not args:
        print "ERROR: Must specify the name of a backup server"
        parser.print_help()
        sys.exit(FAIL)
    backup_server_name = args[0]
    return options, password, backup_server_name

def clear_all_locks(filesystem, logger):
    """Locks are used to ensure that different backup processes don't step on 
    each other. If something breaks, locks can keep things broken. Clearing 
    the locks can be a good way to ensure that the database backup process 
    goes on without stopping for lock files"""
    if CLEAR_LOCKS in OPTIONS:
        for lock_file in filesystem.glob(os.path.join('output', '*lock')):
            status = filesystem.rmScheduledFile(lock_file)
            logger.info("Cleared lock (%s): (%s)" % (lock_file, status))
            if status == FAIL:
                lock_time = filesystem.open(lock_file).read()
                raise DeadLockException(lock_file, lock_time)

def main(filesystem):
    "main method: needs to be broken up"
    logger   = get_logger()
    options, password, backup_server_name = get_options()

    try:
        status = OK
        job = BackupJob(options, password, backup_server_name, 
                        filesystem, logger)
        clear_all_locks(filesystem, logger)
        if PERFORM_BACKUP in OPTIONS:
            job.run_backup()
        if RSYNC_PULL in OPTIONS:
            job.pull_files()
        if ARCHIVE_MAINT in OPTIONS:
            job.archive_maint()
        if RSYNC_PUSH in OPTIONS:
            job.push_files()
        if PERFORM_RESTORE in OPTIONS :
            job.restore()
        if REPORT_OUT in OPTIONS:
            job.wrapup()
    except DeadLockException, dle:
        job.report["EXCEPTION"] = str(dle)
        job.status = FAIL
        job.wrapup()

    print "\n", yaml.dump(job.report, default_flow_style=False)

if __name__ == "__main__":
    filesystem = Filesystem()
    main(filesystem)
