import unittest
import MockObjects
import BackupJob

class Options:
    pass

class BackupJobTest(unittest.TestCase):

    def setUp(self):
        self.logger = MockObjects.MockLogger()
        self.filesystem = MockObjects.MockFilesystem()
        self.backup_server = MockObjects.MockBombardierServer("primarydb")
        self.restore_servers = {"secondary": MockObjects.MockBombardierServer("secondary")}
        self.smtp = MockObjects.MockSmtp()
        self.options = Options()
        self.options.full = True
        self.options.status_path = '.'
        self.options.pull = True
        self.options.push = True
        self.options.backup = True
        self.options.maint = True
        self.options.restore = True
        self.options.restore_users = True
        self.options.clear_locks = False
        self.options.report = True
        self.options.test = False

    def tearDown(self):
        pass

    def simpleBackup(self):
        
        backup_report = {"startTime": "now", "testDB": {"backup": "OK", 
                                                        "verify": "OK", 
                                                        "rrd": "OK",
                                                        "status": "OK",
                                                        "process": "OK",
                                                        "stats": ["abc"]},
                                             "otherDB": 'SKIPPED'}
        restore_testDB = {"status": "OK", "logs-applied":[], "logs-failed": [],
                          "restore": "backupFile.bak.bz2.enc"}
        restore_report = {"lockfile": "OK", "status": "OK", "databases": {"testDB": restore_testDB}}
        self.filesystem.yamlData["./output/primarydb-backupFull.yml"] = backup_report
        self.filesystem.yamlData["./output/secondary-restore.yml"] = restore_report
        self.filesystem.environ["HOSTNAME"] = "mgmt_server"
        self.backup_server.data["network"] = {"administratorEmails": ["foo@manchoo.com"]}

        job = BackupJob.BackupJob(self.options, self.backup_server, self.restore_servers, 
                                  self.smtp, self.filesystem, self.logger)
        report = job.main()
        assert "Subject: Full Backup and Restore successful" in self.smtp.messages[0]["message"]

    def simpleBackupCantRestore(self):
        backup_report = {"startTime": "now", "testDB": {"backup": "OK", 
                                                        "verify": "OK", 
                                                        "rrd": "OK",
                                                        "status": "OK",
                                                        "process": "OK",
                                                        "stats": ["abc"]}}
        self.filesystem.yamlData = {"./output/primarydb-backupFull.yml": backup_report}
        self.filesystem.environ["HOSTNAME"] = "mgmt_server"
        self.backup_server.data["network"] = {"administratorEmails": ["foo@manchoo.com"]}

        job = BackupJob.BackupJob(self.options, self.backup_server, self.restore_servers, 
                                  self.smtp, self.filesystem, self.logger)
        report = job.main()
        print self.smtp.messages
        print report

    def simpleBackupNoAdmins(self):
        options = Options()
        options.full = True
        options.status_path = '.'
        backup_report = {"startTime": "now", "testDB": {"backup": "OK", 
                                                        "verify": "OK", 
                                                        "rrd": "OK",
                                                        "status": "OK",
                                                        "process": "OK",
                                                        "stats": ["abc"]}}
        self.filesystem.yamlData = {"./output/primarydb-backupFull.yml": backup_report}

        job = BackupJob.BackupJob(options, self.backup_server, self.restore_servers, 
                                  self.smtp, self.filesystem, self.logger)
        report = job.main()
        print self.smtp.messages
        print report

if __name__ == "__main__":
    suite = unittest.TestSuite()
    suite.addTest(BackupJobTest("simpleBackup"))
    #suite.addTest(BackupJobTest("simpleBackupCantRestore"))
    suite.addTest(unittest.makeSuite(BackupJobTest))
    unittest.TextTestRunner(verbosity=2).run(suite)
