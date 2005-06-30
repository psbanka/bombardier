#!/cygdrive/c/Python24/python
import threading, sys, traceback, StringIO, random

from staticData import *
import Logger, Filesystem, Exceptions

# ======================== Worker thread

class ReconcileThread(threading.Thread):
    
    def __init__(self, command, commSocketFromService, commSocketToService,
                 config, server, windows, bombardier):
        threading.Thread.__init__(self)
        self.command = command
        self.config  = config
        self.server  = server
        self.windows = windows
        self.bombardier = bombardier
        self.id      = random.randint(0,10000)
        self.commSocketToService   = commSocketToService
        self.commSocketFromService = commSocketFromService
        Logger.info("========== Starting thread ID %s..." % self.id)
        self.config.automated = False
        
    def run(self):
        status = OK
        filesystem = Filesystem.Filesystem()
        try: 
            self.windows.CoInitialize()
            if self.command == CHECK or self.command == AUTOMATED:
                Logger.info("In Check Thread, calling reconcileSystem")
                status = self.bombardier.reconcileSystem(self.commSocketFromService.testStop)
            elif self.command == VERIFY:
                Logger.info("In Verify Thread, calling verifySystem")
                results = self.bombardier.verifySystem(self.commSocketFromService.testStop)
                status  = OK
                if type(results) != type({}):
                    status = FAIL
                    results = {"OVERALL": "Error in package system"}
                else:
                    if FAIL in results.values():
                        status = FAIL
                self.server.nagiosLog(status, results)

            if status == OK:
                Logger.info("========== ENDING thread ID %s:OK " % (self.id))
                self.server.serverLog("INFO", "Finished installing")
            else:
                ermsg = "========== ENDING thread ID %s:FAIL " % (self.id)
                Logger.info(ermsg)
                self.server.serverLog("INFO", "Failed installing")
        except Exceptions.ServerUnavailable, e:
            self.commSocketToService.sendStop()
            filesystem.clearLock()
            filesystem.updateCurrentStatus(ERROR, "Cannot communicate with server")
            return
        except:
            self.commSocketToService.sendStop()
            filesystem.updateCurrentStatus(ERROR, "Unhandled exception encountered")
            ermsg = 'Exception in thread %s: %s' % (self.id, sys.exc_type) 
            e = StringIO.StringIO()
            traceback.print_exc(file=e)
            e.seek(0)
            data = e.read()
            for line in data.split('\n'):
                ermsg += "\n||>>>%s" % line
            Logger.critical(ermsg)
            self.server.serverLog("CRITICAL", ermsg)
            return
        filesystem.updateCurrentStatus(IDLE, "Finished with installation activities")
        self.commSocketToService.sendStop()

def runWithoutService():
    import Config, Windows, Server, CommSocket
    import Repository, BombardierClass, Exceptions

    Logger.addStdErrLogging()
    filesystem = Filesystem.Filesystem()
    filesystem.clearLock()
    server = Server.Server(filesystem)
    config = Config.Config(filesystem, server, Windows.Windows())
    try:
        config.freshen()
    except Exceptions.ServerUnavailable, e:
        print "Cannot connect to server (%s)" % e
        sys.exit(1)
    windows = Windows.Windows()
    cs1 = CommSocket.CommSocket()
    cs2 = CommSocket.CommSocket()

    repository = Repository.Repository(config, filesystem, server)
    repository.getPackageData()
    bombardier = BombardierClass.Bombardier(repository, config, filesystem, server, windows)
    reconcileThread = ReconcileThread(CHECK, cs1, cs2, config, server, windows, bombardier)
    reconcileThread.run()
        
if __name__ == "__main__":
    runWithoutService()
