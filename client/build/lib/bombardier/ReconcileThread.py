#!/cygdrive/c/Python24/python
from staticData import *
import threading
import sys
import random

# ======================== Worker thread

class ReconcileThread(threading.Thread):
    
    def __init__(self, command, commSocketFromService, commSocketToService,
                 logger, config, server, windows, bombardier):
        threading.Thread.__init__(self)
        self.command = command
        self.logger  = logger
        self.config  = config
        self.server  = server
        self.windows = windows
        self.bombardier = bombardier
        self.id      = random.randint(0,10000)
        self.commSocketToService   = commSocketToService
        self.commSocketFromService = commSocketFromService
        self.logger.info("========== Starting thread ID %s..." % self.id)
        self.config.automated = False
        
    def run(self):
        status = OK
        #if 1 == 1:
        try: 
            self.windows.CoInitialize()
            if self.command == CHECK or self.command == AUTOMATED:
                self.logger.info("In Check Thread, calling reconcileSystem")
                status = self.bombardier.reconcileSystem(self.commSocketFromService.testStop)
            elif self.command == VERIFY:
                self.logger.info("In Verify Thread, calling verifySystem")
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
                self.logger.info("========== ENDING thread ID %s:OK " % (self.id))
                self.server.serverLog("INFO", "Finished installing")
            else:
                ermsg = "========== ENDING thread ID %s:FAIL " % (self.id)
                self.logger.info(ermsg)
                self.server.serverLog("INFO", "Failed installing")
        #else:
        except:
            ermsg = 'Exception in thread %s: %s : %s' % (self.id, sys.exc_type,
                                                         sys.exc_value)
            self.logger.critical(ermsg)
            self.server.serverLog("CRITICAL", ermsg)
            etype = sys.exc_info()[0]
            evalue = sys.exc_info()[1]
            etb = traceback.extract_tb(sys.exc_info()[2])
            self.logger.critical('Error Type: ' + str(etype))
            self.logger.critical('Error Value: ' + str(evalue))
            self.logger.critical('Traceback: ' + str(etb))
        self.commSocketToService.sendStop()

        
if __name__ == "__main__":

    # This is the cheater way to run bombardier on the command line
    # without using the service. This is useful for debugging an
    # exception that you can't find in the code. - pbanka

    class Logger:
        def info(self, string):
            print "info:",string
        def debug(self, string):
            print "debug:",string
        def warning(self, string):
            print "warning:",string
        def error(self, string):
            print "error:",string
        def critical(self, string):
            print "critical:",string
        def rmFileLogging(self):
            pass

    import Config, Windows, Filesystem, Server, CommSocket
    import Repository, BombardierClass, Exceptions
    logger = Logger()
    filesystem = Filesystem.Filesystem()
    server = Server.Server(filesystem, logger)
    config = Config.Config(logger, fileystem, server, Windows.Windows())
    try:
        config.freshen()
    except Exceptions.ServerUnavailable, e:
        print "Cannot connect to server (%s)" % e
        sys.exit(1)
    windows = Windows.Windows()
    cs1 = CommSocket.CommSocket()
    cs2 = CommSocket.CommSocket()

    repository = Repository.Repository(config, logger, filesystem, server)
    bombardier = BombardierClass.Bombardier(repository, config, logger, filesystem, server, windows)
    reconcileThread = ReconcileThread(CHECK, cs1, cs2, logger, config, server, windows, bombardier)
    reconcileThread.run()
