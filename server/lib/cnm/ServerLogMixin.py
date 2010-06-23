"Holds a mixin that a Job, Dispatcher, and DispatcherMonitor use"
import ServerLogger
import StringIO, traceback

class ServerLogMixin:
    "Gives a logging object to a class which provides for exception catching"
    def __init__(self):
        "Give ourselves a server log"
        self.server_log = ServerLogger.ServerLogger("Dispatcher",
                                                    use_syslog=True)

    def dump_exception(self, username):
        """Pretty print an exception into the server_log,
         and return traceback info"""
        exc = StringIO.StringIO()
        traceback.print_exc(file=exc)
        exc.seek(0)
        data = exc.read()
        ermsg = ''
        traceback_data = []
        for line in data.split('\n'):
            traceback_data.append(line)
            ermsg = "%% %s" % line
            self.server_log.error(ermsg, username)
        return traceback_data

