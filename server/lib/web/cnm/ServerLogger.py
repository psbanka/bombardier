import StringIO
import syslog
syslog.openlog("dispatcher", syslog.LOG_PID, syslog.LOG_USER)
syslog.LOG_UPTO(syslog.LOG_INFO)
import logging, sys, os, shutil
import logging.handlers

FORMAT_STRING = '%(asctime)s|%(levelname)s|%(message)s|'

class ServerLogger:
    def __init__(self, name, use_syslog = False):
        self.name = name
        self.python_logger = logging.getLogger(self.name)
        self.python_logger.setLevel(logging.DEBUG)
        self.formatter = logging.Formatter(FORMAT_STRING)
        self.file_handle = None
        self.use_syslog = use_syslog
        self.output_pointer = None

    def add_stringio_handle(self):
        self.file_handle = StringIO.StringIO()
        self.output_pointer = 0
        file_log_handler = logging.StreamHandler(self.file_handle)
        file_log_handler.setFormatter(self.formatter)
        self.python_logger.addHandler(file_log_handler)

    def add_std_err(self):
        std_err_handler = logging.StreamHandler(sys.stderr)
        std_err_handler.setFormatter(self.formatter)
        self.python_logger.addHandler(std_err_handler)

    def get_new_logs(self):
        self.file_handle.seek(self.output_pointer)
        new_output = self.file_handle.read()
        output_list = new_output.split('\n')
        self.output_pointer += len(new_output) - len(output_list[-1])
        return output_list[:-1]

    def get_final_logs(self):
        self.file_handle.seek(self.output_pointer)
        return self.file_handle.read().split('\n')

    def get_complete_log(self):
        self.file_handle.seek(0)
        return self.file_handle.read()

    def log_message(self, level, message, username=None):
        if not username:
            username = self.name
        level_map = {"DEBUG": self.debug,
                     "INFO": self.info,
                     "WARNING": self.warning,
                     "ERROR": self.error
                    }
        if level in level_map:
            level_map[level](message, username)
        
    def debug(self, msg, username=None):
        if not username:
            username = self.name
        self.log(syslog.LOG_DEBUG, msg, username)
        self.python_logger.debug("%s|%s" % (username, msg))

    def info(self, msg, username=None):
        if not username:
            username = self.name
        self.log(syslog.LOG_INFO, msg, username)
        self.python_logger.info("%s|%s" % (username, msg))

    def warning(self, msg, username=None):
        if not username:
            username = self.name
        self.log(syslog.LOG_WARNING, msg, username)
        self.python_logger.warning("%s|%s" % (username, msg))

    def error(self, msg, username=None):
        if not username:
            username = self.name
        self.log(syslog.LOG_ERR, msg, username)
        self.python_logger.error("%s|%s" % (username, msg))

    def log(self, level, msg, username=None):
        if not username:
            username = self.name
        if self.use_syslog:
            syslog.syslog(level, "%-15s|dispatcher: %s" % (username, msg))

def test1():
    a = ServerLogger("test1")
    a.add_std_err()
    a.info("hello", "test_user")

def test2():
    a = ServerLogger("test1")
    a.info("hello2", "test_user")

if __name__ == "__main__":
    test1()
    test2()
