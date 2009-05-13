# Logger.py: This is a simple class for wrapping functions in the
# native python logging module.

# Copyright (C) 2005 Peter Banka

import logging, sys, os, shutil
import logging.handlers
from static_data import OK, FAIL, LOG_MAX_SIZE, LOGS_TO_KEEP

FORMAT_STRING = '%(asctime)s|%(levelname)s|%(message)s|'

class Logger:

    """This class implements a simple interface that other logging
    type classes also implement. This is the most complete
    implementation. """

    def __init__(self, name, log_path):
        self.name = name
        self.log_path = log_path
        if self.check_log_size() == FAIL:
            self.cycle_log()
        self.python_logger = None
        self.formatter = None
        self.std_err_handler = None
        self.file_handler = None
        self.make_logger()

    def check_log_size(self):
        # Because we're dealing with Windows, we can't use Python's
        # fancy rolling logger effectively. We therefore have to see
        # if the log is too big and deal with it periodically. -
        # pbanka
        if not os.path.isfile(self.log_path):
            return
        size = os.stat(self.log_path)[6]
        if size > LOG_MAX_SIZE:
            return FAIL
        return OK

    def cycle_log(self):
        oldest_log_file  = "%s.%s" % (self.log_path, LOGS_TO_KEEP)
        if os.path.isfile(oldest_log_file):
            try:
                os.unlink(oldest_log_file)
            except OSError:
                return
        for i in range(LOGS_TO_KEEP-1, 0, -1):
            source_path = "%s.%s" % (self.log_path, i)
            if os.path.isfile(source_path):
                dest_path = "%s.%s" % (self.log_path, (i+1))
                shutil.copyfile(source_path, dest_path)
                os.unlink(source_path)
        shutil.copyfile(self.log_path, "%s.1" % self.log_path)
        try:
            os.unlink(self.log_path)
        except OSError:
            return

    def make_logger(self):
        self.python_logger = logging.getLogger(self.name)
        try:
            self.file_handler = logging.FileHandler(self.log_path)
        except IOError:
            try:
                self.file_handler = logging.FileHandler(self.log_path)
            except IOError:
                self.file_handler = logging.StreamHandler(sys.stderr)
        self.formatter = logging.Formatter(FORMAT_STRING)
        self.file_handler.setFormatter(self.formatter)
        self.python_logger.addHandler(self.file_handler)
        self.python_logger.setLevel(logging.DEBUG)

    def info(self, msg):
        self.python_logger.info(msg)

    def debug(self, msg):
        self.python_logger.debug(msg)

    def warning(self, msg):
        self.python_logger.warning(msg)

    def error(self, msg):
        self.python_logger.error(msg)

    def critical(self, msg):
        self.python_logger.critical(msg)

    def add_std_err_logging(self):
        self.std_err_handler = logging.StreamHandler(sys.stderr)
        self.std_err_handler.setFormatter(self.formatter)
        self.python_logger.addHandler(self.std_err_handler)

    def rm_file_logging(self):
        if self.file_handler:
            msg = "Removing logging to %s" % self.log_path
            self.python_logger.info(msg)
            self.python_logger.removeHandler(self.file_handler)
        else:
            msg = "Being told to remove nonexistent file logging handler."
            self.python_logger.warning(msg)

    def rm_std_err_logging(self):
        if self.std_err_handler:
            self.python_logger.info("Removing logging to standard error")
            self.python_logger.removeHandler(self.std_err_handler)
        else:
            msg = "Being told to remove nonexistent stderr handler."
            self.python_logger.warning(msg)

