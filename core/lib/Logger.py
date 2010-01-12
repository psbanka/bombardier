#!/usr/bin/env python
"""This is a simple class for wrapping functions in the
native python logging module."""

import logging, sys, os, shutil
import logging.handlers
from mini_utility import get_spkg_path
from static_data import OK, FAIL, LOG_MAX_SIZE, LOGS_TO_KEEP, LOG_FILE

FORMAT_STRING = '%(asctime)s|%(levelname)s|%(message)s|'
LOGGER_NAME = "bombardier"

class LoggerClass:

    """This class implements a simple interface that other logging
    type classes also implement. This is the most complete
    implementation. """

    def __init__(self):
        spkg_path = get_spkg_path()
        self.python_logger = None
        self.formatter = None
        self.std_err_handler = None
        self.file_handler = None
        self.log_path = None
        if spkg_path:
            self.log_path = os.path.join(get_spkg_path(), LOG_FILE)
            if self.check_log_size() == FAIL:
                self.cycle_log()
            self.make_logger()

    def check_log_size(self):
        """ We were dealing with Windows, and couldn't use Python's
        fancy rolling logger effectively. We therefore hd to see
        if the log is too big and deal with it periodically. -
         pbanka
        """
        if not os.path.isfile(self.log_path):
            return
        size = os.stat(self.log_path)[6]
        if size > LOG_MAX_SIZE:
            return FAIL
        return OK

    def cycle_log(self):
        "Manually cycle logs"
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
        "use the logging subsystem to obtain a logger"
        self.python_logger = logging.getLogger(LOGGER_NAME)
        try:
            self.file_handler = logging.FileHandler(self.log_path)
        except IOError:
            try:
                self.file_handler = logging.FileHandler(self.log_path)
            except IOError:
                print "Unable to log to %s" % self.log_path
                self.file_handler = logging.StreamHandler(sys.stderr)
        self.formatter = logging.Formatter(FORMAT_STRING)
        self.file_handler.setFormatter(self.formatter)
        self.python_logger.addHandler(self.file_handler)
        self.python_logger.setLevel(logging.DEBUG)

    def info(self, msg):
        "wrap python logging facility"
        self.python_logger.info(msg)

    def debug(self, msg):
        "wrap python logging facility"
        self.python_logger.debug(msg)

    def warning(self, msg):
        "wrap python logging facility"
        self.python_logger.warning(msg)

    def error(self, msg):
        "wrap python logging facility"
        self.python_logger.error(msg)

    def critical(self, msg):
        "wrap python logging facility"
        self.python_logger.critical(msg)

    def add_std_err_logging(self):
        "Get stderr logging in addition to file logging"
        self.std_err_handler = logging.StreamHandler(sys.stderr)
        self.std_err_handler.setFormatter(self.formatter)
        self.python_logger.addHandler(self.std_err_handler)

    def rm_file_logging(self):
        "Sometimes you don't want to log to a file"
        if self.file_handler:
            msg = "Removing logging to %s" % self.log_path
            self.python_logger.info(msg)
            self.python_logger.removeHandler(self.file_handler)
        else:
            msg = "Being told to remove nonexistent file logging handler."
            self.python_logger.warning(msg)

    def rm_std_err_logging(self):
        "Sometimes you're tired of seeing stderr logging"
        if self.std_err_handler:
            self.python_logger.info("Removing logging to standard error")
            self.python_logger.removeHandler(self.std_err_handler)
        else:
            msg = "Being told to remove nonexistent stderr handler."
            self.python_logger.warning(msg)

Logger = LoggerClass()
