#!/cygdrive/c/Python24/python.exe

import Logger
logger = Logger.Logger()
logger.addStdErrLogging()
logger.info("hellO")
import installUtils

installUtils.logSomeStuff("hello")
