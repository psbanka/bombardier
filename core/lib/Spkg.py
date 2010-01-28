#!/usr/bin/env python
"Provides a standard superclass for all Bombardier packages"

import re, os, yaml
from mini_utility import get_spkg_path
import sys, inspect
from static_data import OK, FAIL
from Logger import Logger
from Config import Config

def double_escape(old_string):
    'sometimes stuff needs this for certain reasons.'
    out_string = ''
    for i in old_string:
        if i == '\\':
            out_string += "\\\\"
        else:
            out_string += i
    return out_string

class SpkgException( Exception ):
    "raised when a system call is made that fails"
    def __init__(self, err_msg=""):
        self.err_msg = str(err_msg)
        self.cwd = os.getcwd()
        err = Exception()
        Exception.__init__(err)
    def __str__(self):
        return "(in %s) %s " % (self.cwd, self.err_msg)
    def __repr__(self):
        return self.__str__()

def get_instance():
    "provides the name of the machine this is running on"
    path = os.getcwd()
    spkg_path = get_spkg_path()
    sub_dir = path.split(spkg_path)[1]
    instance_name = sub_dir.split(os.path.sep)[1]
    return instance_name

def get_config():
    "obtains the configuration object"
    instance_name = get_instance()
    config = Config(instance_name)
    return config

def main(cls):
    'A main method that can be used for troubleshooting'
    config = get_config()
    obj = cls(config)
    action = sys.argv[-1].lower()
    status = OK
    exec("status = obj.%s()" % action)
    sys.exit(status)

def mainV4(cls):
    'A main method that can be used for troubleshooting'
    config = get_config()
    obj = cls(config, Logger)
    action = sys.argv[-1].lower()
    status = OK
    if action == "install":
        status = obj.installer()
    elif action == "uninstall":
        status = obj.uninstaller()
    elif action == "configure":
        status = obj.configure()
    elif action == "verify":
        status = obj.verify()
    else:
        print "Unknown action %s" % action
        status = FAIL
    sys.exit(status)

def dumpReport(report, logger):
    "Legacy"
    instance_name = get_instance()
    output_path = os.path.join(get_spkg_path(), instance_name, "output")
    if not os.path.isdir(output_path):
        os.makedirs(output_path)
    script_name = sys.argv[-1].split(".py")[0]
    output_file = "%s-output.yml" % script_name
    yaml_string = yaml.dump(report)
    open(os.path.join(output_path, output_file), 'w').write(yaml_string)
    for line in yaml_string.split('\n'):
        logger.info("==REPORT==:%s" % line)

class SpkgV5:
    "a type-five abstract package"

    def __init__(self, config):
        '''
        config -- configuarion data object
        '''
        self.this_package_name = self._getname()
        self.instance_name     = config.get_instance()
        self.report            = {}
        self.logger            = Logger
        self.config            = config

    def _dump_report(self, report = None):
        '''
        command -- the command that was run
        report -- a dictionary of the data that was generated
        For a command that was run on an spkg class, this provides
        data back to the CNM 
        '''
        if type(report) != type({}):
            report = self.report
        command = inspect.stack()[2][3]
        output_path = os.path.join(get_spkg_path(), self.instance_name, "output")
        if not os.path.isdir(output_path):
            os.makedirs(output_path)
        output_file = "%s-output.yml" % command
        yaml_string = yaml.dump(report)
        open(os.path.join(output_path, output_file), 'w').write(yaml_string)
        for line in yaml_string.split('\n'):
            Logger.info("==REPORT==:%s" % line)

    def _check_status(self, status, err_msg="FAILED"):
        'small convenience function'
        if status != OK:
            raise SpkgException(err_msg)

    def _system(self, command, err_msg=""):
        '''run a command and raise if there's an error'''
        err_msg = command
        self._check_status( os.system( command ), err_msg )

    def _debug(self, string):
        'log a debug-level message'
        Logger.debug("[%s]|%s" % (self.this_package_name, string))

    def _info(self, string):
        'log a info-level message'
        Logger.info("[%s]|%s" % (self.this_package_name, string))

    def _warning(self, string):
        'log a warning-level message'
        Logger.warning("[%s]|%s" % (self.this_package_name, string))

    def _error(self, string):
        'log a error-level message'
        Logger.error("[%s]|%s" % (self.this_package_name, string))

    def _critical(self, string):
        'log a critical-level message'
        Logger.critical("[%s]|%s" % (self.this_package_name, string))

    def _getname(self):
        "Provides the name of this package based on directory structure"        
        cwd = os.getcwd()
        path = cwd.split(os.sep)
        return path[-1]

    def _abstract(self):
        "A generic abstract method"
        self._error("Attempting to call an abstract method")
        return FAIL

    def configure(self):
        """*configure* is the canonical method of configuring a package
        This should modify any settings that can be done on the fly"""
        return self._abstract()

    def verify(self):
        """*verify* is the canonical method of validating that a package
        is installed correctly. Doesn't necessary indicate that it is running
        or providing any services"""
        return self._abstract()

    def install(self):
        """*install* is the canonical method for installing a package the
        first time. Bombardier will call *verify* after install whenever
        a package is installed"""
        return self._abstract()

    def uninstall(self):
        """*uninstall* is the canonical method for removing a package.
        the package maintainer should always strive to remove all trace
        of a package so that the system is identical to the state it was
        in before the package was installed."""
        return self._abstract()

    def _modify_template_string(self, input_string, output_file, encoding=None,
                               process_escape = False):
        '''
        Goes through a string and replaces templates using member variables
        defined in this class
        input_string -- a string that may or may not have templated place-
                        holder information in it
        output_file -- where to write out our templated string output
        encoding -- what encoding to use when outputting our data
        process_escape -- sometimes we need to deal with '\' characters
        '''
        var_match = re.compile("\%\((.*?)\)s")
        variables = var_match.findall(input_string)
        status = OK
        output = []
        for line in input_string.split('\n'):
            variables = var_match.findall(line)
            config_dict = {}
            if len(variables) == 0:
                output.append(line)
                continue
            for variable in variables:
                if hasattr(self, variable):
                    config_value = getattr(self, variable)
                    if process_escape:
                        config_dict[variable] = double_escape(config_value)
                    else:
                        config_dict[variable] = config_value
                else:
                    msg = 'A variable was found in template "%s" for which '\
                          'there is no configuration value (variable: %s)'
                    self._error(msg  % (output_file, variable))
                    config_dict[variable] = 'UNKNOWN_TEMPLATE_VALUE'
                    status = FAIL
            if config_dict == {}:
                output.append(line)
            else:
                output.append(line % config_dict)
        output_data = '\n'.join(output)

        if encoding == None:
            open(output_file, 'w').write(output_data)
        else:
            open(output_file, 'wb').write(output_data.encode( encoding ))
        return status

    def _modify_template(self, input_file, output_file, encoding=None,
                        process_escape = False):
        'opens a file ane processes all template values in it'
        if encoding == None:
            input_string = open(input_file, 'r').read()
        else:
            input_string = unicode( open(input_file, 'rb').read(), encoding )
        self._info("Template: " + input_file )
        self._info("Created: " + output_file )
        return self._modify_template_string(input_string, output_file, encoding,
                                           process_escape)

class Spkg(SpkgV5):
    "Kept for backwards compatibility"
    def __init__(self, config, filesystem = None, logger = None):
        SpkgV5.__init__(self, config)
        self.thisPackagesName = self.this_package_name

    def checkStatus(self, status, err_msg="FAILED"):
        "Legacy"
        return self._check_status(status, err_msg)

    def installer(self):
        "Legacy"
        return self.install()

    def uninstaller(self):
        "Legacy"
        return self.uninstall()

    def modifyTemplateString(self, input_string, output_file, encoding=None,
                             process_escape = False):
        "Legacy"
        return self._modify_template_string(input_string, output_file, encoding,
                                           process_escape)

    def modifyTemplate(self, input_file, output_file, encoding=None,
                       process_escape = False):
        "Legacy"
        return self._modify_template(input_file, output_file, encoding,  
                                    process_escape)

    def dump_report(self, report = None):
        "Legacy"
        return self._dump_report(report)

    def system(self, command, err_msg=""):
        "Legacy"
        return self._system(command, err_msg)

    def debug(self, string):
        "Legacy"
        return self._debug(string)

    def info(self, string):
        "Legacy"
        return self._info(string)

    def warning(self, string):
        "Legacy"
        return self._warning(string)

    def error(self, string):
        "Legacy"
        return self._error(string)

    def critical(self, string):
        "Legacy"
        return self._critical(string)

class SpkgV4(Spkg):
    "Legacy"
    def __init__(self, config, filesystem = None, logger = None):
        Spkg.__init__(self, config, logger, filesystem)
