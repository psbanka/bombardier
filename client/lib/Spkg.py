import re, os, yaml
from bombardier_core.mini_utility import getSpkgPath
import sys, inspect
import Config
from bombardier_core.Filesystem import Filesystem
from bombardier_core.static_data import OK, FAIL
from bombardier_core.Logger import Logger
import Repository

def double_escape(old_string):
    out_string = ''
    for i in old_string:
        if i == '\\':
            out_string += "\\\\"
        else:
            out_string += i
    return out_string

class SpkgException( Exception ):
    def __init__(self, err_msg=""):
        self.err_msg = str(err_msg)
        err = Exception()
        Exception.__init__(err)
    def __str__(self):
        return self.err_msg
    def __repr__(self):
        return self.err_msg

def get_instance():
    path = os.getcwd()
    spkgPath = getSpkgPath()
    subDir = path.split(spkgPath)[1]
    instance_name = subDir.split(os.path.sep)[1]
    return instance_name

def getConfig():
    instance_name = get_instance()
    filesystem = Filesystem()
    _repository = Repository.Repository(filesystem, instance_name)
    config = Config.Config(filesystem, instance_name)
    return config

def main(cls):
    'A main method that can be used for troubleshooting'
    config = getConfig()
    obj = cls(config)
    action = sys.argv[-1].lower()
    status = OK
    exec("status = obj.%s()" % action)
    sys.exit(status)

def mainV4(cls):
    'A main method that can be used for troubleshooting'
    config = getConfig()
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

def mainV5(cls):
    main(cls)

def dumpReport(report, logger):
    instanceName = get_instance()
    outputPath = os.path.join(getSpkgPath(), instanceName, "output")
    if not os.path.isdir(outputPath):
        os.makedirs(outputPath)
    scriptName = sys.argv[-1].split(".py")[0]
    outputFile = "%s-output.yml" % scriptName
    yaml_string = yaml.dump(report)
    open(os.path.join(outputPath, outputFile), 'w').write(yaml_string)
    for line in yaml_string.split('\n'):
        logger.info("==REPORT==:%s" % line)

class SpkgV5:
    "a type-five abstract package"

    def __init__(self, config):
        '''
        config -- configuarion data object
        logger -- log4py object
        filesystem -- object that interacts with the filesystem
        '''
        self.this_package_name = self._getname()
        self.filesystem    = Filesystem()
        self.stderr        = False
        self.server        = None
        self.port          = None
        self.instance_name = config.get_instance()
        self.report        = {}
        self.logger = Logger
        self.config = config

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
        output_path = os.path.join(getSpkgPath(), self.instance_name, "output")
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
        cwd = os.getcwd()
        path = cwd.split(os.sep)
        return path[-1]

    def _abstract(self):
        self.error("Attempting to call an abstract method")
        return FAIL

    def configure(self):
        return self._abstract()

    def verify(self):
        return self._abstract()

    def install(self):
        return self._abstract()

    def uninstall(self):
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
                    self._error('A variable was found in template "%s" for which there'\
                               " is no configuration value (variable: %s)" % (output_file, variable))
                    config_dict[variable] = 'UNKNOWN_TEMPLATE_VALUE'
                    status = FAIL
            if config_dict == {}:
                output.append(line)
            else:
                output.append(line % config_dict)
        output_data = '\n'.join(output)

        if encoding == None:
            self.filesystem.open(output_file, 'w').write(output_data)
        else:
            self.filesystem.open(output_file, 'wb').write(output_data.encode( encoding ))
        return status

    def _modify_template(self, input_file, output_file, encoding=None,
                        process_escape = False):
        'opens a file ane processes all template values in it'
        if encoding == None:
            input_string = self.filesystem.open(input_file, 'r').read()
        else:
            input_string = unicode( self.filesystem.open(input_file, 'rb').read(), encoding )
        self._info("Template: " + input_file )
        self._info("Created: " + output_file )
        return self._modify_template_string(input_string, output_file, encoding,
                                           process_escape)

class Spkg(SpkgV5):
    "Kept for backwards compatibility"
    def __init__(self, config, filesystem = Filesystem(), logger = None):
        SpkgV5.__init__(self, config)
        self.thisPackagesName = self.this_package_name

    def checkStatus(self, status, err_msg="FAILED"):
        return self._check_status(status, err_msg)

    def installer(self):
        return self.install()

    def uninstaller(self):
        return self.uninstall()

    def modifyTemplateString(self, input_string, output_file, encoding=None,
                             process_escape = False):
        return self._modify_template_string(input_string, output_file, encoding,
                                           process_escape)

    def modifyTemplate(self, input_file, output_file, encoding=None,
                       process_escape = False):
        return self._modify_template(input_file, output_file, encoding,  
                                    process_escape)

    def dump_report(self, report = None):
        return self._dump_report(report)

    def system(self, command, err_msg=""):
        return self._system(command, err_msg)

    def debug(self, string):
        return self._debug(string)

    def info(self, string):
        return self._info(string)

    def warning(self, string):
        return self._warning(string)

    def error(self, string):
        return self._error(string)

    def critical(self, string):
        return self._critical(string)

class SpkgV4(Spkg):
    def __init__(self, config, filesystem = Filesystem(), logger = None):
        Spkg.__init__(self, config, logger, filesystem)
