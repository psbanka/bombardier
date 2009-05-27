from SystemStateSingleton import SystemState
from bombardier_core.static_data import OK, FAIL, STRING_TO_RETURN_VALUE_MAPPING
import glob, sys, StringIO, yaml
import libUi
import re
import Slash
from bombardier_core.Logger import Logger

DEBUG = False
IGNORE_MODULE_NAMES = 'tester', 'libUi', 'Slash', 'SystemStateSingleton', '_version'

def get_cls(module_name):
    if module_name in IGNORE_MODULE_NAMES:
        return None
    module = __import__(module_name)
    if not hasattr(module, module_name):
        return None
    cls = getattr(module, module_name)
    return cls

def get_tests(cls, module_name):
    tests = {}
    if module_name in IGNORE_MODULE_NAMES:
        return tests
    module = __import__(module_name)
    if not hasattr(module, module_name):
        return tests
    cls = getattr(module, module_name)
    doc_str = cls.__doc__
    if not doc_str:
        return tests
    command = ''
    for line in doc_str.split('\n'):
        if command:
            result = re.compile("(\[(OK|FAIL), \[.*\]\])").findall(line)
            if result:
                yaml_str = result[0][0]
                try:
                    parsed_test = yaml.load(yaml_str)
                    status = STRING_TO_RETURN_VALUE_MAPPING[parsed_test[0]]
                    output_lines = parsed_test[1]
                    tests[command] = [status, output_lines]
                except yaml.parser.ParserError:
                    print "Invalid result: ", line
                    break
            command = ''
        else:
            result = re.compile('bomsh\# (.*)').findall(line)
            if result:
                command = result[0]
    return tests


def run_tests(cls, tests):
    object = cls()
    slash = Slash.Slash([object])
    if not DEBUG:
        print "(suppressing output)"
        output_handle = StringIO.StringIO()
        slash.set_output(output_handle)

    output = {}
    for command in tests:
        expected_result = tests[command]
        expected_status = expected_result[0]
        expected_cmd_output = expected_result[1]
        no_flag, help_flag, tokens, comment = libUi.process_input(command)
        cmd_status, cmd_output = slash.process_command(command.strip())
        if cmd_status != expected_status:
            output[command] = "FAIL"
            print cmd_output
            continue
        if len(cmd_output) != len(expected_cmd_output):
            output[command] = "FAIL"
            print cmd_output, expected_cmd_output
            continue
        for index in range(0,len(cmd_output)):
            received_user_line = cmd_output[index].strip()
            expected_user_line = expected_cmd_output[index]
            if received_user_line != expected_user_line:
                output[command] = "FAIL"
        output[command] = "PASSED"
    return output

if __name__ == "__main__":
    system_state = SystemState()
    system_state.load_config()
    logger = Logger('bombardier_test', 'test.log')
    libUi.login("admin", logger, 'abc123')

    file_names = glob.glob("*.py")
    module_names = [ x.split('.')[0] for x in file_names ]


    for module_name in module_names:
        cls = get_cls(module_name)
        if not cls:
            continue
        tests = get_tests(cls, module_name)
        if not tests:
            continue

        output = run_tests(cls, tests)
        for command in output:
            print "%s:  '%s' (%s)" % (output[command], command, module_name)
